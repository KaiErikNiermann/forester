(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler

module T = Types
module P = Pure_html
module X = Xml_forester

module Xmlns = struct
  include Xmlns_effect.Make ()

  let run (k : xmlns_attr list -> 'a) =
    run ~reserved: X.reserved_xmlnss @@ fun () ->
    k X.reserved_xmlnss
end

let uri_to_string ~(config : Config.t) uri =
  match URI.host uri with
  | Some host when URI.scheme uri = Some URI_scheme.scheme ->
    if host = config.host then
      URI.path_string uri
    else
      URI.to_string uri
  | _ -> URI.to_string uri (* used to be not percent-encoded; does it matter? *)

let home_uri ~(config : Config.t) =
  let@ root = Option.bind config.home in
  let base = URI_scheme.base_uri ~host: config.host in
  try
    Option.some @@ URI.resolve ~base @@ URI.of_string_exn root
  with
    | _ -> None

let uri_is_home ~config uri =
  match home_uri ~config with
  | Some home_uri ->
    (* By this point, any URIs should be in normal form. *)
    URI.equal home_uri uri
  | None -> false

let rec map_last f xs =
  match xs with
  | [] -> []
  | [x] -> [f x]
  | x :: xs -> x :: map_last f xs

let route_resource_uri ~suffix (forest : State.t) uri =
  let config = forest.config in
  let host = Option.value ~default: "" @@ URI.host uri in
  let components =
    map_last (fun x -> x ^ suffix) @@
      if uri_is_home ~config uri then ["index"]
      else URI.path_components uri
  in
  let prefix_components =
    if host = config.host then []
    else ["foreign"; host]
  in
  prefix_components @ components

let route (forest : State.t) uri : URI.t =
  let uri' =
    match Forest.find_opt forest.resources uri with
    | Some resource ->
      let suffix =
        match resource with
        | T.Article _ -> ".xml"
        | T.Asset _ -> ""
      in
      let path = route_resource_uri ~suffix forest uri in
      URI.make ~path ()
    | None when URI.scheme uri = Some URI_scheme.scheme ->
      Reporter.emitf Broken_link "Could not route link to resource %a" URI.pp uri;
      uri
    | None -> uri
  in
  URI.resolve ~base: (URI.of_string_exn forest.config.base_url) uri'

module Scope = Algaeff.Reader.Make(struct type t = URI.t option end)
module Loop_detection = Algaeff.Reader.Make(struct type t = URI.Set.t end)

(* It's fine to have a global transclusion cache since URIs fully qualify a tree*)
let transclusion_cache = Hashtbl.create 1000

let get_sorted_articles (forest : State.t) addrs =
  let module C = Types.Comparators(struct
    let string_of_content =
      Plain_text_client.string_of_content
        ~forest: forest.resources
        ~router: (route forest)
  end) in
  addrs
  |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.uri_of_vertex
  |> Seq.filter_map (fun uri -> Forest.get_article uri forest.resources)
  |> List.of_seq
  |> List.sort C.compare_article

let get_expanded_title frontmatter forest =
  let scope = Scope.read () in
  let title = Forest.get_expanded_title ?scope ~flags: T.{empty_when_untitled = true} frontmatter forest in
  T.apply_modifier_to_content Sentence_case title

let render_xml_qname qname =
  let qname = Xmlns.normalise_qname qname in
  match qname.prefix with
  | "" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr (forest : State.t) T.{key; value} =
  let str_value = Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) value in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_xmlns_prefix ({prefix; xmlns}: Forester_xml_names.xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  P.string_attr attr "%s" xmlns

let render_section_flags (dict : T.section_flags) = [
  X.optional_ X.show_heading dict.header_shown;
  X.optional_ X.show_metadata dict.metadata_shown;
  X.optional_ X.hidden_when_empty dict.hidden_when_empty;
  X.optional_ X.expanded dict.expanded;
  X.optional_ X.toc dict.included_in_toc;
  X.optional_ X.numbered dict.numbered
]

let add_seen_uri uri kont =
  let@ () = Loop_detection.scope @@ URI.Set.add uri in
  kont ()

let add_seen_uri_opt uri_opt kont =
  match uri_opt with
  | None -> kont ()
  | Some uri -> add_seen_uri uri kont

let have_seen_uri uri =
  URI.Set.mem uri @@ Loop_detection.read ()

let have_seen_uri_opt uri_opt =
  match uri_opt with
  | None -> false
  | Some uri -> have_seen_uri uri

let rec render_section forest (section : T.content T.section) : P.node =
  let@ _ = Xmlns.run in
  X.tree
    (render_section_flags section.flags)
    [
      render_frontmatter forest section.frontmatter;
      let@ () = Scope.run ~env: section.frontmatter.uri in
      X.mainmatter [] @@
        if have_seen_uri_opt section.frontmatter.uri then
          [X.info [] [P.txt "Transclusion loop detected, rendering stopped."]]
        else
          let@ () = add_seen_uri_opt section.frontmatter.uri in
          render_content forest section.mainmatter
    ]

and render_frontmatter (forest : State.t) (frontmatter : T.content T.frontmatter) : P.node =
  let config = forest.config in
  let result =
    X.frontmatter
      []
      [
        render_attributions forest frontmatter.uri frontmatter.attributions;
        render_dates forest frontmatter.dates;
        X.conditional forest.dev (X.optional (X.source_path [] "%s") frontmatter.source_path);
        X.optional (fun uri -> X.addr [] "%s" @@ uri_to_string ~config uri) frontmatter.uri;
        X.optional (X.route [] "%s") @@ Option.map (Fun.compose URI.to_string (route forest)) frontmatter.uri;
        begin
          let title = get_expanded_title frontmatter forest.resources in
          X.title [X.text_ "%s" @@ Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) title] @@
            render_content forest title
        end;
        begin
          match frontmatter.taxon with
          | None -> X.null []
          | Some taxon ->
            X.taxon [] @@ render_content forest (T.apply_modifier_to_content T.Sentence_case taxon)
        end;
        X.null @@ List.map (render_meta forest) frontmatter.metas
      ]
  in
  result

and render_meta forest (key, body) =
  X.meta [X.name "%s" key] @@
    render_content forest body

and render_content (forest : State.t) (Content content: T.content) : P.node list =
  match content with
  | T.Text txt0 :: T.Text txt1 :: content ->
    render_content forest (Content (T.Text (txt0 ^ txt1) :: content))
  | node :: content ->
    let xs = render_content_node forest node in
    let ys = render_content forest (Content content) in
    xs @ ys
  | [] -> []

and render_content_node (forest : State.t) (node : 'a T.content_node) : P.node list =
  let config = forest.config in
  match node with
  | Text str ->
    [P.txt "%s" str]
  | CDATA str ->
    [P.txt ~raw: true "<![CDATA[%s]]>" str]
  | Uri uri ->
    [P.txt "%s" (URI.relative_path_string ~host: config.host uri)]
  | Route_of_uri uri ->
    [P.txt "%s" (URI.to_string (route forest uri))]
  | Xml_elt elt ->
    let prefixes_to_add, (name, attrs, content) =
      let@ () = Xmlns.within_scope in
      render_xml_qname elt.name,
      List.map (render_xml_attr forest) elt.attrs,
      render_content forest elt.content
    in
    let attrs =
      let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
      attrs @ xmlns_attrs
    in
    [P.std_tag name attrs content]
  | Transclude transclusion ->
    render_transclusion forest transclusion
  | Contextual_number addr ->
    let custom_number =
      let@ resource = Option.bind @@ Forest.find_opt forest.resources addr in
      match resource with
      | T.Article article ->
        article.frontmatter.number
      | T.Asset _ -> None
    in
    begin
      match custom_number with
      | None -> [X.contextual_number [X.addr_ "%s" @@ uri_to_string ~config addr]]
      | Some num -> [P.txt "%s" num]
    end
  | Link link ->
    render_link forest link
  | Results_of_datalog_query q ->
    let article_to_section =
      T.article_to_section
        ~flags: {T.default_section_flags with
          expanded = Some false;
          numbered = Some false;
          included_in_toc = Some false;
          metadata_shown = Some true
        }
    in
    let results = Forest.run_datalog_query forest.graphs q in
    let@ article = List.map @~ get_sorted_articles forest results in
    render_section forest @@ article_to_section article
  | Section section ->
    [render_section forest section]
  | KaTeX (mode, content) ->
    let display =
      match mode with
      | Inline -> "inline"
      | Display -> "block"
    in
    let body = Format.asprintf "%a" T.TeX_like.pp_content content in
    [X.tex [X.display "%s" display] "<![CDATA[%s]]>" body]
  | Artefact resource ->
    [render_artefact forest resource]
  | Datalog_script _ -> []

and render_artefact forest (resource : T.content T.artefact) =
  X.resource
    [X.hash "%s" resource.hash]
    [
      X.resource_content [] @@ render_content forest resource.content;
      render_resource_sources resource.sources
    ]

and render_resource_sources sources =
  X.null @@ List.map render_resource_source sources

and render_resource_source source =
  X.resource_source [X.type_ "%s" source.type_; X.resource_part "%s" source.part] "<![CDATA[%s]]>" source.source

and render_transclusion (forest : State.t) (transclusion : T.transclusion) : P.node list =
  match Hashtbl.find_opt transclusion_cache transclusion with
  | Some nodes -> nodes
  | None ->
    match Forest.get_content_of_transclusion transclusion forest.resources with
    | None ->
      Reporter.fatalf Resource_not_found "Could not find tree %a" URI.pp transclusion.href
    | Some content ->
      let nodes = render_content forest content in
      Hashtbl.add transclusion_cache transclusion nodes;
      nodes

and render_link (forest : State.t) (link : T.content T.link) : P.node list =
  let config = forest.config in
  let article_opt = Forest.get_article link.href forest.resources in
  let attrs =
    match article_opt with
    | None ->
      [
        X.href "%s" @@ URI.to_string @@ route forest link.href;
        X.type_ "external"
      ]
    | Some article ->
      [
        X.optional_ (X.href "%s") @@ Option.map (Fun.compose URI.to_string @@ route forest) article.frontmatter.uri;
        X.title_ "%s" @@
        Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) @@
        get_expanded_title article.frontmatter forest.resources;
        X.optional_ (X.addr_ "%s") @@ Option.map (uri_to_string ~config) article.frontmatter.uri;
        X.type_ "local"
      ]
  in
  [X.link attrs @@ render_content forest link.content]

(* Note: this is not a big bottleneck. *)
and render_attributions (forest : State.t) (scope : URI.t option) (attributions : _ T.attribution list) =
  let all_attributions =
    match scope with
    | None -> attributions
    | Some uri ->
      let indirect_attributions =
        let open Datalog_expr.Notation in
        let articles =
          let x = "X" in
          let positives = [Builtin_relation.has_indirect_contributor @* [const (T.Uri_vertex uri); var x]] in
          let negatives = [] in
          Datalog_expr.{var = x; positives; negatives}
          |> Forest.run_datalog_query forest.graphs
          |> get_sorted_articles forest
        in
        let@ biotree = List.filter_map @~ articles in
        let@ uri = Option.map @~ biotree.frontmatter.uri in
        T.{vertex = T.Uri_vertex uri; role = Contributor}
      in
      attributions @
        let@ attribution = List.filter_map @~ indirect_attributions in
        if List.exists (fun (existing : _ T.attribution) -> Vertex.equal attribution.vertex existing.vertex) attributions then None
        else
          Some attribution
  in
  X.authors [] @@ List.map (render_attribution forest) all_attributions

and render_attribution forest (attrib : _ T.attribution) =
  let tag =
    match attrib.role with
    | Author -> X.author
    | Contributor -> X.contributor
  in
  tag [] @@ render_attribution_vertex forest attrib.vertex

and render_attribution_vertex (forest : State.t) vtx =
  match vtx with
  | T.Uri_vertex href ->
    let content = T.Content [T.Transclude {href; target = Title {empty_when_untitled = false}; modifier = Identity}] in
    render_link forest T.{href; content}
  | T.Content_vertex content ->
    render_content forest content

and render_dates forest dates =
  X.null @@ List.map (render_date forest) dates

and render_date forest (date : Human_datetime.t) =
  let config = forest.config in
  let href_attr =
    let str = Format.asprintf "%a" Human_datetime.pp (Human_datetime.drop_time date) in
    let base = URI_scheme.base_uri ~host: config.host in
    let uri = URI.resolve ~base (URI.of_string_exn str) in
    match Forest.get_article uri forest.resources with
    | None -> X.null_
    | Some _ -> X.href "%s" @@ URI.to_string @@ route forest uri
  in
  X.date
    [href_attr]
    [
      X.year [] "%i" (Human_datetime.year date);
      Human_datetime.month date |> X.optional @@ X.month [] "%i";
      Human_datetime.day date |> X.optional @@ X.day [] "%i"
    ]

let render_article (forest : State.t) (article : T.content T.article) : P.node =
  let@ () = Reporter.tracef "when rendering article %a" Format.(pp_print_option URI.pp) article.frontmatter.uri in
  let config = forest.config in
  let@ () = Loop_detection.run ~env: URI.Set.empty in
  let@ () = Scope.run ~env: article.frontmatter.uri in
  let@ xmlnss = Xmlns.run in
  X.tree
    begin
      List.map render_xmlns_prefix xmlnss @
        [
          X.optional_ X.root @@ Option.map (uri_is_home ~config) article.frontmatter.uri;
          P.string_attr "base-url" "%s" config.base_url
        ]
    end
    [
      render_frontmatter forest article.frontmatter;
      X.mainmatter [] @@
        begin
          let@ () = add_seen_uri_opt article.frontmatter.uri in
          render_content forest article.mainmatter
        end;
      X.backmatter [] @@ render_content forest article.backmatter
    ]

let pp_xml ~forest ?stylesheet fmt (article : _ T.article) =
  Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
  Format.pp_print_newline fmt ();
  begin
    let@ xsl_path = Option.iter @~ stylesheet in
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"/%s\"?>" xsl_path
  end;
  Format.pp_print_newline fmt ();
  P.pp_xml fmt @@ render_article forest article
