(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler
open State.Syntax

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
end

module Xmlns = struct
  include Xmlns_effect.Make ()

  let run (k : xmlns_attr list -> 'a) =
    run ~reserved: X.reserved_xmlnss @@ fun () ->
    k X.reserved_xmlnss
end

module In_backmatter = Algaeff.Reader.Make(struct type t = bool end)

let local_path_components (config : Config.t) (uri : URI.t) =
  let host = Option.get @@ URI.host uri in
  let base_host = Option.get @@ URI.host config.url in
  if host = base_host then
    URI.stripped_path_components uri
  else
    "foreign" :: host :: URI.stripped_path_components uri

let local_base_url_string (config : Config.t) =
  let path = URI.path_components config.url in
  String.concat "/" path

let route (forest : State.t) uri : URI.t =
  match forest.={uri} with
  | None -> uri
  | Some tree ->
    match Tree.to_evaluated tree with
    | Some evaluated when evaluated.route_locally ->
      let path = "" :: local_path_components forest.config uri in
      URI.make ~path ()
    | _ -> uri

module Scope = struct
  open struct module E = Algaeff.Reader.Make(struct type t = URI.t option end) end
  let read = E.read

  let run ~(forest : State.t) ~env kont =
    let@ () = E.run ~env in
    let loc_opt =
      let@ uri = Option.bind env in
      let@ path = Option.map @~ State.source_path_of_uri uri forest in
      let position = Range.{source = `File path; offset = 0; start_of_line = 0; line_num = 0} in
      Range.make (position, position)
    in
    let@ () = Reporter.with_loc loc_opt in
    kont ()
end

module Loop_detection = Loop_detection_effect.Make ()

let mainmatter_cache = Hashtbl.create 1000

let render_xml_qname qname =
  let qname = Xmlns.normalise_qname qname in
  match qname.prefix with
  | "" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr (forest : State.t) T.{key; value} =
  let str_value = Plain_text_client.string_of_content ~forest ~router: (route forest) value in
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

let rec render_section forest (section : T.content T.section) : P.node =
  let@ _ = Xmlns.run in
  X.tree
    (render_section_flags section.flags)
    [
      render_frontmatter forest section.frontmatter;
      let@ () = Scope.run ~forest ~env: section.frontmatter.uri in
      X.mainmatter [] @@
        if Loop_detection.have_seen_uri_opt section.frontmatter.uri then
          [X.info [] [P.txt "Transclusion loop detected, rendering stopped."]]
        else
          let@ () = Loop_detection.add_seen_uri_opt section.frontmatter.uri in
          render_mainmatter forest section
    ]

and render_mainmatter forest (section : T.content T.section) =
  match section.frontmatter.uri with
  | None -> render_content forest section.mainmatter
  | Some uri ->
    match Hashtbl.find_opt mainmatter_cache uri with
    | None ->
      let nodes = render_content forest section.mainmatter in
      Hashtbl.add mainmatter_cache uri nodes;
      nodes
    | Some nodes -> nodes

and render_frontmatter (forest : State.t) (frontmatter : T.content T.frontmatter) : P.node =
  let result =
    X.frontmatter
      []
      [
        render_attributions forest frontmatter.uri frontmatter.attributions;
        render_dates forest frontmatter.dates;
        X.conditional forest.dev (X.optional (X.source_path [] "%s") frontmatter.source_path);
        X.optional (fun uri -> X.uri [] "%s" @@ URI.to_string uri) frontmatter.uri;
        X.optional (fun uri -> X.display_uri [] "%s" @@ URI.display_path_string ~base: forest.config.url uri) frontmatter.uri;
        X.optional (X.route [] "%s") @@ Option.map (Fun.compose URI.to_string (route forest)) frontmatter.uri;
        begin
          match frontmatter.title with
          | None -> X.null []
          | Some _ ->
            let title = State.get_expanded_title ?scope: (Scope.read ()) frontmatter forest in
            X.title [X.text_ "%s" @@ Plain_text_client.string_of_content ~forest ~router: (route forest) title] @@
              render_content forest title
        end;
        begin
          match frontmatter.taxon with
          | None -> X.null []
          | Some taxon ->
            X.taxon [] @@ render_content forest taxon
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
  match node with
  | Text str ->
    [P.txt "%s" str]
  | CDATA str ->
    [P.txt ~raw: true "<![CDATA[%s]]>" str]
  | Uri uri ->
    [P.txt "%s" (URI.display_path_string ~base: forest.config.url uri)]
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
  | Contextual_number uri ->
    let custom_number =
      let@ resource = Option.bind @@ forest.@{uri} in
      match resource with
      | T.Article article ->
        article.frontmatter.number
      | _ -> None
    in
    begin
      match custom_number with
      | None ->
        [
          X.contextual_number
            [
              X.uri_ "%s" @@ URI.to_string uri;
              X.display_uri_ "%s" @@ URI.display_path_string ~base: forest.config.url uri
            ]
        ]
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
    let@ article = List.map @~ Forest_util.get_sorted_articles forest results in
    render_section forest @@ article_to_section article
  | Section section ->
    [render_section forest section]
  | KaTeX (mode, content) ->
    let display =
      match mode with
      | Inline -> "inline"
      | Display -> "block"
    in
    let body = Format.asprintf "%a" TeX_like.pp_content content in
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
  match State.get_content_of_transclusion transclusion forest with
  | None ->
    Reporter.fatal (Resource_not_found transclusion.href)
  | Some content ->
    render_content forest content

and render_link (forest : State.t) (link : T.content T.link) : P.node list =
  let article_opt = State.get_article link.href forest in
  let attrs =
    match article_opt with
    | None ->
      begin
        if not @@ In_backmatter.read () then
          match State.suggestion_for_uri link.href forest with
          | Ok -> ()
          | Not_found {suggestion} -> Reporter.emit @@ Broken_link {uri = link.href; suggestion}
      end;
      [
        X.href "%s" @@ URI.to_string @@ route forest link.href;
        X.type_ "external"
      ]
    | Some article ->
      [
        X.href "%s" @@ URI.to_string @@ route forest link.href;
        X.title_ "%s" @@
        Plain_text_client.string_of_content ~forest: forest ~router: (route forest) @@
        State.get_expanded_title ?scope: (Scope.read ()) article.frontmatter forest;
        X.optional_ (X.uri_ "%s") @@ Option.map URI.to_string article.frontmatter.uri;
        X.optional_ (X.display_uri_ "%s") @@ Option.map (URI.display_path_string ~base: forest.config.url) article.frontmatter.uri;
        X.type_ "local"
      ]
  in
  [X.link attrs @@ render_content forest link.content]

and render_attributions (forest : State.t) (scope : URI.t option) (primary_attributions : _ T.attribution list) =
  X.authors [] @@
  List.map (render_attribution forest) @@
  Forest_util.collect_attributions forest scope primary_attributions

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
    let content = T.Content [T.Transclude {href; target = Title {empty_when_untitled = false}}] in
    render_link forest T.{href; content}
  | T.Content_vertex content ->
    render_content forest content

and render_dates forest dates =
  X.null @@ List.map (render_date forest) dates

and render_date forest (date : Human_datetime.t) =
  let config = forest.config in
  let href_attr =
    let str = Format.asprintf "%a" Human_datetime.pp (Human_datetime.drop_time date) in
    let uri = URI_scheme.named_uri ~base: config.url str in
    match State.get_article uri forest with
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
  let before = Unix.gettimeofday () in
  let@ () = fun kont ->
    let result = kont () in
    let after = Unix.gettimeofday () in
    let elapsed = after -. before in
    if elapsed > 0.1 then
      Logs.debug (fun m -> m "[Performance] rendering %a took %f seconds" Format.(pp_print_option URI.pp) article.frontmatter.uri elapsed);
    result
  in
  let config = forest.config in
  let@ () = Loop_detection.run in
  let@ () = Scope.run ~forest ~env: article.frontmatter.uri in
  let@ xmlnss = Xmlns.run in
  let@ () = In_backmatter.run ~env: false in
  X.tree
    begin
      List.map render_xmlns_prefix xmlnss @
        [
          X.optional_ X.root @@
            begin
              let@ uri = Option.map @~ article.frontmatter.uri in
              URI.equal (Config.home_uri config) uri
            end;
          P.string_attr "base-url" "%s" (local_base_url_string config)
        ]
    end
    [
      render_frontmatter forest article.frontmatter;
      X.mainmatter [] @@
        begin
          let@ () = Loop_detection.add_seen_uri_opt article.frontmatter.uri in
          render_mainmatter forest @@ T.article_to_section article
        end;
      X.backmatter [] @@
        let@ () = In_backmatter.run ~env: true in
        render_content forest article.backmatter
    ]

let pp_xml ~(forest : State.t) ?stylesheet fmt (article : _ T.article) =
  Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
  Format.pp_print_newline fmt ();
  begin
    let@ xsl_path = Option.iter @~ stylesheet in
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s%s\"?>" (local_base_url_string forest.config) xsl_path
  end;
  Format.pp_print_newline fmt ();
  P.pp_xml fmt @@ render_article forest article
