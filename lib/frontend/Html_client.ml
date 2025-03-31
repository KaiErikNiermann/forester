(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_xml_names
open State.Syntax

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
end

module Xmlns = Xmlns_effect.Make ()
module Scope = Algaeff.Reader.Make(struct type t = URI.t option end)
module Section_depth = Algaeff.Reader.Make(struct type t = int end)
module Loop_detection = Loop_detection_effect.Make ()

let hx attrs children = P.std_tag (Format.sprintf "h%i" @@ min 6 @@ Section_depth.read ()) attrs children

let incr_section_depth k =
  let i = Section_depth.read () in
  Section_depth.run ~env: (i + 1) k

let route uri = URI.to_string uri

let get_expanded_title frontmatter forest =
  let scope = Scope.read () in
  Forest.get_expanded_title ?scope ~flags: T.{empty_when_untitled = true} frontmatter forest

let render_xml_qname qname =
  let qname = Xmlns.normalise_qname qname in
  match qname.prefix with
  | "" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr (forest : State.t) T.{key; value} =
  let str_value = Plain_text_client.string_of_content ~forest: forest ~router: Fun.id value in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_xmlns_prefix ({prefix; xmlns}: Forester_xml_names.xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  P.string_attr attr "%s" xmlns

let rec render_content (forest : State.t) (Content content: T.content) : P.node list =
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
    [P.txt "%s" (URI.to_string uri)]
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
  | Route_of_uri uri ->
    [P.txt "%s" (route uri)]
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
      | None -> [P.txt "%s" @@ URI.relative_path_string ~base: config.url uri]
      | Some num -> [P.txt "%s" num]
    end
  | KaTeX (_, content) ->
    [P.HTML.code [] @@ render_content forest content]
  | Artefact artefact -> render_content forest @@ artefact.content
  | Section section -> render_section forest section
  | Transclude transclusion -> render_transclusion forest transclusion
  | Link link -> render_link forest link
  | Results_of_datalog_query _ -> [] (* TODO: just make a list of links *)
  | Datalog_script _ -> []

and render_link (forest : State.t) (link : T.content T.link) : P.node list = [
  P.HTML.a
    [
      P.HTML.href "%s" (Format.asprintf "%a" URI.pp link.href);
    ] @@
    render_content forest link.content
]

and render_transclusion (forest : State.t) (transclusion : T.transclusion) : P.node list =
  match State.get_content_of_transclusion transclusion forest with
  | None ->
    Reporter.fatal (Resource_not_found transclusion.href)
  | Some content ->
    render_content forest content

and render_section forest (section : T.content T.section) : P.node list =
  let@ () = Scope.run ~env: section.frontmatter.uri in
  let@ () = incr_section_depth in
  [
    P.HTML.section
      []
      [
        begin
          match section.frontmatter.title with
          | None -> P.HTML.null []
          | Some title ->
            P.HTML.header
              []
              [
                hx [] @@ render_content forest title
              ]
        end;
        if Loop_detection.have_seen_uri_opt section.frontmatter.uri then
          P.txt "Transclusion loop detected, rendering stopped."
        else
          let@ () = Loop_detection.add_seen_uri_opt section.frontmatter.uri in
          P.HTML.null @@ render_content forest section.mainmatter
      ]
  ]

let render_article ?(heading_level = 0) (forest : State.t) (article : T.content T.article) : P.node =
  let@ () = Section_depth.run ~env: heading_level in
  let@ () = Scope.run ~env: article.frontmatter.uri in
  let@ () = Loop_detection.run in
  let reserved = [
    {prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}
  ]
  in
  let@ () = Xmlns.run ~reserved in
  P.HTML.article
    (List.map render_xmlns_prefix reserved)
    [
      let@ () = Loop_detection.add_seen_uri_opt article.frontmatter.uri in
      P.HTML.null @@ render_content forest article.mainmatter
    ]
