(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_xml_names

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
end

module Xmlns = Xmlns_effect.Make ()
module Scope = Algaeff.Reader.Make(struct type t = URI.t option end)

let route (forest : State.t) uri = failwith ""

let uri_to_string ~(config : Config.t) uri =
  match URI.host uri with
  | Some host when URI.scheme uri = Some URI_scheme.scheme ->
    if host = config.host then
      URI.path_string uri
    else
      URI.to_string uri
  | _ -> URI.to_string uri (* used to be not percent-encoded; does it matter? *)

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
    [P.txt "%s" (URI.relative_path_string ~host: config.host uri)]
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
    [P.txt "%s" (route forest uri)]
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
      | None -> [P.txt "%s" @@ uri_to_string ~config addr]
      | Some num -> [P.txt "%s" num]
    end
  | _ -> failwith ""

and render_link (forest : State.t) (link : T.content T.link) : P.node list = [
  P.HTML.a
    [
      P.HTML.href "%s" (Format.asprintf "%a" URI.pp link.href);
    ] @@
    render_content forest link.content
] *)
