(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler

open struct
  module T = Types
end

open Pure_html
open HTML
module Xmlns = Xmlns_effect.Make ()

type query = {
  query : (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
[@@deriving repr]

type render_callbacks = {
  render_section : State.t -> T.content T.section -> node;
  render_contextual_number : T.content T.section -> node;
}

let local_path_components (uri : URI.t) =
  let host =
    match URI.host uri with Some host -> host | None -> assert false
  in
  host :: URI.path_components uri

let route (forest : State.t) uri : URI.t =
  let open State.Syntax in
  match forest.={uri} with
  | None -> uri
  | Some _ ->
      let path = "" :: local_path_components uri in
      URI.make ~path ()

let title_flags_to_http_header (flags : T.title_flags) =
  match flags with
  | { empty_when_untitled } ->
      `Assoc
        [
          ("Empty-When-Untitled", `String (Bool.to_string empty_when_untitled));
        ]

let section_flags_to_http_header (flags : T.section_flags) =
  match flags with
  | {
   hidden_when_empty;
   included_in_toc;
   header_shown;
   metadata_shown;
   numbered;
   expanded;
  } ->
      let to_header label = function
        | Some value -> Some (label, `String (Bool.to_string value))
        | None -> None
      in
      let headers =
        [
          to_header "Hidden-When-Empty" hidden_when_empty;
          to_header "Included-In-Toc" included_in_toc;
          to_header "Header-Shown" header_shown;
          to_header "Metadata-Shown" metadata_shown;
          to_header "Numbered" numbered;
          to_header "Expanded" expanded;
        ]
      in
      `Assoc (List.filter_map Fun.id headers)

let content_target_to_http_header (target : T.content_target) =
  match target with
  | T.Full flags ->
      let (`Assoc flags) = section_flags_to_http_header flags in
      `Assoc (("Full", `String "true") :: flags)
  | T.Mainmatter -> `Assoc [ ("Mainmatter", `String "true") ]
  | T.Title flags ->
      let (`Assoc flags) = title_flags_to_http_header flags in
      `Assoc (("Title", `String "true") :: flags)
  | T.Taxon -> `Assoc [ ("Taxon", `String "true") ]

let render_xml_qname = function
  | { prefix = ""; uname; _ } -> uname
  | { prefix; uname; _ } -> Format.sprintf "%s:%s" prefix uname

let render_xml_attr : T.content T.xml_attr -> _ =
 fun T.{ key; value = _ } -> string_attr (render_xml_qname key) "todo"

let render_xmlns_prefix ({ prefix; xmlns } : xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  string_attr attr "%s" xmlns

let render_transclusion transclusion =
  match transclusion with
  | T.{ href; target } ->
      let headers =
        Yojson.Safe.to_string @@ content_target_to_http_header target
      in
      [
        span
          [
            Hx.trigger "load";
            Hx.get "/trees%s" (URI.path_string href);
            Hx.target "this";
            Hx.swap "outerHTML";
            Hx.headers "%s" headers;
          ]
          [ txt "transclusion: %s" (Format.asprintf "%a" URI.pp href) ];
      ]

let rec render_content ~callbacks (forest : State.t)
    (T.Content content : T.content) : node list =
  List.concat_map (render_content_node ~callbacks forest) content

and render_content_node ~callbacks (forest : State.t) (node : 'a T.content_node)
    : node list =
  match node with
  | Text str -> [ txt "%s" str ]
  | CDATA str -> [ txt ~raw:true "<![CDATA[%s]]>" str ]
  | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        let@ () = Xmlns.within_scope in
        ( render_xml_qname elt.name,
          List.map render_xml_attr elt.attrs,
          render_content ~callbacks forest elt.content )
      in
      let attrs = attrs @ List.map render_xmlns_prefix prefixes_to_add in
      [ std_tag name attrs content ]
  | Transclude transclusion -> render_transclusion transclusion
  | Contextual_number addr -> begin
      match State.get_article addr forest with
      | Some article ->
          [ callbacks.render_contextual_number (T.article_to_section article) ]
      | None -> []
    end
  | Link link -> render_link ~callbacks forest link
  | Section section -> [ callbacks.render_section forest section ]
  | KaTeX (mode, content) ->
      let body = Plain_text_client.string_of_content ~forest content in
      begin match mode with
      | Inline -> [ span [ class_ "math" ] [ txt ~raw:true "%s" body ] ]
      | Display -> [ div [ class_ "math" ] [ txt ~raw:true "%s" body ] ]
      end
  | Results_of_datalog_query q ->
      [
        span
          [
            Hx.get "/query";
            Hx.trigger "load";
            Hx.swap "outerHTML";
            Hx.target "this";
            Hx.vals "%s"
              Repr.(to_json_string ~minify:true query_t { query = q });
          ]
          [];
      ]
  | T.Datalog_script _ -> []
  | T.Artefact _ | T.Uri _ | T.Route_of_uri _ -> [ txt "todo" ]

and render_link ~callbacks (forest : State.t) (link : T.content T.link) :
    node list =
  let attrs =
    match State.get_article link.href forest with
    | None -> [ href "%s" (Format.asprintf "%a" URI.pp link.href) ]
    | Some article -> begin
        match article.frontmatter.uri with
        | Some _uri ->
            [
              title_ "%s" @@ Option.value ~default:""
              @@ Option.map
                   (Plain_text_client.string_of_content ~forest
                      ~router:(Legacy_xml_client.route forest))
                   article.frontmatter.title;
              href "/trees%s" (Format.asprintf "%s" (URI.path_string link.href));
              Hx.target "#tree-container";
              Hx.swap "innerHTML";
            ]
        | None -> [ HTML.null_ ]
      end
  in
  [
    span
      [ class_ "link local" ]
      [ a attrs (render_content ~callbacks forest link.content) ];
  ]
