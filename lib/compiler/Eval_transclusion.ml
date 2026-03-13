(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module T = Types
module Symbol_map = Value.Symbol_map

type located = Value.t Range.located

let get_current_uri ~loc (frontmatter : T.content T.frontmatter) =
  match frontmatter.T.uri with
  | Some uri -> uri
  | None ->
    Reporter.fatal
      ?loc
      Internal_error
      ~extra_remarks: [Asai.Diagnostic.loctext "No uri for tree"]

let get_transclusion_flags ~loc ~dynenv ~extract_bool =
  let get_bool key =
    let@ value = Option.map @~ Symbol_map.find_opt key dynenv in
    extract_bool @@ Range.locate_opt loc value
  in
  let module S = Expand.Builtins.Transclude in
  let open Option_util in
  let flags = T.default_section_flags in
  {flags with
    expanded = override (get_bool S.expanded_sym) flags.expanded;
    header_shown = override (get_bool S.show_heading_sym) flags.header_shown;
    included_in_toc = override (get_bool S.toc_sym) flags.included_in_toc;
    numbered = override (get_bool S.numbered_sym) flags.numbered;
    metadata_shown =
    override (get_bool S.show_metadata_sym) flags.metadata_shown;
  }

let eval_transclude ~loc ~flags ~eval_pop_arg ~extract_uri ~emit_content_node =
  let href_arg = eval_pop_arg ~loc in
  let href =
    match extract_uri href_arg with
    | Ok uri -> uri
    | Error _ ->
      Reporter.fatal
        ?loc
        (Type_error {got = None; expected = [URI]})
        ~extra_remarks:
          [Asai.Diagnostic.loctext "Expected valid URI in transclusion"]
  in
  emit_content_node ~loc @@ T.Transclude {href; target = Full flags}

let eval_subtree
    ~loc
    ~flags
    ~config
    ~(parent_frontmatter : T.content T.frontmatter)
    ~addr_opt
    ~nodes
    ~eval_tree_inner
    ~emit_tree
    ~emit_content_node
  =
  let uri =
    match addr_opt with
    | Some addr -> Some (URI_scheme.named_uri ~base: config.Config.url addr)
    | None -> None
  in
  let subtree = eval_tree_inner ?uri nodes in
  let frontmatter = {subtree.T.frontmatter with
    uri;
    designated_parent = parent_frontmatter.T.uri;
  }
  in
  let subtree = {subtree with T.frontmatter} in
  match uri with
  | Some uri ->
    emit_tree subtree;
    let transclusion = T.{href = uri; target = Full flags} in
    emit_content_node ~loc @@ T.Transclude transclusion
  | None ->
    emit_content_node ~loc @@ T.Section (T.article_to_section ~flags subtree)

let eval_results_of_query ~loc ~eval_pop_arg ~emit_content_node =
  let ({Range.value; loc = arg_loc}: located) = eval_pop_arg ~loc in
  match value with
  | Value.Dx_query query ->
    emit_content_node ~loc @@ T.Results_of_datalog_query query
  | other ->
    Reporter.fatal
      ?loc: arg_loc
      (Type_error {expected = [Dx_query]; got = Some other})

let json_blob_job ~loc ~config ~name query =
  let blob_uri =
    URI_scheme.named_uri ~base: config.Config.url @@ name ^ ".json"
  in
  Range.locate_opt loc @@ Job.Syndicate (Json_blob {blob_uri; query})

let atom_feed_job ~loc ~source_uri =
  let feed_uri =
    let components =
      URI.append_path_component (URI.path_components source_uri) "atom.xml"
    in
    URI.with_path_components components source_uri
  in
  Range.locate_opt loc @@ Job.Syndicate (Atom_feed {source_uri; feed_uri})

let latex_artefact ~loc ~config ~preamble ~body =
  let source =
    let latex = config.Config.latex in
    LaTeX_template.to_string
      ~document_class: latex.document_class
      ~document_class_options: latex.document_class_options
      ~preamble
      ~body
  in
  let hash = Digest.to_hex @@ Digest.string source in
  let job = Job.{hash; source} in
  let uri = Job.uri_for_latex_to_svg_job ~base: config.Config.url job in
  let content =
    T.Content
      [
        T.Xml_elt
          {
            content = T.Content [];
            name = {
              uname = "img";
              prefix = "html";
              xmlns = Some "http://www.w3.org/1999/xhtml";
            };
            attrs = [
              {
                key = {uname = "src"; prefix = ""; xmlns = None};
                value = T.Content [T.Route_of_uri uri];
              };
            ];
          };
      ]
  in
  let sources = [
    T.{
      type_ = "latex";
      part = "preamble";
      source = preamble
    };
    T.{
      type_ = "latex";
      part = "body";
      source = body
    };
  ]
  in
  let artefact = T.{hash; content; sources} in
    (Range.locate_opt loc (Job.LaTeX_to_svg job), artefact)

let route_asset_nodes ?loc ~source_path () =
  [T.Route_of_uri (Asset_router.uri_of_asset ?loc ~source_path ())]

let current_tree_node ~uri = T.Uri uri
