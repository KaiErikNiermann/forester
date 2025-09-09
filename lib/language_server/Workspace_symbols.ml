(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend

open struct
  module L = Lsp.Types
  module Unit_map = Forester_compiler.Expand.Unit_map
  let (let*) = Option.bind
end

let location_of_range loc =
  let* view = Option.map Range.view loc in
  match view with
  | `End_of_file {source; _}
  | `Range ({source; _}, _) ->
    match source with
    | `String _ | `File "" -> None
    | `File path ->
      let uri = Lsp.Uri.of_path path in
      Option.some @@ L.Location.{range = Lsp_shims.Loc.lsp_range_of_range loc; uri;}

let exports_to_symbols (exports : Tree.exports) =
  let@ path, (data, range) = List.filter_map @~ List.of_seq @@ Trie.to_seq exports in
  let@ location = Option.map @~ location_of_range range in
  match data with
  | Xmlns _ ->
    L.SymbolInformation.create
      ~kind: Namespace
      ~location
      ~name: (Format.asprintf "%a" Resolver.Scope.pp_path path)
      ()
  | Term syn ->
    let kind =
      match (List.hd syn).value with
      | Syn.Text _ -> L.SymbolKind.String
      | Syn.Verbatim _ -> String
      | Syn.Fun (_, _) -> Function
      | Syn.Object _ -> Object
      | Syn.Group (_, _)
      | Syn.Math (_, _)
      | Syn.Link _
      | Syn.Subtree (_, _)
      | Syn.Var _
      | Syn.Sym _
      | Syn.Put (_, _, _)
      | Syn.Default (_, _, _)
      | Syn.Get _
      | Syn.Xml_tag (_, _, _)
      | Syn.TeX_cs _
      | Syn.Unresolved_ident _
      | Syn.Prim _
      | Syn.Patch _
      | Syn.Call (_, _)
      | Syn.Results_of_query
      | Syn.Transclude
      | Syn.Embed_tex
      | Syn.Ref
      | Syn.Title
      | Syn.Parent
      | Syn.Taxon
      | Syn.Meta
      | Syn.Attribution (_, _)
      | Syn.Tag _
      | Syn.Date
      | Syn.Number
      | Syn.Dx_sequent (_, _)
      | Syn.Dx_query (_, _, _)
      | Syn.Dx_prop (_, _)
      | Syn.Dx_var _
      | Syn.Dx_const (_, _)
      | Syn.Dx_execute
      | Syn.Route_asset
      | Syn.Syndicate_current_tree_as_atom_feed
      | Syn.Syndicate_query_as_json_blob
      | Syn.Current_tree ->
        Constant
    in
    L.SymbolInformation.create
      ~kind
      ~location
      ~name: (Format.asprintf "%a" Resolver.Scope.pp_path path)
      ()

let contains_substring_case_insensitive ~pattern text =
  let text = String.lowercase_ascii text
  and pattern = String.lowercase_ascii pattern
  in
  let n = String.length text
  and m = String.length pattern
  in
  let rec search i =
    if i + m > n then false
    else if String.sub text i m = pattern then true
    else search (i + 1)
  in
  if m = 0 then true else search 0

let fuzzy_match ~pattern text =
  let pattern = String.lowercase_ascii pattern in
  let text = String.lowercase_ascii text in
  let n = String.length text
  and m = String.length pattern
  in
  if n = 0 then false
  else
    let rec aux i j =
      if j = m then true (* matched entire pattern *)
      else if i = n then false (* text exhausted first *)
      else if text.[i] = pattern.[j] then
        aux (i + 1) (j + 1) (* match, advance both *)
      else
        aux (i + 1) j (* skip char in text *)
    in
    aux 0 0

(* We no longer show exported functions, as this is really gumming up the editor interfaces. *)
let compute (params : L.WorkspaceSymbolParams.t) : _ =
  Logs.debug (fun m -> m "QUERY: %s" params.query);
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render = Plain_text_client.string_of_content ~forest in
  Option.some @@
    let@ uri, item = List.concat_map @~ List.of_seq @@ State.to_seq forest in
    let@ {frontmatter; _} = List.concat_map @~ Option.to_list (Tree.to_article item) in
    let title = render @@ State.get_expanded_title frontmatter forest in
    let@ () = List.concat_map @~ if fuzzy_match ~pattern: params.query title then [()] else [] in
    let@ file_symbol =
      List.concat_map @~
      Option.to_list @@
      let@ source_path = Option.map @~ State.source_path_of_uri uri forest in
      let lsp_uri = Lsp.Uri.of_string source_path in
      let location =
        L.Location.{
          range = L.Range.{
            end_ = {character = 0; line = 0;};
            start = {character = 0; line = 0;};
          };
          uri = lsp_uri;
        }
      in
      L.SymbolInformation.create ~kind: File ~location ~name: title ()
    in
    [file_symbol]
