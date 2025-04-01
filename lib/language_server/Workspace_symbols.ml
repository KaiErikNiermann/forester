(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_frontend
open Forester_core
open Forester_compiler

module L = Lsp.Types
module Unit_map = Forester_compiler.Expand.Unit_map

let (let*) = Option.bind

let location_of_range loc =
  match Option.map Range.view loc with
  | None -> None
  | Some`End_of_file {source; _}
  | Some`Range ({source; _}, _) ->
    match source with
    | `String _ -> None
    | `File path ->
      if path = "" then
        None
      else
        let uri = (Lsp.Uri.of_path path) in
        Some
          L.Location.{
            range = Lsp_shims.Loc.lsp_range_of_range loc;
            uri;
          }

let exports_to_symbols (exports : Tree.exports) =
  Trie.to_seq exports
  |> List.of_seq
  |> List.filter_map (fun (path, (data, loc)) ->
      match location_of_range loc with
      | None -> None
      | Some location ->
        match data with
        | Resolver.P.Xmlns _ ->
          Some
            (
              L.SymbolInformation.create
                ~kind: Namespace
                ~location
                ~name: (Format.asprintf "%a" Resolver.Scope.pp_path path)
                ()
            )
        | Resolver.P.Term syn ->
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
          Some
            (
              L.SymbolInformation.create
                ~kind
                ~location
                ~name: (Format.asprintf "%a" Resolver.Scope.pp_path path)
                ()
            )
    )

let compute
    ({query = _; _}: L.WorkspaceSymbolParams.t)
  =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render =
    Plain_text_client.string_of_content
      ~forest
      ~router: Fun.id
  in
  let symbols
    =
    forest
    |> State.to_seq
    |> List.of_seq
    |> List.concat_map
        (fun (_, item) ->
          let title =
            match Tree.to_article item with
            | Some {frontmatter; _} ->
              render (State.get_expanded_title frontmatter forest)
            | None -> "untitled"
          in
          let units =
            List.concat @@
            Option.to_list @@
            Option.map exports_to_symbols (Tree.get_units item)
          in
          let lsp_uri = Option.map Lsp.Text_document.documentUri (Tree.to_doc item) in
          let file_symbol =
            Option.to_list @@
              match lsp_uri with
              | None -> None
              | Some uri ->
                let location =
                  L.Location.{
                    range = L.Range.{
                      end_ = {character = 0; line = 0;};
                      start = {character = 0; line = 0;};
                    };
                    uri;
                  }
                in
                Some (L.SymbolInformation.create ~kind: File ~location ~name: title ())
          in
          units @ file_symbol
        )
  in
  Some symbols
