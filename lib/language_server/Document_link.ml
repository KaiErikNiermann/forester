(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_frontend
open Forester_compiler

open struct
  module L = Lsp.Types
  let (let*) = Option.bind
end
open State.Syntax

(* TODO: handle external links as well? *)
let compute (params : L.DocumentLinkParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render = Plain_text_client.string_of_content ~forest in
  let config = forest.config in
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let links =
    let uri = URI_scheme.lsp_uri_to_uri ~base: config.url params.textDocument.uri in
    (* match Imports.resolve_uri_to_code forest uri with *)
    match Option.bind forest.={uri} Tree.to_code with
    | None -> []
    | Some tree ->
      let@ node = List.filter_map @~ tree.nodes in
      match Range.(node.value) with
      | Code.Group (Squares, [{value = Text addr; _}])
      | Code.Group (Parens, [{value = Text addr; _}])
      | Code.Group (Braces, [{value = Text addr; _}]) ->
        (* TODO: Need to analyse syn *)
        let range = Lsp_shims.Loc.lsp_range_of_range node.loc in
        let uri = URI_scheme.named_uri ~base: config.url addr in
        let* target = Option.map Lsp.Uri.of_path @@ URI.Tbl.find_opt forest.resolver uri in
        let* {frontmatter; _} = State.get_article uri forest in
        let* tooltip = Option.map (fun c -> render c) frontmatter.title in
        let link = L.DocumentLink.create ~range ~target ~tooltip () in
        Some link
      | _ -> None
  in
  Some links
