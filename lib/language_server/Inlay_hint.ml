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
open State.Syntax

open struct
  module L = Lsp.Types
end

let compute (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let uri = URI_scheme.lsp_uri_to_uri ~base: config.url params.textDocument.uri in
  let@ {nodes; _} = Option.map @~ Option.bind forest.={uri} Tree.to_code in
  let@ (Range.{loc; _} as node) = List.filter_map @~ Analysis.flatten nodes in
  match Option.map Range.view loc with
  | None | Some (`End_of_file _) -> None
  | Some (`Range (_, pos)) ->
    let@ {value = str; _} = Option.bind @@ Analysis.extract_addr node in
    let uri = URI_scheme.named_uri ~base: config.url str in
    let@ {frontmatter; _} = Option.bind @@ State.get_article uri forest in
    let@ title = Option.map @~ frontmatter.title in
    let content = " " ^ Plain_text_client.string_of_content ~forest title in
    L.InlayHint.create ~position: (Lsp_shims.Loc.lsp_pos_of_pos pos) ~label: (`String content) ()
