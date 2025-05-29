(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_compiler

open struct module L = Lsp.Types end

let resolve (params : L.CodeAction.t) = params

let next_addrs ~(forest : State.t) prefix =
  URI_util.next_uri ~prefix ~mode: `Sequential ~forest, URI_util.next_uri ~prefix ~mode: `Random ~forest

let create_tree_edit ~range ~uri addr dir =
  L.WorkspaceEdit.create
    ~documentChanges: [
      `CreateFile
        (
          L.CreateFile.create
            ~uri: (Lsp.Uri.of_path (Format.asprintf "%s/%s.tree" dir addr))
            ()
        );
      `TextDocumentEdit
        (
          L.TextDocumentEdit.create
            ~textDocument: {uri; version = None}
            ~edits: [`TextEdit {newText = Format.asprintf "\\transclude{%s}" addr; range}]
        )
    ]
    ()

let compute (L.CodeActionParams.{range; textDocument = {uri}; _;}) : L.CodeActionResult.t =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let actions =
    let next_sequential, next_random = next_addrs ~forest None in
    match forest.config.trees with
    | [] -> []
    | dir :: _ ->
      let sequential =
        L.CodeAction.create
          ~title: (Format.asprintf "create new tree (sequential address)")
          ~kind: (L.CodeActionKind.Other "new tree")
          ~edit: (create_tree_edit ~range ~uri next_sequential dir)
          ()
      in
      let random =
        L.CodeAction.create
          ~title: (Format.asprintf "create new tree (random address)")
          ~kind: (L.CodeActionKind.Other "new tree")
          ~edit: (create_tree_edit ~range ~uri next_random dir)
          ()
      in
      [`CodeAction sequential; `CodeAction random]
  in
  Some actions
