(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

module L = Lsp.Types
open State.Syntax

let compute (params : L.DidChangeTextDocumentParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  match params with
  | {textDocument = {uri = lsp_uri; _}; contentChanges} ->
    let uri = URI_scheme.lsp_uri_to_uri ~host: forest.config.host lsp_uri in
    match forest.={uri} with
    | None -> ()
    | Some tree ->
      match Tree.to_doc tree with
      | None -> assert false
      | Some doc ->
        let new_doc = Lsp.Text_document.apply_content_changes doc contentChanges in
        Eio.traceln "After change, doc has content %s" (Lsp.Text_document.text new_doc);
        forest.={uri} <- Document new_doc;
        Lsp_state.modify (fun ({forest; _} as lsp_state) ->
          let new_forest = Driver.run_until_done (Action.Parse lsp_uri) forest in
          {lsp_state with forest = new_forest}
        );
        Diagnostics.compute new_doc
