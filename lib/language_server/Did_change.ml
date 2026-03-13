(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct module L = Lsp.Types end
open State.Syntax

let compute (params : L.DidChangeTextDocumentParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let lsp_uri = params.textDocument.uri in
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url lsp_uri in
  let@ tree = Option.iter @~ forest.={uri} in
  match Tree.to_doc tree with
  | None ->
    Logs.debug (fun m -> m "Did_change.compute fatal error, could not find tree with uri %a from LSP uri %s" URI.pp uri (Lsp.Uri.to_string lsp_uri));
    assert false
  | Some doc ->
    let new_doc = Lsp.Text_document.apply_content_changes doc params.contentChanges in
    forest.={uri} <- Document new_doc;
    Lsp_state.modify (fun ({forest; _} as lsp_state) ->
      let new_forest = Driver.run_until_done (Action.Parse lsp_uri) forest in
        {lsp_state with forest = new_forest}
    );
    Diagnostics.compute new_doc
