(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open State.Syntax
module L = Lsp.Types

let compute
    ({textDocument = {uri = lsp_uri; _}} as params: L.DidOpenTextDocumentParams.t)
  =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let document =
    Lsp.Text_document.make
      ~position_encoding: `UTF16
      params
  in
  let uri = URI_scheme.lsp_uri_to_uri ~host: forest.config.host lsp_uri in
  forest.={uri} <- Document document;
  Lsp_state.modify (fun ({forest; _} as lsp_state) ->
    let new_forest = Driver.run_until_done (Action.Parse lsp_uri) forest in
    {lsp_state with forest = new_forest}
  );
  Diagnostics.compute document
