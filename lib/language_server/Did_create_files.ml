(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open State.Syntax

module L = Lsp.Types

let compute ({files}: L.CreateFilesParams.t) =
  Eio.traceln "recieved DidCreateFiles notification";
  Lsp_state.modify (fun ({forest; _} as lsp_state) ->
    let host = forest.config.host in
    let env = forest.env in
    Eio.traceln "client created %d files" (List.length files);
    begin
      let@ {uri} = List.iter @~ files in
      let lsp_uri = L.DocumentUri.of_string uri in
      let uri = URI_scheme.lsp_uri_to_uri ~host lsp_uri in
      let path = Eio.Path.(env#fs / (L.DocumentUri.to_path lsp_uri)) in
      let doc = Imports.load_tree path in
      forest.={uri} <- Document doc
    end;
    let new_forest = Driver.run_until_done Parse_all forest in
    {lsp_state with forest = new_forest}
  )
