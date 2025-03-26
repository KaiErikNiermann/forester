(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler.State.Syntax
module L = Lsp.Types

let compute
    ({textDocument = {uri; _}} as params: L.DidOpenTextDocumentParams.t)
  =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let document =
    Lsp.Text_document.make
      ~position_encoding: `UTF16
      params
  in
  (* Hashtbl.replace forest.documents uri document; *)
  let uri = URI_scheme.lsp_uri_to_uri ~host: forest.config.host uri in
  forest.={uri} <- Document document;
  Diagnostics.compute document
