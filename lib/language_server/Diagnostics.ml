(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* When computing the diagnostics for a specific text document, do we know that
   the emitted diagnostics should be reported to the same URI?
   *)

open Forester_core
open Forester_compiler

open struct module L = Lsp.Types end
open State.Syntax

let compute (document : Lsp.Text_document.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let lsp_uri = Lsp.Text_document.documentUri document in
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url lsp_uri in
  match forest.?{uri} with
  | [] ->
    Eio.traceln "Clearing diagnostics for %s" (Lsp.Uri.to_path lsp_uri);
    Publish.publish lsp_uri []
  | diagnostics ->
    Eio.traceln "publishing %i diagnostics to %s" (List.length diagnostics) (Lsp.Uri.to_path lsp_uri);
    Publish.publish lsp_uri diagnostics
