(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend
open State.Syntax

open struct
  module L = Lsp.Types
end

module Sources = Completion_sources

let pp_completion = Sources.pp_completion

let compute ({ position; textDocument = { uri }; _ } : L.CompletionParams.t) =
  Logs.debug (fun m ->
      m "when computing completions for %s" (Lsp.Uri.to_string uri));
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = forest.config in
  let base = config.url in
  let uri = URI_scheme.lsp_uri_to_uri ~base uri in
  let tree = forest.={uri} in
  match tree with
  | None ->
      Reporter.fatal Internal_error
        ~backtrace:
          (Bwd.of_list
             [
               Asai.Diagnostic.loctextf "when computing completions for %a"
                 URI.pp uri;
             ])
        ~extra_remarks:
          [
            Asai.Diagnostic.loctextf "%a was not found in the index" URI.pp uri;
          ]
  | Some tree ->
      let code = Tree.to_code tree in
      Logs.debug (fun m ->
          m "Received completion request at %s"
            (Yojson.Safe.to_string (L.Position.yojson_of_t position)));
      Logs.debug (fun m -> m "phase is %s" (Tree.show_phase tree));
      let completion_types = Sources.completion_types ~position tree in
      Logs.debug (fun m ->
          m "computed completion types: %a"
            (Format.pp_print_list Sources.pp_completion)
            completion_types);
      let items =
        List.concat_map
          (fun completion ->
            Sources.items_for_completion ~forest ~position ~completion ~code)
          completion_types
      in
      Logs.debug (fun m -> m "items: %d" (List.length items));
      Option.some
      @@ `CompletionList (L.CompletionList.create ~isIncomplete:false ~items ())
