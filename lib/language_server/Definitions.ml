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

let (let*) = Option.bind

let compute
    (params : L.DefinitionParams.t)
    : L.Locations.t option
  =
  match params with
  | {textDocument;
    position;
    _;
  } ->
    let Lsp_state.{forest; _} = Lsp_state.get () in
    let host = forest.config.host in
    let uri = URI_scheme.lsp_uri_to_uri ~host textDocument.uri in
    match Option.bind forest.={uri} Tree.to_code with
    | None -> None
    | Some code ->
      match Analysis.addr_at ~position code.nodes with
      | None -> assert false
      | Some addr ->
        let uri = URI_scheme.user_uri ~host addr in
        let path = URI.Tbl.find forest.resolver uri in
        let uri = Lsp.Uri.of_path path in
        Logs.debug (fun m -> m "Definitions: %s" path);
        let range = L.Range.create ~start: {character = 1; line = 0} ~end_: {character = 1; line = 0} in
        Some
          (`Location [L.Location.{uri; range}])
