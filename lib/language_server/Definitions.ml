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

let compute (params : L.DefinitionParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url params.textDocument.uri in
  let@ tree = Option.bind forest.={uri} in
  let@ {nodes; _} = Option.bind @@ Tree.to_code tree in
  let@ {value = str; _} = Option.bind @@ Analysis.addr_at ~position: params.position nodes in
  let uri = URI_scheme.named_uri ~base: forest.config.url str in
  let@ path = Option.map @~ URI.Tbl.find_opt forest.resolver uri in
  let uri = Lsp.Uri.of_path path in
  let range = L.Range.create ~start: {character = 1; line = 0} ~end_: {character = 1; line = 0} in
  `Location [L.Location.{uri; range}]
