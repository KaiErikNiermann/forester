(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct
  module L = Lsp.Types
  module T = Types
end

let compute (params : L.DocumentHighlightParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url params.textDocument.uri in
  let@ tree = Option.map @~ State.get_code forest uri in
  let@ Range.{loc; value} = List.map @~ tree.nodes in
  let range = Lsp_shims.Loc.lsp_range_of_range loc in
  let kind =
    match value with
    | Code.Text _
    | Code.Verbatim _
    | Code.Group (_, _)
    | Code.Math (_, _)
    | Code.Ident _
    | Code.Hash_ident _
    | Code.Xml_ident (_, _)
    | Code.Subtree (_, _)
    | Code.Let (_, _, _)
    | Code.Open _
    | Code.Scope _
    | Code.Put (_, _)
    | Code.Default (_, _)
    | Code.Get _
    | Code.Fun (_, _)
    | Code.Object _
    | Code.Patch _
    | Code.Call (_, _)
    | Code.Import (_, _)
    | Code.Def (_, _, _)
    | Code.Decl_xmlns (_, _)
    | Code.Alloc _
    | Code.Namespace (_, _)
    | Code.Dx_sequent (_, _)
    | Code.Dx_query (_, _, _)
    | Code.Dx_prop (_, _)
    | Code.Dx_var _
    | Code.Dx_const_content _
    | Code.Dx_const_uri _
    | Code.Comment _
    | Code.Error _ ->
      None
  in
  L.DocumentHighlight.create ~range ?kind ()
