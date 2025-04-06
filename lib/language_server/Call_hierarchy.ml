(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

module T = Types
module L = Lsp.Types

(* The plan is to use this for transclusion/context *)

let incoming (params : L.CallHierarchyIncomingCallsParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let module G = (val forest.graphs) in
  match params with
  | {item; _} ->
    let vertex_to_item (v : _ T.vertex) =
      let from = item in
      let fromRanges = [] in
      match v with
      | T.Uri_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
    in
    match item with
    | {uri; _} ->
      let uri = URI_scheme.path_to_uri ~base: config.url (Lsp.Uri.to_path uri) in
      let vertex = T.Uri_vertex uri in
      let run_query = Forest.run_datalog_query forest.graphs in
      let fwdlinks = run_query @@ Builtin_queries.fwdlinks_datalog vertex in
      Eio.traceln "got %i link items" (Vertex_set.cardinal fwdlinks);
      let children = run_query @@ Builtin_queries.children_datalog vertex in
      Eio.traceln "got %i transclusion items" (Vertex_set.cardinal children);
      let items = Vertex_set.union fwdlinks children |> Vertex_set.to_list |> List.map vertex_to_item in
      Some items

let outgoing (params : L.CallHierarchyOutgoingCallsParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let module G = (val forest.graphs) in
  Eio.traceln "computing outgoing calls";
  match params with
  | {item; _} ->
    let vertex_to_item (v : _ T.vertex) =
      let to_ = item in
      let fromRanges = [] in
      match v with
      | T.Uri_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
    in
    match item with
    | {uri; _} ->
      let uri = URI_scheme.path_to_uri ~base: config.url (Lsp.Uri.to_path uri) in
      let vertex = T.Uri_vertex uri in
      let run_query = Forest.run_datalog_query forest.graphs in
      let backlinks = run_query @@ Builtin_queries.backlinks_datalog vertex in
      Eio.traceln "got %i link items" (Vertex_set.cardinal backlinks);
      let parents = run_query @@ Builtin_queries.context_datalog vertex in
      Eio.traceln "got %i transclusion items" (Vertex_set.cardinal parents);
      let items = Vertex_set.union backlinks parents |> Vertex_set.to_list |> List.map vertex_to_item in
      Some items

let compute (params : L.CallHierarchyPrepareParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  match params with
  | {position; textDocument; _} ->
    let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url textDocument.uri in
    match Imports.resolve_uri_to_code forest uri with
    | None -> None
    | Some tree ->
      let item =
        match Analysis.code_node_at ~position tree.nodes with
        | None -> None
        | Some {loc = _; value} ->
          match value with
          | Def (_, _, _)
          | Fun (_, _) ->
            None
          | Text _
          | Verbatim _
          | Group (_, _)
          | Math (_, _)
          | Ident _
          | Hash_ident _
          | Xml_ident (_, _)
          | Subtree (_, _)
          | Let (_, _, _)
          | Open _
          | Scope _
          | Put (_, _)
          | Default (_, _)
          | Get _
          | Object _
          | Patch _
          | Call (_, _)
          | Import (_, _)
          | Decl_xmlns (_, _)
          | Alloc _
          | Namespace (_, _)
          | Dx_sequent (_, _)
          | Dx_query (_, _, _)
          | Dx_prop (_, _)
          | Dx_var _
          | Dx_const_content _
          | Dx_const_uri _
          | Comment _
          | Error _ ->
            None
      in
      Option.map (fun item -> [item]) item
