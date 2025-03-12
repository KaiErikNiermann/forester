(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Types
type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  dev: bool;
  config: Config.t;
  units: Expand.Env.t;
  documents: (Lsp.Uri.t, Lsp.Text_document.t) Hashtbl.t;
  parsed: Code.tree Forest.t;
  expanded: Syn.t Forest.t;
  diagnostics: Diagnostic_store.t;
  resources: resource Forest.t;
  graphs: (module Forest_graphs.S);
  import_graph: Forest_graph.t;
  dependency_cache: Cache.t;
  resolver: string URI.Tbl.t;
  search_index: Forester_search.Index.t;
}

let make
  ~(env : Eio_unix.Stdenv.base)
  ~(config : Config.t)
  ~(dev : bool)
  ?(graphs = (module Forest_graphs.Make (): Forest_graphs.S))
  ?(import_graph = Forest_graph.create ~size: 1000 ())
  ?(parsed = Forest.create 1000)
  ?(documents = Hashtbl.create 1000)
  ?(resolver = URI.Tbl.create 1000)
  ?(expanded = Forest.create 1000)
  ?(resources = Forest.create 1000)
  ?(diagnostics = Diagnostic_store.create 100)
  ?(units = Expand.Env.empty)
  ?(search_index = Forester_search.Index.create [])
  ?(dependency_cache = Cache.empty)
  ()
= {env; dev; config; units; documents; diagnostics; parsed; expanded; resources; resolver; import_graph; graphs; search_index; dependency_cache;}

let serialize_graphs
  : (module Forest_graphs.S) -> 'a
= fun s ->
  let module Graphs = (val s) in
  Graphs.dl_db

(* This function is a temporary solution. Here is the plan:

  Write this function, then go to Phases.ml and make sure that the computations
  we are doing in bulk here are carried out with each step.
   *)
let batch_write : t -> _ = function
  | {import_graph;
    _
  } ->
    (* let dl_db = serialize_graphs graphs in *)
    let open Cache in
    let module Gmap = Forest_graph.Map(Cache.Dependecy_graph) in
    let tbl = Dependency_tbl.create 100 in
    let now = Unix.time () in
    let g =
      Gmap.map
        (function
          | T.Content_vertex _ ->
            (*Import graph has no content vertices*)
            assert false
          | T.Uri_vertex uri ->
            let item = Item.Tree uri in
            Dependency_tbl.add tbl item Item.{timestamp = Some now; color = Green};
            item
        )
        import_graph
    in
    {Cache.empty with graph = g; tbl;}

let reconstruct = fun ~env: _ ~(_config : Config.t) paths cache ->
  match cache with
  | {search_index = _; _} ->
    (* let init = Phases.init ~env ~config ~dev: true in *)
    (* let graphs = Forest_graphs.init dl_db in *)
    paths
    |> Seq.iter (fun _path ->
        (* let uri = URI_scheme.path_to_uri ~host: config.host (Eio.Path.native_exn path) in *)
        (* match URI.Tbl.find_opt forest uri with *)
        (* | None -> () *)
        (* | Some tree -> *)
        (*   match check_timestamp path tree.timestamp with *)
        (*   | _ -> () *)
        ()
      )
