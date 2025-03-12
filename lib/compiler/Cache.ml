(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module T = Types

(*Inspired by
https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html
*)

module Item = struct
  type color = Red | Green | Unknown

  type meta = {
    timestamp: float option;
    color: color;
  }

  type t = (* Key *)
  | Tree of URI.t
  | Path of Trie.path
  | Asset of string

  (* TODO: Hand-roll these for performance? *)
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)

  let check_timestamp
  = fun path timestamp ->
    match timestamp with
    | Some timestamp ->
      let last_modified = Eio.Path.(stat ~follow: true @@ path).mtime in
      if last_modified > timestamp then
        Red
      else Green
    | _ -> Red
end

(* The core datastructure here is a {graph; hashtbl}
   We shadow many functions from both datastructures.
   *)

module Dependency_tbl = Hashtbl.Make(Item)

module Dependecy_graph : sig
    type t
    type vertex = Item.t
    val add_vertex : t -> vertex -> t

    val create : ?size: int -> unit -> t
    val pred : t -> vertex -> vertex list
    val empty : unit -> t
  end
= struct
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(Item)
  type t = G.t
  type vertex = Item.t
  let create = G.create
  let pred = G.pred
  let add_vertex g v = G.add_vertex g v; g
  let empty = G.create ?size: None
end

type t = {
  tbl: Item.meta Dependency_tbl.t;
  graph: Dependecy_graph.t;
  db: Datalog_engine.db;
}

let empty = {
  tbl = Dependency_tbl.create 1000;
  graph = Dependecy_graph.create ();
  db = Datalog_engine.db_create ();
}

let find_opt t iri = Dependency_tbl.find_opt t.tbl iri

let add_vertex t v color =
  ignore @@ Dependecy_graph.add_vertex t.graph v;
  Dependency_tbl.add t.tbl v color

let pred t v = Dependecy_graph.pred t.graph v

let get_changed_paths
  : host: string ->
  t ->
  Eio.Fs.dir_ty Eio.Path.t List.t ->
  Eio.Fs.dir_ty Eio.Path.t Seq.t
= fun ~host cache dirs ->
  Dir_scanner.scan_directories dirs
  |> Seq.filter_map
      (fun path ->
        let path_str = Eio.Path.native_exn path in
        let iri = URI_scheme.path_to_iri ~host path_str in
        let last_modified = Eio.Path.(stat ~follow: true path).mtime in
        (* "flipped" bind, by default returns the current path. IDK, I am being lazy. *)
        let (let*) o f = match o with None -> Some path | Some v -> f v in
        let* {timestamp; _} = Dependency_tbl.find_opt cache.tbl (Tree iri) in
        let* last_seen = timestamp in
        if last_modified > last_seen then
          Some path
        else
          None
      )

let rec try_mark_green t node =
  let exception Done of bool in
  let dependencies =
    List.filter_map
      (fun v ->
        match Dependency_tbl.find_opt t.tbl v with
        | None -> None
        | Some c ->
          Some (v, c)
      )
      (pred t node)
  in
  let result =
    try
      List.fold_right
        (fun (dep, Item.{color; _}) acc ->
          match color with
          | Red -> raise (Done false)
          | Green -> true && acc
          | Unknown ->
            if try_mark_green t dep then true && acc
            else raise (Done false)
        )
        dependencies
        true
    with
      | Done b -> b
  in
  if result then
    Dependency_tbl.replace
      t.tbl
      node
      {
        color = Green;
        timestamp = Some (Unix.time ())
      }
  else
    assert false;
  result

let marshal filename (v : t) =
  let oc = open_out_bin filename in
  Fun.protect
    ~finally: (fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc v [])

let unmarshal filename : t =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally: (fun () -> close_in ic)
    (fun () -> Marshal.from_channel ic)
