(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude

open struct
  module T = Types
  module Dl = Datalog_engine
end

module type S = sig
  val dl_db : Dl.db
  val register_uri : URI.t -> unit
end

let init (db : Dl.db) : (module S) =
(module struct
  let dl_db = Dl.db_create ()

  let register_uri uri =
    let vtx : Vertex.t = T.Uri_vertex uri in
    Dl.db_add_fact dl_db @@ Dl.mk_literal Builtin_relation.is_node [Dl.mk_const vtx];
    let@ host = Option.iter @~ URI.host uri in
    let host_vtx = T.Content_vertex (T.Content [T.Text host]) in
    Dl.db_add_fact dl_db @@ Dl.mk_literal Builtin_relation.in_host [Dl.mk_const vtx; Dl.mk_const host_vtx]

  let add_edge rel ~source ~target =
    Dl.db_add_fact dl_db @@
      Dl.mk_literal
        rel
        [
          Dl.mk_const source;
          Dl.mk_const target
        ]
end)

module Make () : S = (val (init @@ Dl.db_create ()))
