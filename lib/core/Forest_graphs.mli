(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** A simple graph database. Used to record the {{!Forester_core.Builtin_relation}relations} that exist between trees.*)

module type S = sig
  val dl_db : Datalog_engine.db
  val register_uri : URI.t -> unit
end

module Make () : S

val init : Datalog_engine.db -> (module S)
