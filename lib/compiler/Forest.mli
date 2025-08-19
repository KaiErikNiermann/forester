(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

include module type of URI.Tbl

(**/**)
module T := Types
module Dx := Datalog_expr

val execute_datalog_script : (module Forest_graphs.S) -> (string, Vertex.t) Dx.sequent list -> unit
(**/**)

val analyse_resource : (module Forest_graphs.S) -> T.content T.resource -> unit
(** [analyse_resource graphs resource] traverses {{!Forester_core.Types.resource}[resource]}, recording facts about it in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Link}[Link]}, it adds an edge to the {{!Forester_core.Builtin_relation.links_to}[links_to]} relation in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Transclude}[Transclude]}, it adds an edge to the {{!Forester_core.Builtin_relation.transcludes}[transcludes]} relation in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Datalog_script}[Datalog_script script]}, it runs the script and records the results in {{!Forester_core.Forest_graphs.S}[graphs]}.
    *)

val run_datalog_query :
  (module Forest_graphs.S) ->
  (string, Forester_core.Vertex.t) Dx.query ->
  Forester_core.Vertex_set.t
