(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type analysis_env = {
  follow: bool;
  forest: State.t;
  graph: Forest_graph.t;
}

val load_tree : Eio.Fs.dir_ty Eio.Path.t -> Lsp.Text_document.t
val register_document : host: string -> Forest_graph.t -> Lsp.Text_document.t -> unit
val build : State.t -> Forest_graph.t
val run_builder : ?root: URI.t -> analysis_env -> Forest_graph.t
val dependencies : Code.tree -> State.t -> Forest_graph.t
val resolve_iri_to_code : State.t -> Forest.key -> (Code.tree * Lsp.Text_document.t) option
val fixup : Code.tree -> State.t -> unit
