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
val build : State.t -> Forest_graph.t
val dependencies : Tree.code -> State.t -> Forest_graph.t
val resolve_uri_to_code : State.t -> Forest.key -> Tree.code option
val fixup : Tree.code -> State.t -> unit
