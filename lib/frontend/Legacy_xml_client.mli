(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T := Types
module P := Pure_html

val local_path_components : Config.t -> URI.t -> string list
val route : State.t -> URI.t -> URI.t

val render_article : State.t -> T.content T.article -> P.node
val render_content : State.t -> T.content -> P.node list
val pp_xml : forest: State.t -> ?stylesheet: string -> Format.formatter -> T.content T.article -> unit
