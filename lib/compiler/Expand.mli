(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Unit_map = URI.Map

val builtins : (string list * Syn.node) list

module Builtins :
sig
  val register_builtins : (Yuujinchou.Trie.path * Syn.node) list -> unit
  module Transclude :
  sig
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val show_metadata_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end
end

val suggestions : string list -> ('a, 'b) Trie.t -> (Trie.path * 'a * int) list

val expand_tree : forest:State.t -> Tree.code -> Tree.syn * Reporter.Message.t Asai.Diagnostic.t list
val expand : Code.t -> Syn.t
val scope_effect : Code.node Asai.Range.located -> (unit -> 'a) -> 'a
