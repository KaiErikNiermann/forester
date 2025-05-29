(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Unit_map = URI.Map

val initial_visible_trie : (Syn.resolver_data, Range.t option) Trie.t

module Builtins :
sig
  module Transclude :
  sig
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val show_metadata_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end
end

val expand : forest: State.t -> Code.t -> Syn.t
val expand_tree : forest: State.t -> Tree.code -> Tree.syn * Reporter.Message.t Asai.Diagnostic.t list

type 'a Effect.t += Entered_range : Range.t option -> unit Effect.t
val expand_eff : forest: State.t -> Code.t -> Syn.t
