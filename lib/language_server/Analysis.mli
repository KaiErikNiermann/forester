(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

(** {1 Syntactic analysis}

    Functions for analysing parse trees.
*)

(** [contains ~position node] is true if [position] is contained in the range of [node]*)
val contains : position: Lsp.Types.Position.t -> Range.t option -> bool

(** [extract_addr node] attempts to extract a string from the node. It does not verify the validity of the address.
*)
val extract_addr :
  Code.node Range.located ->
  string Range.located option

(** [node_at_code ~position code] returns the smallest node in [code] that contains [position] (if it exists)*)
val node_at_code : position: Lsp.Types.Position.t -> Code.node Range.located list -> Code.node Range.located option

(** [node_at_syn ~position syn] returns the smallest node in [syn] that contains [position] (if it exists)*)
val node_at_syn : position: Lsp.Types.Position.t -> Syn.node Range.located list -> Syn.node Range.located option

(** [addr_at ~position code] uses {!extract_addr} to extract an address from [position] in [code]
*)
val addr_at :
  position: Lsp.Types.Position.t ->
  Code.t ->
  string Asai.Range.located option

(** [flatten code] is a "flat" list of nodes. This function is underspecified and needs to be thought about more.*)
val flatten : Code.t -> Code.t

module Item : sig type t = Path of Trie.path | Addr of string end

val analyse_syntax : Code.t -> Item.t Asai.Range.located Seq.t

val word_at : position: Lsp.Types.Position.t -> Lsp.Text_document.t -> string option

(* [get_visible ~position code] returns the bindings visible at [position] in [code]. This is used to compute context-sensitive completion.*)
val get_visible : forest: State.t -> position: Lsp.Types.Position.t -> Code.t -> (Resolver.Scope.data, Resolver.P.tag) Trie.t

val get_enclosing_code_group : position: Lsp.Types.Position.t -> Code.t -> (delim * Code.t) option
val get_enclosing_syn_group : position: Lsp.Types.Position.t -> Syn.t -> (delim * Syn.t) Asai.Range.located option

val parent_or_prev_at_code :
  position: Lsp.Types.Position.t ->
  Code.node Range.located list ->
  [
    | `Node of Code.node Range.located
    | `Parent of Code.node Range.located
    | `Prev of Code.node Range.located * Code.node Range.located
  ] option

val parent_or_prev_at_syn :
  position: Lsp.Types.Position.t ->
  Syn.node Range.located list ->
  [
    | `Node of Syn.node Range.located
    | `Parent of Syn.node Range.located
    | `Prev of Syn.node Range.located * Syn.node Range.located
  ] option

val find_with_prev :
  position: Lsp.Types.Position.t ->
  'a Range.located list ->
  ('a Range.located option * 'a Range.located) option
