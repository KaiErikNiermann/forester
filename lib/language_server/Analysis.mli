(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

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


val code_node_at : position:Lsp.Types.Position.t -> Code.node Range.located list -> Code.node Range.located option
val syn_node_at : position:Lsp.Types.Position.t -> Syn.node Range.located list -> Syn.node Range.located option

(** [addr_at ~position code] uses {!extract_addr} to extract an address from [position] in [code]
*)
val addr_at :
  position: Lsp.Types.Position.t ->
  Code.t ->
  string Asai.Range.located option

(** [flatten code] is a "flat" list of nodes. This function is underspecified and needs to be thought about more.*)
val flatten : Code.t -> Code.t

val analyse_syntax : Code.t -> [`Addr of string | `Path of Trie.path] Asai.Range.located Seq.t

val word_at : position:Lsp.Types.Position.t -> Lsp.Text_document.t -> string option
