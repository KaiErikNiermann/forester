(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

module T := Types

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path
  | Hash_ident of string
  | Xml_ident of string option * string
  | Subtree of string option * t
  | Let of
    Trie.path
    * Trie.path binding list
    * t
  | Open of Trie.path
  | Scope of t
  | Put of Trie.path * t
  | Default of Trie.path * t
  | Get of Trie.path
  | Fun of Trie.path binding list * t
  | Object of t _object
  | Patch of t patch
  | Call of t * string
  | Import of visibility * string
  | Def of
    Trie.path
    * Trie.path binding list
    * t
  | Decl_xmlns of string * string
  | Alloc of Trie.path
  | Namespace of Trie.path * t
  | Dx_sequent of t * t list
  | Dx_query of string * t list * t list
  | Dx_prop of t * t list
  | Dx_var of string
  | Dx_const_content of t
  | Dx_const_uri of t
  | Comment of string
  | Error of string
[@@deriving show]

and t = node Range.located list

and 'a _object = {
  self: Trie.path option;
  methods: (string * 'a) list;
}

and 'a patch = {
  obj: 'a;
  self: Trie.path option;
  methods: (string * 'a) list;
}

val t : t Repr.t
val pp : Format.formatter -> t -> unit

type tree = {
  source_path: string option;
  uri: URI.t option;
  timestamp: float option;
  code: t;
}
[@@deriving show]

val parens : t -> node
val squares : t -> node
val braces : t -> node

val import_private : string -> node
val import_public : string -> node
val inline_math : t -> node
val display_math : t -> node

val map : (t -> t) -> node -> node
