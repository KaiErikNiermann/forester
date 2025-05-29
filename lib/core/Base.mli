(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** {1 Base} *)

type eval_mode =
  | Text_mode
  | TeX_mode
[@@deriving show, repr]

type binding_info =
  | Strict
  | Lazy
[@@deriving show, repr]

(** {2 Delimiters} *)

type delim = Braces | Squares | Parens
val pp_delim :
  Format.formatter -> delim -> unit
val show_delim : delim -> string
val delim_t : delim Repr.t
val delim_to_strings : delim -> string * string

(** {2 Variable binding} *)
type 'a binding = binding_info * 'a
[@@deriving show, repr]

(** {2 Math modes} *)

type math_mode = Inline | Display

val pp_math_mode :
  Format.formatter ->
  math_mode ->
  unit

val show_math_mode : math_mode -> string

val math_mode_t : math_mode Repr.t

(** {2 Import visibility} *)

type visibility = Private | Public

val pp_visibility :
  Format.formatter ->
  visibility ->
  unit
val show_visibility : visibility -> string
type identity = Anonymous | URI of URI.t
val pp_identity : Format.formatter -> identity -> unit
val show_identity : identity -> string
val identity_to_uri : identity -> URI.t option
type origin =
  | Physical of Lsp.Text_document.t
  | Subtree of {parent: identity;}
  | Undefined
val pp_origin : Format.formatter -> origin -> unit
val show_origin : origin -> string
val visibility_t : visibility Repr.t
