(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** {1 Base} *)

(** {2 Delimiters} *)

type delim = Braces | Squares | Parens
val pp_delim :
  Format.formatter -> delim -> unit
val show_delim : delim -> string
val delim_t : delim Repr.t
val delim_to_strings : delim -> string * string

(** {2 Variable binding} *)

type binding_strategy = Lazy | Strict
val pp_binding_strategy :
  Format.formatter ->
  binding_strategy ->
  unit
val show_binding_strategy : binding_strategy -> string
val binding_strategy_t : binding_strategy Repr.t
type 'a binding = binding_strategy * 'a
val pp_binding :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a binding ->
  unit
val show_binding :
  (Format.formatter -> 'a -> unit) ->
  'a binding ->
  string
val binding_t : 'a Repr.t -> (binding_strategy * 'a) Repr.t

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
  | Subtree of { parent : identity; }
  | Undefined
val pp_origin : Format.formatter -> origin -> unit
val show_origin : origin -> string
val visibility_t : visibility Repr.t
