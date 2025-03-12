(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type delim =
  Braces | Squares | Parens
[@@deriving show, repr]

type binding_strategy =
  Lazy | Strict
[@@deriving show, repr]

type 'a binding = binding_strategy * 'a
[@@deriving show, repr]

let delim_to_strings = function
  | Braces -> "{", "}"
  | Squares -> "[", "]"
  | Parens -> "(", ")"

type math_mode =
  Inline | Display
[@@deriving show, repr]

type visibility =
  Private | Public
[@@deriving show, repr]
