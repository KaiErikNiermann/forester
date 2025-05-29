(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type eval_mode =
  | Text_mode
  | TeX_mode
[@@deriving show, repr]

type binding_info =
  | Strict
  | Lazy
[@@deriving show, repr]

type 'a binding = binding_info * 'a
[@@deriving show, repr]

type delim =
  Braces | Squares | Parens
[@@deriving show, repr]

let delim_to_strings = function
  | Braces -> "{", "}"
  | Squares -> "[", "]"
  | Parens -> "(", ")"

type math_mode =
  | Inline
  | Display
[@@deriving show, repr]

type visibility =
  Private | Public
[@@deriving show, repr]

type identity =
  | Anonymous
  | URI of URI.t
[@@deriving show]

let identity_to_uri = function
  | URI uri -> Some uri
  | Anonymous -> None

type origin =
  | Physical of
    (Lsp.Text_document.t [@printer fun ppf doc ->
      Format.pp_print_string
        ppf
        (Lsp.(Uri.to_path @@ Text_document.documentUri doc))])
  | Subtree of {parent: identity}
  | Undefined
[@@deriving show]
