(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base
open Forester_xml_names

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest: t; title: t option}
  | Subtree of string option * t
  | Fun of Symbol.t binding list * t
  | Var of Symbol.t
  | Sym of Symbol.t
  | Put of t * t * t
  | Default of t * t * t
  | Get of t
  | Xml_tag of xml_qname * (xml_qname * t) list * t
  | TeX_cs of TeX_cs.t
  | Prim of Prim.t
  | Object of {self: Symbol.t; methods: (string * t) list}
  | Patch of {obj: t; self: Symbol.t; super: Symbol.t; methods: (string * t) list}
  | Call of t * string
  | Results_of_query
  | Transclude
  | Embed_tex
  | Ref
  | Title
  | Parent
  | Taxon
  | Meta
  | Attribution of Types.attribution_role * [`Content | `Uri]
  | Tag of [`Content | `Uri]
  | Date
  | Number
  | Dx_sequent of t * t list
  | Dx_query of string * t list * t list
  | Dx_prop of t * t list
  | Dx_var of string
  | Dx_const of [`Content | `Uri] * t
  | Dx_execute
  | Route_asset
  | Syndicate_query_as_json_blob
  | Syndicate_current_tree_as_atom_feed
  | Current_tree
[@@deriving show]

and t = node Range.located list
[@@deriving show]
