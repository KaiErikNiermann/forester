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

let map f node =
  match node with
  | Group (d, t) -> Group (d, f @@ t)
  | Math (m, t) -> Math (m, f @@ t)
  | Subtree (a, t) -> Subtree (a, f @@ t)
  | Link {dest; title} -> Link {dest = f @@ dest; title = Option.map f title}
  | Fun (b, t) -> Fun (b, f t)
  | Put (r, s, t) -> Put (f r, f s, f t)
  | Default (r, s, t) -> Default (f r, f s, f t)
  | Get t -> Get (f t)
  | Xml_tag (q, qs, t) -> Xml_tag (q, List.map (fun (q, t) -> q, f t) qs, f t)
  | Call (t, s) -> Call (f t, s)
  | Object {self; methods} ->
    Object
      {
        self;
        methods = List.map (fun (str, t) -> str, f t) methods
      }
  | Patch {obj; self; super; methods} ->
    Patch
      {
        obj = f obj;
        self;
        super;
        methods = List.map (fun (str, t) -> str, f t) methods
      }
  | Dx_sequent (t, ts) -> Dx_sequent (f t, List.map f ts)
  | Dx_query (s, ps, ns) -> Dx_query (s, List.map f ps, List.map f ns)
  | Dx_const (s, n) -> Dx_const (s, f n)
  | Dx_prop (t, ts) -> Dx_prop (f t, List.map f ts)
  | Text _
  | Verbatim _
  | Var _
  | Sym _
  | TeX_cs _
  | Prim _
  | Results_of_query
  | Transclude
  | Embed_tex
  | Ref
  | Title
  | Parent
  | Taxon
  | Meta
  | Attribution (_, _)
  | Tag _
  | Date
  | Number
  | Dx_var _
  | Dx_execute
  | Route_asset
  | Syndicate_current_tree_as_atom_feed
  | Syndicate_query_as_json_blob
  | Current_tree ->
    node
