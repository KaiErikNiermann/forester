(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

open struct module T = Types end

type 'a _object = {
  self: string option;
  methods: (string * 'a) list
}
[@@deriving show, repr]

type 'a patch = {
  obj: 'a;
  self: string option;
  super: string option;
  methods: (string * 'a) list
}
[@@deriving show, repr]

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path
  | Hash_ident of string
  | Xml_ident of string option * string
  | Subtree of string option * t
  | Let of Trie.path * string binding list * t
  | Open of Trie.path
  | Scope of t
  | Put of Trie.path * t
  | Default of Trie.path * t
  | Get of Trie.path
  | Fun of string binding list * t
  | Object of t _object
  | Patch of t patch
  | Call of t * string
  | Import of visibility * string
  | Def of Trie.path * string binding list * t
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
[@@deriving show, repr]

and t = node Range.located list
[@@deriving show, repr]

type tree = {
  source_path: string option;
  uri: URI.t option;
  timestamp: float option;
  code: t
}
[@@deriving show, repr]

let import_private x = Import (Private, x)
let import_public x = Import (Public, x)

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)

let map f node =
  match node with
  | Group (d, t) -> Group (d, f t)
  | Math (m, t) -> Math (m, f t)
  | Namespace (p, t) -> Namespace (p, f t)
  | Dx_const_content t -> Dx_const_content (f t)
  | Dx_const_uri t -> Dx_const_uri (f t)
  | Def (p, b, t) -> Def (p, b, f t)
  | Dx_sequent (t, ts) -> Dx_sequent (f t, List.map f ts)
  | Dx_query (s, ts, rs) -> Dx_query (s, List.map f ts, List.map f @@ rs)
  | Dx_prop (t, ts) -> Dx_prop (t, ts)
  | Subtree (s, t) -> Subtree (s, f t)
  | Let (p, b, t) -> Let (p, b, f t)
  | Default (p, t) -> Default (p, f t)
  | Scope t -> Scope (f t)
  | Put (p, t) -> Put (p, f t)
  | Fun (b, t) -> Fun (b, f t)
  | Call (t, s) -> Call (f t, s)
  | Object {self; methods} -> Object {self; methods = List.map (fun (s, t) -> (s, f t)) methods}
  | Patch {obj; self; super; methods} -> Patch {obj = f obj; self; super; methods = List.map (fun (s, t) -> (s, f t)) methods}
  | Text _
  | Verbatim _
  | Ident _
  | Hash_ident _
  | Xml_ident (_, _)
  | Open _
  | Get _
  | Import (_, _)
  | Decl_xmlns (_, _)
  | Alloc _
  | Dx_var _
  | Comment _
  | Error _ ->
    node

let children (node : node Range.located) =
  match node.value with
  | Math (_, t)
  | Group (_, t)
  | Let (_, _, t)
  | Scope t
  | Put (_, t)
  | Fun (_, t)
  | Default (_, t)
  | Def (_, _, t)
  | Namespace (_, t)
  | Dx_const_uri t
  | Dx_const_content t
  | Call (t, _)
  | Subtree (_, t) ->
    t
  | Dx_prop (_, t)
  | Dx_query (_, _, t)
  | Dx_sequent (_, t) ->
    (List.concat t)
  | Object {methods; _} ->
    (methods |> List.map snd |> List.concat)
  | Patch {obj; methods; _} ->
    let methods = (methods |> List.map snd |> List.concat) in
      (List.append obj methods)
  | Text _
  | Verbatim _
  | Ident _
  | Hash_ident _
  | Xml_ident (_, _)
  | Open _
  | Get _
  | Import (_, _)
  | Decl_xmlns (_, _)
  | Alloc _
  | Dx_var _
  | Comment _
  | Error _ ->
    []
