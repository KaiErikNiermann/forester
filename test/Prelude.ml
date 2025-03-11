(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_parser

let rec strip_loc : Code.t -> Code.t = fun nodes ->
  List.map
    (fun Range.{value; _} -> Range.{value = strip_loc_node value; loc = None})
    nodes

and strip_loc_node : Code.node -> Code.node = fun node ->
  let open Code in
  match node with
  | Group (d, t) -> Group (d, strip_loc t)
  | Math (m, t) -> Math (m, strip_loc t)
  | Namespace (p, t) -> Namespace (p, strip_loc t)
  | Dx_const_content t -> Dx_const_content (strip_loc t)
  | Dx_const_iri t -> Dx_const_iri (strip_loc t)
  | Def (p, b, t) -> Def (p, b, strip_loc t)
  | Dx_sequent (t, ts) -> Dx_sequent (strip_loc t, List.map strip_loc ts)
  | Dx_query (s, ts, rs) -> Dx_query (s, List.map strip_loc ts, List.map strip_loc rs)
  | Dx_prop (t, ts) -> Dx_prop (t, ts)
  | Subtree (s, t) -> Subtree (s, strip_loc t)
  | Let (p, b, t) -> Let (p, b, strip_loc t)
  | Default (p, t) -> Default (p, strip_loc t)
  | Scope t -> Scope (strip_loc t)
  | Put (p, t) -> Put (p, strip_loc t)
  | Fun (b, t) -> Fun (b, strip_loc t)
  | Call (t, s) -> Call (strip_loc t, s)
  | Text _
  | Verbatim _
  | Ident _
  | Hash_ident _
  | Xml_ident (_, _)
  | Open _
  | Get _
  (* TODO: strip object *)
  | Object _
  | Patch _
  | Import (_, _)
  | Decl_xmlns (_, _)
  | Alloc _
  | Dx_var _
  | Comment _
  | Error _ ->
    node

type raw_tree = {path: string; content: string}

let parse_string str =
  let lexbuf = Lexing.from_string str in
  let res = Parse.parse ~source: (`String {title = None; content = str}) lexbuf in
  Result.map strip_loc res

let with_open_tmp_dir ~env kont =
  let open Eio in
  (* let dir_name = string_of_int @@ Oo.id (object end) in *)
  let cwd = Eio.Stdenv.cwd env in
  let tmp = "_tmp" in
  (* Path.rmtree ~missing_ok: true tmp_path; *)
  Path.mkdirs ~exists_ok: true ~perm: 0o755 Path.(cwd / tmp);
  let tmp_dir =
    Filename.temp_dir
      ~temp_dir: tmp
      ~perms: 0o755
      ""
      ""
  in
  Logs.app (fun m -> m "%s" tmp_dir);
  (* Path.mkdirs ~exists_ok: true ~perm: 0o755 tmp_path; *)
  let tmp_path = Eio.Path.(cwd / tmp_dir) in
  (* let@ p = Eio.Path.with_open_dir tmp_path in *)
  let result = kont tmp_path in
  Path.rmtree ~missing_ok: true tmp_path;
  result

let with_test_forest ~env ~raw_trees ~(config : Config.t) kont =
  let@ tmp = with_open_tmp_dir ~env in
  let module EP = Eio.Path in
  let tree_dirs =
    List.map
      (fun dir_name ->
        let dir = EP.(tmp / dir_name) in
        EP.(mkdir ~perm: 0o755 dir);
        dir
      )
      config.trees
  in
  let create = `Exclusive 0o644 in
  let first_tree_dir = List.hd tree_dirs in
  List.iter
    (fun tree ->
      EP.(
        save
          ~create
          (first_tree_dir / tree.path)
          tree.content
      )
    )
    raw_trees;
  kont tmp
