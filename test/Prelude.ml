(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_parser

let rec strip_syn : Syn.t -> Syn.t = fun syn ->
  List.map
    (fun Asai.Range.{value; _} ->
      Asai.Range.{value = Syn.map strip_syn value; loc = None}
    )
    syn

let rec strip_code : Code.t -> Code.t = fun code ->
  List.map
    (fun Asai.Range.{value; _} ->
      Asai.Range.{value = Code.map strip_code value; loc = None}
    )
    code

type raw_tree = {path: string; content: string}

let parse_string_loc str =
  let lexbuf = Lexing.from_string str in
  Parse.parse lexbuf

let parse_string str =
  Result.map strip_code @@
    parse_string_loc str

let with_open_tmp_dir ~env kont =
  let open Eio in
  let cwd = Eio.Stdenv.cwd env in
  let tmp = "_tmp" in
  Path.mkdirs ~exists_ok: true ~perm: 0o755 Path.(cwd / tmp);
  let tmp_dir =
    Filename.temp_dir
      ~temp_dir: tmp
      ~perms: 0o755
      ""
      ""
  in
  Logs.app (fun m -> m "%s" tmp_dir);
  let tmp_path = Eio.Path.(cwd / tmp_dir) in
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
