(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct module L = Lsp.Types end

let rec strip_syn (syn : Syn.t) : Syn.t =
  let@ Asai.Range.{value; _} = List.map @~ syn in
  Asai.Range.{value = Syn.map strip_syn value; loc = None}

let rec strip_code (code : Code.t) : Code.t =
  let@ Asai.Range.{value; _} = List.map @~ code in
  Asai.Range.{value = Code.map strip_code value; loc = None}

type raw_tree = {path: string; content: string}

let parse_string str =
  let lexbuf = Lexing.from_string str in
  Parse.parse lexbuf

let parse_string_no_loc str =
  Result.map strip_code @@
    parse_string str

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
        Eio.traceln "mkdir: %s" dir_name;
        EP.(mkdir ~perm: 0o755 dir);
        dir
      )
      config.trees
  in
  let create = `Exclusive 0o644 in
  let first_tree_dir = List.hd tree_dirs in
  List.iter
    (fun tree ->
      match Filename.dirname tree.path with
      | "." ->
        EP.(
          save
            ~create
            (first_tree_dir / tree.path)
            tree.content
        )
      | dir ->
        Eio.traceln "%s" dir;
        EP.(
          save
            ~create
            (tmp / dir / Filename.basename tree.path)
            tree.content
        )
    )
    raw_trees;
  kont tmp

let mk_tree ~uri ~code ~expanded =
  Tree.Expanded
    {
      nodes = expanded;
      identity = URI uri;
      units = Trie.empty;
      code = {
        nodes = code;
        identity = URI uri;
        origin = Subtree {parent = Anonymous};
        timestamp = None;
      }
    }

type test_env = {
  dirs: Eio.Fs.dir_ty Eio.Path.t list;
  config: Config.t;
  position: L.Position.t;
}

module Test_env = Algaeff.State.Make(struct type t = test_env end)

let find_tree addr =
  let env = Test_env.get () in
  let dirs = env.dirs in
  Eio.Path.native_exn @@
  Option.get @@
  Dir_scanner.find_tree dirs @@
  URI_scheme.named_uri ~base: env.config.url addr

let find_doc (env : test_env) addr : L.TextDocumentIdentifier.t =
  let path =
    Eio.Path.native_exn @@
    Option.get @@
    Dir_scanner.find_tree env.dirs (URI_scheme.named_uri ~base: env.config.url addr)
  in
    {uri = Lsp.Uri.of_path path}
