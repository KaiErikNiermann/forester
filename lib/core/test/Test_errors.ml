(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_test
open Forester_core
open Forester_compiler
open Forester_frontend
open Testables

let error_run ~env ~config ~raw_trees =
  with_test_forest
    ~raw_trees
    ~env
    ~config
    (fun path ->
      Sys.chdir (Eio.Path.native_exn path);
      let@ () = Reporter.easy_run in
      let forest = State.make ~env ~config ~dev: false () in
      let forest = Driver.run_until_done Load_all_configured_dirs forest in
      URI.Tbl.iter (fun _ d -> List.iter Reporter.Tty.display d) forest.diagnostics
    )

let import_error = [{path = "test-1.tree"; content = {|\import{nonexistent}|}};]

module Expansion = struct

  let raw_trees = [
    {
      path = "jon.tree";
      content = {|
        \taxon{person}
        \title{Jon Sterling}
      |}
    };
    {
      path = "test-1.tree";
      content = {|
        \def\greet[name]{Hello, \name!}
      |}
    };
    {
      path = "test-2.tree";
      content = {|
      \import{test-1}
      \greet{jon}
      \greet
      |}
    };
    {
      path = "test-3.tree";
      content = {|
        \tag{nonexistent}
      |}
    }
  ]

  let f ~env ~config =
    with_test_forest
      ~raw_trees
      ~env
      ~config
      (fun path ->
        Sys.chdir (Eio.Path.native_exn path);
        let@ () = Reporter.easy_run in
        let forest =
          State.make ~env ~config ~dev: false ()
          |> Driver.run_until_done Load_all_configured_dirs
        in
        URI.Tbl.iter (fun _ d -> List.iter Reporter.Tty.display d) forest.diagnostics
      )
end

let config = Config.default

let () =
  let@ env = Eio_main.run in
  error_run ~env ~config ~raw_trees: import_error;
  Expansion.f ~env ~config
(* error_run ~env ~config ~raw_trees: expansion_error; *)
