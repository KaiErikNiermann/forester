(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_test
open Forester_core
open Forester_compiler
open Testables

module Import_errors = struct
  let raw_trees = [{path = "test-1.tree"; content = {|\import{nonexistent}|}};]
  let test_case ~env ~config =
    with_test_forest
      ~raw_trees
      ~env
      ~config
      (fun path ->
        Sys.chdir (Eio.Path.native_exn path);
        let@ () = Reporter.easy_run in
        (* let forest = State.make ~env ~config ~dev: false () in *)
        let _ = Driver.batch_run ~env ~config ~dev: false in
        ()
      )
end

let () =
  let@ env = Eio_main.run in
  Import_errors.test_case ~env ~config: Config.default
