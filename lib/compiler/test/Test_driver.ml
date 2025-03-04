(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test
open Testables

let config = Config.default

let check_actions ~env () =
  let forest, history =
    setup_forest
      ~raw_trees: [{path = "foo.tree"; content = {|\title{hello}|}}]
      ~env
      ~config
      (fun path ->
        Sys.chdir (Eio.Path.native_exn path);
        let@ () = Reporter.easy_run in
        let forest = State.make ~env ~config ~dev: false () in
        Driver.run_with_history Load_all_configured_dirs forest
      )
  in
  Alcotest.(check @@ list action)
    ""
    [
      Load_all_configured_dirs;
      Parse_all;
      Build_import_graph;
      Expand_all;
      Eval_all;
      Run_jobs [];
      Done;
    ]
    history;
  Alcotest.(check @@ int) "" 1 (Iri_tbl.length forest.resources)

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  (* Logs.set_reporter (Logs.format_reporter ()); *)
  (* let () = assert false in *)
  let open Alcotest in
  run
    "Test_driver"
    [
      "Steps",
      [
        test_case "Batch compilation steps" `Quick (check_actions ~env)
      ]
    ]
