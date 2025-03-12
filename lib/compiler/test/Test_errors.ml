(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forester_test
open Testables

let parse_string str =
  let lexbuf = Lexing.from_string str in
  let res = Parse.parse ~source: (`String {title = None; content = str}) lexbuf in
  Result.map strip_loc res

let _test_parse_error_explanation src expect =
  Alcotest.(check @@ result code string)
    ""
    (Result.Error expect)
    (
      parse_string src
      |> Result.map_error
          (fun d -> Asai.Diagnostic.string_of_text d.explanation.value)
    )

let raw_trees = [
  {
    path = "parse_error.tree";
    content = "\\})--aa]jv"
  };
  {
    path = "import_error.tree";
    content = {|\import{nonexistent}|}
  }
]

let check_diagnostic expect kont =
  let fatal = fun d ->
    Alcotest.(check message)
      ""
      expect
      (d.message)
  in
  let emit = Fun.const () in
  Reporter.run ~fatal ~emit (fun () -> kont (); ())

let () =
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = Config.default in
  let test () =
    let@ tmp_dir = with_test_forest ~env ~raw_trees ~config in
    Sys.chdir (Eio.Path.native_exn tmp_dir);
    let@ () = check_diagnostic Resource_not_found in
    let forest, history =
      State.make ~env ~config ~dev: false ()
      |> Driver.run_with_history Load_all_configured_dirs
    in
    Alcotest.(check @@ list action)
      ""
      [
        Load_all_configured_dirs;
        Parse_all;
        Build_import_graph;
        Expand_all;
        Eval_all;
        (Run_jobs []);
        Done
      ]
      history;
    Alcotest.(check int) "" 1 (Diagnostic_store.length forest.diagnostics);
    Alcotest.(check bool) "" true (URI.Tbl.length forest.parsed = (Hashtbl.length forest.documents - 1))
  in
  let test_expansion () = Alcotest.(check string) "" "" "" in
  let open Alcotest in
  run
    "verify error reporting"
    [
      "parsing",
      [
        test_case "nonexistent tree" `Quick test;
      ];
      "expanding",
      [
        test_case "expansion" `Quick test_expansion;
      ];
      "evaluating",
      [
      ];
      "driver",
      [];
    ]
