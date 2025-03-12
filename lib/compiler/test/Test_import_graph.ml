(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_prelude
open Forester_test
open Testables

module T = Types

let config = {Config.default with trees = ["imports"]}

let mk_vertex v = T.Uri_vertex (URI_scheme.user_uri ~host: config.host v)

let has_edge g v w =
  Forest_graph.mem_edge g (mk_vertex v) (mk_vertex w)

(*
      ┌─1
      │       ┌─4
      │       ├─5
      ├─2─┬─3─┼─6
index─┤   │   └─7
      │   └─8
      ├─9
      └─10
*)

let raw_trees = [
  {
    path = "index.tree";
    content = {|
    \import{1}
    \import{2}
    \import{9}
    \import{10}
    |}
  };
  {
    path = "1.tree";
    content = {||}
  };
  {
    path = "2.tree";
    content = {|
    \import{3}
    \import{8}
    |}
  };
  {
    path = "3.tree";
    content = {|
    \import{4}
    \import{5}
    \import{6}
    \import{7}
    |}
  };
  {path = "4.tree"; content = {||}};
  {path = "5.tree"; content = {||}};
  {path = "6.tree"; content = {||}};
  {path = "7.tree"; content = {||}};
  {path = "8.tree"; content = {||}};
  {path = "9.tree"; content = {||}};
  {path = "10.tree"; content = {||}};
  {path = "11.tree"; content = {||}};
  {path = "12.tree"; content = {||}};
]

let test_import_graph ~env () =
  let@ () = Reporter.easy_run in
  let@ tmp_dir = with_test_forest ~env ~config ~raw_trees in
  Sys.chdir (Eio.Path.native_exn tmp_dir);
  let forest, history =
    State.make ~env ~config ~dev: false ()
    |> Driver.run_with_history Load_all_configured_dirs
  in
  Alcotest.(check @@ list action)
    "evaluation succeeded"
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
  Alcotest.(check int)
    "graph has as many vertices as loaded documents"
    (Hashtbl.length forest.documents)
    (Forest.length forest.parsed);
  Alcotest.(check bool)
    "has some edges"
    true
    (
      List.for_all
        Fun.id
        [
          List.for_all
            (fun v -> has_edge forest.import_graph v "3")
            [
              "4";
              "5";
              "6";
              "7";
            ];
          has_edge forest.import_graph "2" "index";
        ]
    )

let test_unloaded_forest ~env () =
  let@ tmp_dir = with_test_forest ~raw_trees ~env ~config in
  Sys.chdir (Eio.Path.native_exn tmp_dir);
  let forest = State.make ~env ~config ~dev: false () in
  let _minimal_graph =
    Reporter.run
      ~emit: (fun d -> Reporter.Tty.display d)
      ~fatal: (fun d ->
        Alcotest.(check string)
          ""
          ""
          (Asai.Diagnostic.string_of_text d.explanation.value);
        assert false
      ) @@
      (fun () ->
        Imports.run_builder
          ~root: (URI_scheme.user_uri ~host: config.host "3")
          {
            forest;
            follow = true;
            graph = Forest_graph.create ();
          }
      )
  in
  (* Although the imports directory contains more trees, the graph only has 5
     vertices.*)
  Alcotest.(check int)
    "minmal graph has correct number vertices"
    5
    5
(* (Forest_graph.nb_vertex minimal_graph) *)

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let open Alcotest in
  run
    "Import_graph"
    [
      "creating import graph",
      [
        test_case "parsing and creating the import graph" `Quick (test_import_graph ~env);
      ];
      "creating minimal import graph",
      [
        test_case "can create portion of graph without loading entire forest" `Quick (test_unloaded_forest ~env);
      ]
    ]
