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

module T = Types

let raw_trees =
  let t1 = {path = "t1.tree"; content = {||}} in
  let t2 = {path = "t2.tree"; content = {||}} in
  let t3 = {path = "t3.tree"; content = {||}} in
  let t4 = {path = "t4.tree"; content = {||}} in
  let t5 = {path = "t5.tree"; content = {||}} in
  let t6 = {path = "t6.tree"; content = {||}} in
  let t7 = {path = "t7.tree"; content = {||}} in
  let t8 = {path = "t8.tree"; content = {||}} in
  [t1; t2; t3; t4; t5; t6; t7; t8;]

let test_batch_run ~env () =
  let@ () = Reporter.easy_run in
  let config = Config.default in
  with_test_forest ~raw_trees ~env ~config (fun path ->
    Sys.chdir (Eio.Path.native_exn path);
    let@ () = Reporter.easy_run in
    let forest, history =
      State.make ~env ~config ~dev: false ()
      |> Driver.run_with_history Load_all_configured_dirs
    in
    Alcotest.(check int) "" 7 @@ List.length history;
    Alcotest.(check int) "" 0 @@ Diagnostic_store.length forest.diagnostics;
    Alcotest.(check int) "" 8 @@ URI.Tbl.length forest.expanded;
    Alcotest.(check int) "" 8 @@ List.length @@ Forest.get_all_articles forest.resources
  )

let test_includes_paths ~env () =
  let@ () = Reporter.easy_run in
  let config = Config.default in
  with_test_forest ~raw_trees ~env ~config (fun path ->
    Sys.chdir (Eio.Path.native_exn path);
    let@ () = Reporter.easy_run in
    let forest, history =
      State.make ~env ~config ~dev: true ()
      |> Driver.run_with_history Load_all_configured_dirs
    in
    Alcotest.(check int) "number of loaded documents" 8 (Hashtbl.length forest.documents);
    Alcotest.(check int) "number of parsed trees" 8 (URI.Tbl.length forest.parsed);
    Alcotest.(check int) "number of trees in resolver" 8 (URI.Tbl.length forest.resolver);
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
    let uri = (URI.of_string_exn "forest://my-forest/t8") in
    let path =
      match Forest.get_article uri forest.resources with
      | None -> Reporter.fatalf Internal_error ""
      | Some {frontmatter = {source_path; _}; _} ->
        source_path
    in
    Alcotest.(check bool)
      "path is some"
      true
      (Option.is_some path)
  )

let test_reparsing ~env () =
  let config = Config.default in
  with_test_forest ~raw_trees ~env ~config (fun tmp_path ->
    Logs.app (fun m -> m "In temp dir %s" (Unix.realpath @@ Eio.Path.native_exn tmp_path));
    let@ () = Reporter.easy_run in
    let forest =
      State.make ~env ~config ~dev: false ()
      |> Driver.run_action Load_all_configured_dirs
    in
    let reparse_addr = "t8.tree" in
    let reparse_uri = URI_scheme.path_to_uri ~host: config.host reparse_addr in
    let vtx = T.Uri_vertex reparse_uri in
    Alcotest.(check int)
      "Number of vertices before reparsing"
      8
      (Forest_graph.nb_vertex forest.import_graph);
    Alcotest.(check int)
      "old vertex has no import"
      0
      (Forest_graph.in_degree forest.import_graph vtx);
    let proof =
      Hashtbl.to_seq_keys forest.documents
      |> Seq.find_map
          (fun uri ->
            if Lsp.Uri.to_path uri
              |> String.ends_with ~suffix: "t8.tree" then
              begin
                let path = Eio.Path.(forest.env#fs / (Lsp.Uri.to_path uri)) in
                Eio.Path.save ~create: (`Or_truncate 0o644) path {|\import{t1}|};
                let reparsed = Driver.run_action (Load_tree (Lsp.Uri.to_path uri)) forest in
                let import_graph = reparsed.import_graph in
                Alcotest.(check bool)
                  "vertex has an import"
                  true
                  (Forest_graph.in_degree import_graph vtx > 0);
                Some ()
              end
            else None
          )
    in
    Alcotest.(check bool)
      "did run"
      true
      (Option.is_some proof)
  )

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = {Config.default with trees = []} in
  let test_omits_paths () =
    let@ () = Reporter.easy_run in
    let forest = Driver.batch_run ~env ~config ~dev: false in
    let path =
      let@ {frontmatter = {source_path; _}; _} =
        Option.bind @@ Forest.get_article (URI.of_string_exn "forest://my-forest/index") forest.resources
      in
      source_path
    in
    Alcotest.(check bool) "" true @@ Option.is_none path
  in
  let open Alcotest in
  run
    "check compiler internals"
    [
      "pipeline",
      [
        test_case "basic batch run" `Quick (test_batch_run ~env);
        test_case "reparsing" `Quick (test_reparsing ~env);
      ];
      "dev mode",
      [
        test_case "includes paths in dev mode" `Quick (test_includes_paths ~env);
        test_case "omits paths outside dev mode" `Quick test_omits_paths;
      ]
    ]
