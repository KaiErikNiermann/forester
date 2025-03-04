(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler

module T = Types

module Mock_forest = struct
  let languageId = "forester"
  let mk_doc ?(version = 1) path content =
    Lsp.Text_document.make
      ~position_encoding: `UTF8
      {
        textDocument = {
          languageId;
          text = content;
          uri = Lsp.Uri.of_path path;
          version;
        }
      }
  let t1 = mk_doc "/test/t1.tree" {||}
  let t2 = mk_doc "/test/t2.tree" {||}
  let t3 = mk_doc "/test/t3.tree" {||}
  let t4 = mk_doc "/test/t4.tree" {||}
  let t5 = mk_doc "/test/t5.tree" {||}
  let t6 = mk_doc "/test/t6.tree" {||}
  let t7 = mk_doc "/test/t7.tree" {||}
  let t8 = mk_doc "/test/reparse.tree" {||}

  let add_trees trees (forest : State.t) =
    trees
    |> List.iter (fun doc ->
        let uri = Lsp.Text_document.documentUri doc in
        Hashtbl.add forest.documents uri doc;
        let path = Lsp.Uri.to_path uri in
        let iri = Iri_scheme.uri_to_iri ~host: forest.config.host uri in
        Iri_tbl.replace forest.resolver iri path;
      );
    forest

  let init ~env ~config () =
    State.make ~env ~config ~dev: true ()
    |> add_trees [t1; t2; t3; t4; t5; t6; t7; t8]

  let build ~env ~config () =
    init ~env ~config ()
    |> Driver.run_action Parse_all
end

let () =
  (* Logs.set_level (Some Debug); *)
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = {Config.default with trees = []} in

  (* When a tree changes, we need to recompute its dependencies and update the
     import graph*)
  let test_reparsing () =
    let@ () = Reporter.test_run in
    (*First, perform a regular batch compilation run.*)
    let forest =
      Mock_forest.init ~env ~config ()
      |> Driver.run_action Parse_all
    in
    let path = "/test/reparse.tree" in
    let vtx = T.Iri_vertex (Iri_scheme.path_to_iri ~host: config.host path) in
    Format.printf "%a@." T.(pp_vertex pp_content) vtx;
    let old_import_graph = forest.import_graph in
    Alcotest.(check int) "old vertex has no import" 0 (Forest_graph.in_degree old_import_graph vtx);
    let reparsed_forest, _ =
      let open Phases in
      let document =
        (* Create a Text_document.t with new content.*)
        Mock_forest.mk_doc
          ~version: 2
          path
          {| \import{t1}|}
      in
      reparse document forest
    in
    let import_graph = reparsed_forest.import_graph in
    Forest_graph.iter_vertex
      (fun v -> Format.printf "%a@." T.(pp_vertex pp_content) v)
      import_graph;
    Alcotest.(check bool) "vertex has an import" true (Forest_graph.in_degree import_graph vtx > 0);
  in
  let test_batch_run () =
    let forest =
      let@ () = Reporter.easy_run in
      Mock_forest.build ~env ~config ()
    (* Driver.batch_run ~env ~config ~dev: false *)
    in
    Alcotest.(check int) "" 8 @@ List.length @@ Forest.get_all_articles forest.resources
  in
  let test_includes_paths () =
    let@ () = Reporter.easy_run in
    let forest = Mock_forest.init ~env ~config () |> Driver.run_action Parse_all in
    let path =
      let@ {frontmatter = {source_path; _}; _} =
        Option.bind @@ Forest.get_article (Iri.of_string "forest://my-forest/reparse") forest.resources
      in
      source_path
    in
    Alcotest.(check bool) "" true @@ Option.is_some path
  in
  let test_omits_paths () =
    let@ () = Reporter.easy_run in
    let forest = Driver.batch_run ~env ~config ~dev: false in
    let path =
      let@ {frontmatter = {source_path; _}; _} =
        Option.bind @@ Forest.get_article (Iri.of_string "forest://my-forest/index") forest.resources
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
        test_case "basic batch run" `Quick test_batch_run;
        test_case "reparsing" `Quick test_reparsing
      ];
      "dev mode",
      [
        test_case "includes paths in dev mode" `Quick test_includes_paths;
        test_case "omits paths outside dev mode" `Quick test_omits_paths;
      ]
    ]
