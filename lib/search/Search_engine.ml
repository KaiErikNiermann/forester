(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_search
open Forester_frontend
open Cmdliner
open Cmdliner.Term.Syntax

let test_ranked (forest : State.t) =
  let ranked_results =
    Reporter.profile "Ranked search" @@ fun () ->
    Index.ranked_search
      ~fuzz: 2
      forest.search_index
      forest.resources
      "hyprtext format"
  in
  Format.printf "got %i ranked results.@." (List.length ranked_results);
  List.iter
    (fun (iri, score) ->
      match Forest.get_article iri forest.resources with
      | Some article ->
        Format.printf "%a, %f@." pp_iri iri score;
      | None -> assert false
    )
    ranked_results

let test_search (forest : State.t) =
  let s = read_line () in
  let results =
    (* Reporter.profile "Searching" @@ fun () -> *)
    Index.search ~fuzz: 1 forest.search_index s
  in
  Format.printf "got %i results@." (List.length results)

let main ~env () =
  let config = Config_parser.parse_forest_config_file "forest.toml" in
  let dev = true in
  let forest = Driver.batch_run ~env ~dev ~config in
  let articles = Forest.get_all_articles forest.resources in
  let index =
    (* Reporter.profile "Building index" @@ fun () -> *)
    Index.create articles
  in
  let size = Obj.reachable_words @@ Obj.repr index in
  Format.printf "index size: %i@." size;
  let forest = {forest with search_index = index} in
  test_search forest
(*   test_search_cache forest.search_index *)
(*   test_ranked forest; *)

let () =
  let@ env = Eio_main.run in
  let@ () = Forester_core.Reporter.easy_run in
  main ~env ()
