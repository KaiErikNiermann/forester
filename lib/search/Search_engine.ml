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

(* let test_search_cache index = *)
(*   Reporter.profile "marshalling" @@ fun () -> *)
(*   Index.marshal index ".forester.index"; *)
(*   Reporter.profile "unmarshal" @@ fun () -> *)
(*   let index = Index.unmarshal ".forester.index" in *)
(*   Index.search ~fuzz: 1 index "forester" |> fun results -> Format.printf "got %d results for \"forester\" from marshalled index" (List.length results) *)
(**)
let test_search (forest : State.t) =
  let s = read_line () in
  let results =
    Reporter.profile "Searching" @@ fun () ->
    Index.search ~fuzz: 1 forest.search_index s
  in
  Format.printf "average doc length: %f words@." (Index.average_doc_length forest.search_index);
  Format.printf "got %i results@." (List.length results);
  List.iter
    (fun (locations, iri) ->
      match Forest.get_article iri forest.resources with
      | Some article ->
        Format.printf "%a@." pp_iri iri;
        List.iter
          (fun path ->
            Format.(
              printf
                "@[<1>%a@ = %s@]@."
                (
                  pp_print_list
                    ~pp_sep: (fun out () -> fprintf out "; ")
                    pp_print_int
                )
                path
                (
                  Context.render_context_article path article
                )
            )
          )
          locations
      | None -> assert false
    )
    results

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
