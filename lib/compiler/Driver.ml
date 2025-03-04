(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* TODO:
   - Come up with error hadling/merging strategy
   *)

open Forester_core
open Forester_prelude

type target = HTML | JSON | XML | STRING

module T = Types

type state = State.t

module Action = struct
  type exit =
    Fail | Finished
  [@@deriving show]

  type t =
    | Quit of exit
    | Build_import_graph
    | Build_dependency_graph of iri
    | Plant_assets
    | Plant_foreign
    | Done
    | Load_all_configured_dirs
    | Parse_all
    | Expand_all
    | Eval_all
    | Expand_only of iri
    | Eval_only of iri
    | Parse of iri
    | Query of (string, Vertex.t) Datalog_expr.query
    | Cache_results of (Vertex_set.t [@opaque])
    | Run_jobs of Job.job Range.located list
  [@@deriving show]
end

module Trace = Algaeff.Sequencer.Make(Action)

let update
  : Action.t -> State.t -> Action.t * State.t
= fun action forest ->
  let open Action in
  match action with
  | Quit e ->
    begin
      (* TODO: Graciously exit, i.e. persist cache *)
      match e with
      | Fail -> exit 1
      | Finished -> exit 0
    end
  | Query q ->
    let@ () = Reporter.trace "when running query" in
    let r = Forest.run_datalog_query forest.graphs q in
    Cache_results r, forest
  | Load_all_configured_dirs ->
    let@ () = Reporter.trace "when loading files from disk" in
    let tree_dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
    List.iter (fun path -> assert (Eio.Path.is_directory path)) tree_dirs;
    let docs = Phases.load tree_dirs in
    docs
    |> Seq.iter (fun doc ->
        let uri = Lsp.Text_document.documentUri doc in
        let path = Lsp.Uri.to_path uri in
        let iri = Iri_scheme.uri_to_iri ~host: forest.config.host uri in
        Iri_tbl.replace forest.resolver iri path;
        Hashtbl.replace forest.documents uri doc
      );
    Logs.debug (fun m -> m "loaded %d trees" (Seq.length docs));
    (* Logs.debug (fun m -> m "%d documents" (Hashtbl.length forest.documents)); *)
    Parse_all, forest
  | Parse_all ->
    let@ () = Reporter.trace "when parsing trees" in
    let errors, trees =
      Phases.parse forest
      |> Seq.partition_map (fun res ->
          match res with
          | Ok tree -> Right tree
          | Error err -> Left err
        )
    in
    Logs.debug (fun m -> m "parsed %d trees" (Seq.length trees));
    assert (Seq.length trees <= Hashtbl.length forest.documents);
    assert (Seq.length trees <= Iri_tbl.length forest.resolver);
    Seq.iter
      (fun _ ->
        (*TODO: *)
        ()
      )
      errors;
    Seq.iter
      (fun tree ->
        (* Every tree that comes from the filesystem has an IRI *)
        let iri = Option.get Code.(tree.iri) in
        Iri_tbl.add
          forest.parsed
          iri
          tree
      )
      trees;
    Build_import_graph, forest
  | Build_import_graph ->
    let@ () = Reporter.trace "when building import graph" in
    let import_graph, diagnostics = Phases.build_import_graph forest in
    assert (Forest_graph.nb_vertex import_graph >= Iri_tbl.length forest.parsed);
    (* Logs.debug (fun m -> m "%d vertices@." (Forest_graph.nb_vertex import_graph)); *)
    Forest_graph.iter_vertex (fun v -> Logs.debug (fun m -> m "vertex %a" T.(pp_vertex pp_content) v)) import_graph;
    Diagnostic_store.iter (fun _ d -> Diagnostic_store.add forest.diagnostics d) diagnostics;
    Expand_all, {forest with import_graph}
  | Build_dependency_graph iri ->
    let@ () = Reporter.tracef "when building dependency graph for %a" pp_iri iri in
    let import_graph = Phases.build_import_graph_for ~iri forest in
    Expand_only iri, {forest with import_graph}
  | Expand_all ->
    let@ () = Reporter.tracef "when expanding trees" in
    let units, expanded_trees = Phases.expand forest in
    Logs.debug (fun m -> m "Expand: got %d trees" (List.length expanded_trees));
    assert (List.length expanded_trees >= Iri_tbl.length forest.parsed);
    begin
      let@ (diagnostics, source_path, (syn : Syn.tree)) = List.iter @~ expanded_trees in
      let@ iri = Option.iter @~ syn.iri in
      Forest.replace forest.expanded iri syn.syn;
      match source_path with
      | None ->
        Logs.warn (fun m -> m "tree at %a has no source path." pp_iri iri);
        (* There is an implicit assumption here that diagnostics are only
            emitted for trees that have a source path. I should think about
            this.*)
        if forest.dev then assert false
      | Some path ->
        assert (not (Filename.is_relative path));
        match diagnostics with
        | [] -> ()
        | diagnostics ->
          (* If we obtained some diagnostics from expanding, we have
          succesfully parsed the tree and can thus overwrite the parsing
          diagnostics *)
          Diagnostic_store.replace forest.diagnostics diagnostics
    end;
    let patched_units =
      let open Expand in
      Unit_map.fold
        (fun iri exports acc ->
          match Unit_map.find_opt iri units with
          | None ->
            Unit_map.add iri exports acc
          | Some _ ->
            Unit_map.add iri exports acc
        )
        units
        units
    in
    Eval_all, {forest with units = patched_units}
  | Expand_only iri ->
    let@ () = Reporter.tracef "when expanding %a" pp_iri iri in
    let result, _err = Phases.expand_only iri forest in
    Eval_only iri, result
  | Eval_all ->
    let@ () = Reporter.tracef "when evaluating" in
    let trees, _errors = Phases.eval forest in
    Logs.debug (fun m -> m "Eval: got %d trees" (Seq.length trees));
    let jobs =
      trees
      |> List.of_seq
      |> List.concat_map
          (fun Eval.{articles; jobs} ->
            begin
              let@ article = List.iter @~ articles in
              Forest.plant_resource (T.Article article) forest.graphs forest.resources
            end;
            jobs
          )
    in
    Run_jobs jobs, forest
  | Eval_only iri ->
    let@ () = Reporter.tracef "when evaluating %a" pp_iri iri in
    let result, _err = Phases.eval_only iri forest in
    Done, result
  | Plant_assets ->
    let@ () = Reporter.tracef "when planting assets" in
    let paths =
      Dir_scanner.scan_asset_directories
        (Eio_util.paths_of_dirs ~env: forest.env forest.config.assets)
    in
    Logs.debug (fun m -> m "planting %i assets" (Seq.length paths));
    let module EP = Eio.Path in
    begin
      let@ path = Eio.Fiber.List.iter ~max_fibers: 20 @~ List.of_seq paths in
      let content = EP.load path in
      let source_path = EP.native_exn path in
      let iri = Asset_router.install ~host: forest.config.host ~source_path ~content in
      Logs.debug (fun m -> m "Installed %s at %a" source_path pp_iri iri);
      Forest.plant_resource (T.Asset {iri; host = forest.config.host; content}) forest.graphs forest.resources;
    end;
    Done, forest
  | Plant_foreign ->
    let@ () = Reporter.tracef "when planting foreign forest" in
    let result, err = Phases.implant_foreign forest in
    let _ = Option.map Reporter.Tty.display err in
    Done, result
  | Run_jobs jobs ->
    Phases.run_jobs forest jobs;
    Done, forest
  | Parse _
  | Cache_results _
  | Done ->
    Done, forest

let run_action a s : state =
  let rec go action state =
    match update action state with
    | new_action, new_state ->
      if action = Done then new_state
      else
        begin
          let fatal d = Reporter.Tty.display d; new_state in
          let@ () = Reporter.try_with ~emit: Reporter.Tty.display ~fatal in
          go new_action new_state
        end
  in
  go a s

let run_with_history a s =
  let history = ref [] in
  let rec go action state =
    history := action :: !history;
    match update action state with
    | new_action, new_state ->
      if action = Done then new_state
      else
        begin
          let fatal d = Reporter.Tty.display d; new_state in
          let@ () = Reporter.try_with ~emit: Reporter.Tty.display ~fatal in
          go new_action new_state
        end
  in
  let forest = go a s in
  forest, List.rev !history

let rec force
  : Action.t list -> state -> unit
= fun msgs state ->
  match msgs with
  | [] -> ()
  | msg :: remaining ->
    let _discard, new_state = update msg state in
    force remaining new_state

let implant_foreign = run_action Plant_foreign

let plant_assets = run_action Plant_assets

let batch_run ~env ~(config : Config.t) ~dev =
  State.make ~env ~config ~dev ()
  |> plant_assets
  |> implant_foreign
  |> run_action Load_all_configured_dirs

let language_server
  : state -> unit
= fun _ -> ()
