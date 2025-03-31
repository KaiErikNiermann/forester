(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module T = Types

open State.Syntax

let load (tree_dirs : Eio.Fs.dir_ty Eio.Path.t list) =
  Logs.debug (fun m -> m "loading trees from %i directories" (List.length tree_dirs));
  Dir_scanner.scan_directories tree_dirs
  |> Seq.map Imports.load_tree

let parse (forest : State.t) =
  let trees = forest.index |> URI.Tbl.to_seq_values |> List.of_seq in
  let results =
    let@ tree = List.filter_map @~ trees in
    match tree with
    | Document doc -> Some (Parse.parse_document ~config: forest.config doc)
    | Parsed _
    | Expanded _
    | Resource _ ->
      None
  in
  let@ result = List.partition_map @~ results in
  match result with
  | Ok t -> Right t
  | Error d -> Left d

let reparse (doc : Lsp.Text_document.t) (forest : State.t) =
  Logs.debug (fun m -> m "reparsing");
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url @@ Lsp.Text_document.documentUri doc in
  begin
    match Parse.parse_document ~config: forest.config doc with
    | Ok code ->
      forest.={uri} <- Parsed code;
      Imports.fixup code forest
    | Error d ->
      forest.?{uri} <- [d]
  end;
  forest

let build_import_graph (forest : State.t) =
  let@ () = Reporter.trace "when resolving imports" in
  let errors = ref [] in
  let push d = errors := d :: !errors in
  let new_graph =
    Reporter.run
      ~emit: push
      ~fatal: (fun d ->
        push d;
        Reporter.Tty.display d;
        forest.import_graph
      )
      @@ fun () ->
      Imports.build forest
  in
  !errors, new_graph

let expand (forest : State.t) =
  Expand.expand_tree ~forest

let expand_all (forest : State.t) =
  let diagnostics = ref [] in
  let task vertex =
    match vertex with
    | T.Content_vertex _ -> assert false
    | T.Uri_vertex uri ->
      match forest.={uri} with
      | None ->
        ()
      | Some tree ->
        match Tree.to_code tree with
        | Some tree ->
          let expanded, errors = Expand.expand_tree ~forest tree in
          diagnostics := errors @ !diagnostics;
          forest.={uri} <- Expanded expanded;
          forest.?{uri} <- errors
        | None ->
          Logs.debug (fun m -> m "expanding: no source code for %a" URI.pp uri);
          assert false;
  in
  Forest_graph.topo_iter task forest.import_graph;
  !diagnostics

let export_publication ~env ~(forest : State.t) (publication : Job.publication) : unit =
  let vertices = Forest.run_datalog_query forest.graphs publication.query in
  let resources =
    let@ vertex = List.filter_map @~ Vertex_set.elements vertices in
    match vertex with
    | Content_vertex _ -> None
    | Uri_vertex uri ->
      match forest.@{uri} with
      | None ->
        Reporter.emit Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "Attempted to export publication but tree `%a` has not yet been planted" URI.pp uri];
        None
      | Some result -> Some result
  in
  match publication.format with
  | Json_blob ->
    let blob = Repr.to_json_string ~minify: true (T.forest_t T.content_t) resources in
    let cwd = Eio.Stdenv.cwd env in
    let dir = Eio.Path.(cwd / "publications") in
    let filename = publication.name ^ ".json" in
    let path = Eio.Path.(dir / filename) in
    Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
    Eio.Path.save ~create: (`Or_truncate 0o644) path blob;
    assert (Eio.Path.is_file path)

let run_jobs (forest : State.t) jobs =
  Logs.debug (fun m -> m "Running %d jobs" (List.length jobs));
  (* All resources induced by LaTeX jobs must be planted prior to publication export. *)
  let resources_to_plant =
    let@ Range.{value; loc} = Eio.Fiber.List.filter_map ~max_fibers: 20 @~ jobs in
    let@ () = Reporter.easy_run in
    match value with
    | Job.LaTeX_to_svg job ->
      let svg = Build_latex.latex_to_svg ~env: forest.env ?loc job.source in
      let uri = Job.uri_for_latex_to_svg_job ~base: forest.config.url job in
      Some (T.Asset {uri; content = svg})
    | Job.Publish _ -> None
  in
  begin
    (* It is probably not save to plant the articles in parallel, so this is done sequentially! *)
    let@ resource = List.iter @~ resources_to_plant in
    State.plant_resource resource forest
  end;
  begin
    (* Now that the articles have been planted, we can export publications. *)
    let@ Range.{value; _} = Eio.Fiber.List.iter ~max_fibers: 20 @~ jobs in
    let@ () = Reporter.easy_run in
    match value with
    | Publish publication ->
      export_publication ~env: forest.env ~forest publication
    | Job.LaTeX_to_svg _ -> ()
  end

let eval (forest : State.t) =
  let result =
    State.get_all_unevaluated forest
    |> Seq.filter Tree.is_expanded
    |> Seq.map (fun tree ->
        let tree = Option.get @@ Tree.to_expanded tree in
        match Tree.identity_to_uri tree.identity with
        | None -> Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctext "can't evaluate a tree with no URI"]
        | Some uri ->
          let source_path =
            if forest.dev then
              URI.Tbl.find_opt forest.resolver uri
            else None
          in
          Eval.eval_tree
            ~config: forest.config
            ~source_path
            ~uri
            tree.nodes
      )
  in
  result

let eval_only (uri : URI.t) (forest : State.t) =
  match forest.={uri} with
  | None -> assert false
  | Some (Document _) -> assert false
  | Some (Parsed _) | Some (Resource _) -> assert false
  | Some (Expanded expanded) ->
    (* NOTE: Not running jobs. *)
    let Eval.{articles; jobs = _}, diagnostics =
      Eval.eval_tree
        ~config: forest.config
        ~source_path: None
        ~uri
        expanded.nodes
    in
    begin
      let@ article = List.iter @~ articles in
      State.plant_resource (Article article) forest
    end;
    forest, diagnostics

let check_status _uri (forest : State.t) =
  match forest with
  | {dependency_cache = _; _} ->
    forest, None

let implant_foreign (state : State.t) : State.t * _ =
  begin
    let foreign_paths = Eio_util.paths_of_files ~env: state.env state.config.foreign in
    Logs.debug (fun m -> m "implanting %i foreign paths" (List.length foreign_paths));
    let module EP = Eio.Path in
    let@ path = List.iter @~ foreign_paths in
    let path_str = EP.native_exn path in
    Reporter.log Format.pp_print_string (Format.sprintf "Implant foreign forest from `%s'" path_str);
    let blob = try EP.load path with _ -> Reporter.fatal IO_error ~extra_remarks: [Asai.Diagnostic.loctextf "Could not read foreign forest blob at `%s`" path_str] in
    match Repr.of_json_string (T.forest_t T.content_t) blob with
    | Ok foreign_forest ->
      List.iter
        (fun r -> State.plant_resource r state)
        foreign_forest
    | Error (`Msg err) ->
      Reporter.fatal Parse_error ~extra_remarks: [Asai.Diagnostic.loctextf "Could not parse foreign forest blob: %s" err]
    | exception exn ->
      Reporter.fatal Parse_error ~extra_remarks: [Asai.Diagnostic.loctextf "Encountered unknown error while decoding foreign forest blob: %s" (Printexc.to_string exn)]
  end;
  state, []
