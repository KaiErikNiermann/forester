(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module T = Types

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

let load (tree_dirs : Eio.Fs.dir_ty Eio.Path.t list) =
  Logs.debug (fun m -> m "loading trees from %i directories" (List.length tree_dirs));
  Dir_scanner.scan_directories tree_dirs
  |> Seq.map Imports.load_tree

let parse (forest : State.t) =
  let host = forest.config.host in
  forest.documents
  |> Hashtbl.to_seq
  |> Seq.map @@ fun (lsp_uri, doc) ->
    let uri = URI_scheme.lsp_uri_to_uri ~host lsp_uri in
    let source_path = Lsp.Uri.to_path lsp_uri in
    let@ () = Reporter.tracef "when parsing %a (%s)" URI.pp uri source_path in
    Parse.parse_document ~host doc

(* Fix signature. Should not mutate the forest*)
let reparse (doc : Lsp.Text_document.t) : State.t -> State.t * diagnostic option = fun forest ->
  Logs.debug (fun m -> m "reparsing");
  let host = forest.config.host in
  let uri = Lsp.Text_document.documentUri doc in
  match Parse.parse_document ~host doc with
  | Ok tree ->
    Forest.replace forest.parsed (URI_scheme.lsp_uri_to_uri ~host uri) tree;
    Imports.fixup tree forest;
    Diagnostic_store.remove forest.diagnostics uri;
    forest, None
  | Error d ->
    (* When we get a parsing diagnostic, we don't need to merge the value
       with the diagnostics from previous passes*)
    Diagnostic_store.replace forest.diagnostics [d];
    forest, None

let build_import_graph (forest : State.t) =
  (* Now that I am adding caching, is this function still correct?*)
  let@ () = Reporter.trace "when resolving imports" in
  let diagnostics = Diagnostic_store.create 100 in
  let push d = Diagnostic_store.add diagnostics [d] in
  let new_graph =
    Reporter.run
      ~emit: push
      ~fatal: (fun d -> push d; forest.import_graph)
      @@ fun () ->
      Imports.build forest
  in
  new_graph, diagnostics

let expand (forest : State.t) =
  (* This should only return the units for trees that have actually changed.
     Then the driver can just merge the maps without checking anything.*)
  let parsed = forest.parsed in
  let module Graphs = (val forest.graphs) in
  let task (addr : Vertex.t) (units, trees) =
    match addr with
    | T.Content_vertex _ ->
      (* when creating the import graph we are only adding uri vertices *)
      assert false
    | T.Uri_vertex uri ->
      match Forest.find_opt parsed uri with
      | None ->
        (* The import graph has subtrees for vertices, which do not correspond to a source file *)
        units, trees
      | Some tree ->
        let diagnostics, units, syn =
          Expand.expand_tree
            ~host: forest.config.host
            units
            tree
        in
        let source_path = tree.source_path in
        units, (diagnostics, source_path, syn) :: trees
  in
  let units, expanded_trees =
    Forest_graph.topo_fold
      task
      forest.import_graph
      (Expand.Env.empty, [])
  in
  units, expanded_trees

(* There is some duplicated code here. The only significant difference is the
   fact that we are using a different graph builder, one that only traverses
   the dependencies of a specific tree*)
let expand_only_aux ~(addr : URI.t) (forest : State.t) : Expand.Env.t * Diagnostic_store.t * Syn.t Forest.t =
  let import_graph = Forest_graph.dependencies forest.import_graph (T.Uri_vertex addr) in
  assert (Forest_graph.nb_vertex import_graph >= Forest.length forest.parsed);
  let task (addr : Vertex.t) (units, diagnostics, (trees : (Syn.t URI.Tbl.t))) =
    match addr with
    | T.Content_vertex _ ->
      (* when creating the import graph we are only adding uri vertices *)
      assert false
    | T.Uri_vertex uri ->
      match Imports.resolve_uri_to_code forest uri with
      | None ->
        (* The import graph has vertices for subtrees, which do not correspond to a source file *)
        Logs.debug (fun m -> m "failed to resolve %a" URI.pp uri);
        units, diagnostics, trees
      | Some (tree, _) ->
        let ds, units, syn = Expand.expand_tree ~host: forest.config.host units tree in
        begin
          Forest.add trees uri syn.syn;
          match ds with
          | [] -> ()
          | ds -> Diagnostic_store.add diagnostics ds
        end;
        units, diagnostics, trees
  in
  let units, diagnostics, expanded_trees =
    Logs.debug (fun m -> m "Folding the import graph with %i vertices" (Forest_graph.nb_vertex import_graph));
    Forest_graph.topo_fold
      task
      import_graph
      (Expand.Env.empty, Diagnostic_store.create 100, Forest.create 1000)
  in
  Logs.debug (fun m -> m "Got %i expanded trees" (Forest.length expanded_trees));
  units, diagnostics, expanded_trees

(* The purpose of this function is to update the exported units when a tree has
   been changed when running with the lsp.*)
let expand_only (uri : URI.t) : State.t -> State.t * diagnostic option = fun forest ->
  let units, new_diagnostics, trees =
    expand_only_aux
      ~addr: uri
      forest
  in
  assert (Forest.length trees > 0);
  begin
    trees
    |> Forest.iter @@ fun uri tree ->
      Forest.replace forest.expanded uri tree;
      match URI.Tbl.find_opt forest.resolver uri with
      | None ->
        Logs.debug (fun m -> m "resolver knows about %i paths" (URI.Tbl.length forest.resolver));
        assert false
      | Some path ->
        Logs.debug (fun m -> m "clearing diagnostics for %s" path);
        Diagnostic_store.remove forest.diagnostics (Lsp.Uri.of_path path)
  end;
  begin
    new_diagnostics
    |> Diagnostic_store.iter @@ fun uri diagnostics ->
      Logs.debug (fun m -> m "%s got some expansion diagnostics." (Lsp.Uri.to_string uri));
      match diagnostics with
      | [] -> ()
      | diagnostics ->
        Diagnostic_store.add forest.diagnostics diagnostics
  end;
  (*FIXME: Don't replace all units. Just update the ones that have changed!*)
  {forest with units}, None

let export_publication ~env ~(forest : State.t) (publication : Job.publication) : unit =
  let vertices = Forest.run_datalog_query forest.graphs publication.query in
  let resources =
    let@ vertex = List.filter_map @~ Vertex_set.elements vertices in
    match vertex with
    | T.Content_vertex _ -> None
    | T.Uri_vertex uri ->
      match Forest.find_opt forest.resources uri with
      | None ->
        Reporter.emitf Internal_error "Attempted to export publication but tree `%a` has not yet been planted" URI.pp uri;
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
  (* All articles induced by LaTeX jobs must be planted prior to publication export. *)
  let articles_to_plant =
    let@ Range.{value; loc} = Eio.Fiber.List.filter_map ~max_fibers: 20 @~ jobs in
    match value with
    | Job.LaTeX_to_svg job ->
      let@ () = Reporter.easy_run in
      let svg = Build_latex.latex_to_svg ~env: forest.env ?loc job.source in
      let uri = URI_scheme.hash_uri ~host: forest.config.host job.hash in
      let frontmatter = T.default_frontmatter ~uri () in
      let mainmatter = job.content ~svg in
      let backmatter = T.Content [] in
      Some T.{frontmatter; mainmatter; backmatter}
    | Job.Publish _ -> None
  in
  begin
    (* It is probably not save to plant the articles in parallel, so this is done sequentially! *)
    let@ article = List.iter @~ articles_to_plant in
    Forest.plant_resource (T.Article article) forest.graphs forest.resources
  end;
  begin
    (* Now that the articles have been planted, we can export publications. *)
    let@ Range.{value; _} = Eio.Fiber.List.iter ~max_fibers: 20 @~ jobs in
    match value with
    | Publish publication ->
      export_publication ~env: forest.env ~forest publication
    | Job.LaTeX_to_svg _ -> ()
  end

let eval (forest : State.t) =
  let host = forest.config.host in
  let trees, diagnostics =
    URI.Tbl.to_seq forest.expanded
    |> Seq.map (fun (uri, syn) ->
        let source_path = if forest.dev then URI.Tbl.find_opt forest.resolver uri else None in
        Eval.eval_tree
          ~host
          ~source_path
          ~uri
          syn
      )
    |> Seq.split
  in
  let diags =
    match List.of_seq diagnostics with
    | [] -> None
    | hd :: rest ->
      List.iter
        (fun tbl ->
          Hashtbl.to_seq tbl
          |> Seq.iter (fun (uri, diags) ->
              assert (not @@ Hashtbl.mem hd uri);
              Hashtbl.add hd uri diags
            )
        )
        rest;
      Some hd
  in
  trees, diags

let eval_only
  (uri : URI.t)
  : State.t -> State.t * diagnostic option
= fun forest ->
  match Forest.find_opt forest.expanded uri with
  | None -> assert false
  | Some syn ->
    (* NOTE: Not running jobs. *)
    let Eval.{articles; jobs = _}, _diagnostics =
      Eval.eval_tree
        ~host: forest.config.host
        ~source_path: None
        ~uri
        syn
    in
    begin
      let@ article = List.iter @~ articles in
      Forest.plant_resource (Article article) forest.graphs forest.resources
    end;
    (* Diagnostic_store.add forest.diagnostics diagnostics; *)
    forest, None

let check_status
  _uri
  : State.t -> State.t * diagnostic option
= fun state ->
  match state with
  | {dependency_cache = _;
    _;
  } ->
    state, None

let implant_foreign
  : State.t -> State.t * diagnostic option
= fun state ->
  begin
    let foreign_paths = Eio_util.paths_of_files ~env: state.env state.config.foreign in
    Logs.debug (fun m -> m "implanting %i foreign paths" (List.length foreign_paths));
    let module EP = Eio.Path in
    let@ path = List.iter @~ foreign_paths in
    let path_str = EP.native_exn path in
    Reporter.log Format.pp_print_string (Format.sprintf "Implant foreign forest from `%s'" path_str);
    let blob = try EP.load path with _ -> Reporter.fatalf IO_error "Could not read foreign forest blob at `%s`" path_str in
    match Repr.of_json_string (T.forest_t T.content_t) blob with
    | Ok foreign_forest ->
      List.iter (fun r -> Forest.plant_resource r state.graphs state.resources) foreign_forest
    | Error (`Msg err) ->
      Reporter.fatalf Parse_error "Could not parse foreign forest blob: %s" err
    | exception exn ->
      Reporter.fatalf Parse_error "Encountered unknown error while decoding foreign forest blob: %s" (Printexc.to_string exn)
  end;
  state, None
