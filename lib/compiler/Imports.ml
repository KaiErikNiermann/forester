(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
module T = Types

type analysis_env = {
  follow: bool;
  forest: State.t; (* We don't touch the import graph in here.*)
  graph: Forest_graph.t;
}

let load_tree path =
  let content = Eio.Path.load path in
  let path_str = Eio.Path.native_exn path in
  assert (not @@ Filename.is_relative path_str);
  let uri = Lsp.Uri.of_path path_str in
  Lsp.Text_document.make
    ~position_encoding: `UTF8
    {
      textDocument = {
        languageId = "forester";
        text = content;
        uri;
        version = 1
      }
    }

let _add_vertex (forest : State.t) g v =
  try
    let iri_v = Option.get (Vertex.iri_of_vertex v) in
    assert (URI.Tbl.mem forest.parsed iri_v);
    Forest_graph.add_vertex g v
  with
    | exn -> Reporter.fatalf Internal_error "%a" Eio.Exn.pp exn

(* Only add edge if both vertices are already present*)
let add_edge g v w =
  try
    assert (Forest_graph.mem_vertex g v);
    assert (Forest_graph.mem_vertex g w);
    Forest_graph.add_edge g v w
  with
    | exn -> Reporter.fatalf Internal_error "%a" Eio.Exn.pp exn

let register_document ~host g doc =
  let uri = Lsp.Text_document.documentUri doc in
  let iri = URI_scheme.lsp_uri_to_iri ~host uri in
  Forest_graph.add_vertex g (T.Iri_vertex iri)

module Analysis_env = Algaeff.Reader.Make(struct type t = analysis_env end)

let resolve_iri_to_code (forest : State.t) iri =
  let dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
  match Forest.find_opt forest.parsed iri, URI.Tbl.find_opt forest.resolver iri with
  | Some tree, Some path ->
    let doc = load_tree Eio.Path.(forest.env#fs / path) in
    Some (tree, doc)
  | None, Some path ->
    begin
      let doc = load_tree Eio.Path.(forest.env#fs / path) in
      match Parse.parse_document ~host: forest.config.host doc with
      | Ok code ->
        Some (code, doc)
      | Error _ -> None
    end
  (* TODO: Think about these cases *)
  | Some _, None -> None
  | None, _ ->
    begin
      match Dir_scanner.find_tree dirs iri with
      | Some path ->
        let native = Eio.Path.native_exn path in
        URI.Tbl.add forest.resolver iri native;
        begin
          let doc = load_tree path in
          match Parse.parse_document ~host: forest.config.host doc with
          | Ok code ->
            Some (code, doc)
          | Error _ -> None
        end
      | None -> None
    end

let rec analyse_tree (root : URI.t) (tree : Code.tree) =
  let env = Analysis_env.read () in
  let iri_opt = tree.iri in
  let code = tree.code in
  let@ iri = Option.iter @~ iri_opt in
  Forest_graph.add_vertex env.graph (T.Iri_vertex iri);
  analyse_code root code;

and analyse_tree_exn (tree : Code.tree) =
  match tree.iri with
  | Some iri -> analyse_tree iri tree
  | None -> Reporter.fatalf Internal_error "Import graph: cannot analyse a tree without an address"

and analyse_code root (code : Code.t) =
  List.iter (analyse_node root) code

and analyse_node root (node : Code.node Asai.Range.located) =
  let env = Analysis_env.read () in
  let host = env.forest.config.host in
  match node.value with
  | Import (_, dep) ->
    (* NOTE: Doesn't this imply we can't import like \import{forest://foo/bar}?*)
    let dep_iri = URI_scheme.user_iri ~host dep in
    let dependency = T.Iri_vertex dep_iri in
    let target = T.Iri_vertex root in
    begin
      match resolve_iri_to_code env.forest dep_iri with
      | None ->
        Reporter.fatalf
          ?loc: node.loc
          Resource_not_found
          "could not find tree %a"
          URI.pp
          dep_iri
      | Some (tree, doc) ->
        register_document ~host: env.forest.config.host env.graph doc;
        add_edge env.graph dependency target;
        analyse_tree root tree
      (* | Some (_tree, None) -> *)
      (*   (* TODO: *) *)
      (*   Reporter.fatalf ?loc: node.loc Resource_not_found "could not find tree %a" URI.pp dep_iri *)
    end
  | Subtree (addr, code) ->
    let iri = Option.map (URI_scheme.user_iri ~host) addr in
    analyse_tree
      root
      (* Consider using the env to keep track of the current source path *)
      (* FIXME: not passing timestamp of parent tree. Need to modify Analysis_env for that *)
      {iri; code; source_path = None; timestamp = None;}
  | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
    analyse_code root code
  | Object {methods; _} | Patch {methods; _} ->
    let@ _, code = List.iter @~ methods in
    analyse_code root code
  | Dx_prop (rel, args) ->
    analyse_code root rel;
    List.iter (analyse_code root) args
  | Dx_sequent (concl, premises) ->
    analyse_code root concl;
    List.iter (analyse_code root) premises
  | Dx_query (_, positives, negatives) ->
    List.iter (analyse_code root) positives;
    List.iter (analyse_code root) negatives
  | Text _ | Hash_ident _ | Xml_ident (_, _) | Verbatim _ | Ident _ | Open _ | Put (_, _) | Default (_, _) | Get _ | Decl_xmlns (_, _) | Call (_, _) | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_iri _ | Comment _ | Error _ -> ()

let dependencies tree forest =
  let env = {forest; follow = true; graph = Forest_graph.create ()} in
  let@ () = Analysis_env.run ~env in
  analyse_tree_exn tree;
  env.graph

let fixup (tree : Code.tree) (forest : State.t) =
  let@ () = Reporter.tracef "when resolving imports" in
  let graph = forest.import_graph in
  let this_iri = Option.get tree.iri in
  let this_vertex = T.Iri_vertex this_iri in
  let old_deps = Vertex_set.of_list @@ Forest_graph.immediate_dependencies graph this_vertex in
  let new_deps =
    let env = {
      forest;
      follow = false;
      graph;
    }
    in
    let@ () = Analysis_env.run ~env in
    begin
      analyse_tree_exn tree;
      Vertex_set.of_list @@ Forest_graph.immediate_dependencies env.graph this_vertex
    end;
  in
  let unchanged_deps = Vertex_set.inter new_deps old_deps in
  let added_deps = Vertex_set.diff new_deps unchanged_deps in
  let removed_deps = Vertex_set.diff old_deps unchanged_deps in
  Vertex_set.iter (fun v -> Forest_graph.remove_edge graph v this_vertex) removed_deps;
  Vertex_set.iter (fun v -> Forest_graph.add_edge graph v this_vertex) added_deps

let _minimal_dependency_graph
  : addr: URI.t -> Forest_graph.t
= fun ~addr ->
  let dep_graph = Forest_graph.create () in
  let rec f v =
    Forest_graph.iter_succ
      (fun w -> Forest_graph.add_edge dep_graph v w; f w)
      dep_graph
      v
  in
  f (T.Iri_vertex addr);
  dep_graph

let run_builder ?root env =
  let@ () = Analysis_env.run ~env in
  begin
    match root with
    | Some iri ->
      begin
        match resolve_iri_to_code env.forest iri with
        | None ->
          let@ () = Reporter.trace "when building import graph" in
          Reporter.fatalf Resource_not_found "could not find tree `%a'" URI.pp iri
        | Some (tree, _) -> analyse_tree_exn tree
      end
    | None ->
      env.forest.parsed
      |> Forest.to_seq_values
      |> Seq.iter (analyse_tree_exn)
  end;
  env.graph

let build forest =
  let env = {forest; follow = false; graph = Forest_graph.create ()} in
  run_builder env
