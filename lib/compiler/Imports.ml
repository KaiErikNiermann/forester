(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open struct module T = Types end

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

(* Only add edge if both vertices are already present*)
let add_edge g v w =
  try
    assert (Forest_graph.mem_vertex g v);
    assert (Forest_graph.mem_vertex g w);
    Forest_graph.add_edge g v w
  with
    | exn -> Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "%a" Eio.Exn.pp exn]

module Analysis_env = Algaeff.Reader.Make(struct type t = analysis_env end)

let resolve_uri_to_code
    (forest : State.t)
    (uri : URI.t)
    : Tree.code option
  =
  let dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
  match Forest.find_opt forest.index uri with
  | Some tree -> Tree.to_code tree
  | None ->
    match URI.Tbl.find_opt forest.resolver uri with
    | Some path ->
      let doc = load_tree Eio.Path.(forest.env#fs / path) in
      Result.to_option @@
        Parse.parse_document ~config: forest.config doc
    | None ->
      match Dir_scanner.find_tree dirs uri with
      | Some path ->
        let native = Eio.Path.native_exn path in
        URI.Tbl.add forest.resolver uri native;
        let doc = load_tree path in
        Result.to_option @@
          Parse.parse_document ~config: forest.config doc
      | None ->
        Reporter.fatal (Resource_not_found uri)

let rec analyse_tree (tree : Tree.code) =
  let env = Analysis_env.read () in
  let@ root = Option.iter @~ Tree.identity_to_uri tree.identity in
  let code = tree.nodes in
  Forest_graph.add_vertex env.graph (T.Uri_vertex root);
  analyse_code ~root code;

and analyse_code ~root (code : Code.t) =
  List.iter (analyse_node ~root) code

and analyse_node ~root (node : Code.node Asai.Range.located) =
  let env = Analysis_env.read () in
  let config = env.forest.config in
  match node.value with
  | Import (_, dep) ->
    let dep_uri = URI_scheme.named_uri ~base: config.url dep in
    let dependency = T.Uri_vertex dep_uri in
    let target = T.Uri_vertex root in
    Forest_graph.add_vertex env.graph dependency;
    (* add_vertex env.forest env.graph tar; *)
    add_edge env.graph dependency target;
    if env.follow then
      begin
        match resolve_uri_to_code env.forest dep_uri with
        | None -> Reporter.fatal ?loc: node.loc (Resource_not_found dep_uri)
        | Some code ->
          analyse_tree code;
          assert false
      end
  | Subtree (addr, nodes) ->
    let identity =
      match addr with
      | None -> Tree.Anonymous
      | Some string ->
        URI (URI_scheme.named_uri ~base: config.url string)
    in
    analyse_tree
      {identity; origin = Subtree {parent = URI root}; nodes; timestamp = None;}
  | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
    analyse_code ~root code
  | Object {methods; _} | Patch {methods; _} ->
    let@ _, code = List.iter @~ methods in
    analyse_code ~root code
  | Dx_prop (rel, args) ->
    analyse_code ~root rel;
    List.iter (analyse_code ~root) args
  | Dx_sequent (concl, premises) ->
    analyse_code ~root concl;
    List.iter (analyse_code ~root) premises
  | Dx_query (_, positives, negatives) ->
    List.iter (analyse_code ~root) positives;
    List.iter (analyse_code ~root) negatives
  | Text _ | Hash_ident _ | Xml_ident (_, _) | Verbatim _ | Ident _ | Open _ | Put (_, _) | Default (_, _) | Get _ | Decl_xmlns (_, _) | Call (_, _) | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_uri _ | Comment _ | Error _ -> ()

let dependencies tree forest =
  let env = {forest; follow = true; graph = Forest_graph.create ()} in
  let@ () = Analysis_env.run ~env in
  analyse_tree tree;
  env.graph

let fixup (tree : Tree.code) (forest : State.t) =
  let@ () = Reporter.tracef "when updating imports for %a" Tree.pp_identity tree.identity in
  Logs.debug (fun m -> m "updating imports for %a" Tree.pp_identity tree.identity);
  let graph = forest.import_graph in
  match tree.identity with
  | Tree.Anonymous -> assert false
  | Tree.URI uri ->
    let this_vertex = T.Uri_vertex uri in
    let old_deps = Vertex_set.of_list @@ Forest_graph.immediate_dependencies graph this_vertex in
    let new_deps =
      let env = {forest; follow = false; graph;} in
      let@ () = Analysis_env.run ~env in
      begin
        analyse_tree tree;
        Vertex_set.of_list @@ Forest_graph.immediate_dependencies env.graph this_vertex
      end;
    in
    let unchanged_deps = Vertex_set.inter new_deps old_deps in
    let added_deps = Vertex_set.diff new_deps unchanged_deps in
    let removed_deps = Vertex_set.diff old_deps unchanged_deps in
    Logs.debug (fun m -> m "added %d dependencies" (Vertex_set.cardinal added_deps));
    Logs.debug (fun m -> m "removed %d dependencies" (Vertex_set.cardinal removed_deps));
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
  f (T.Uri_vertex addr);
  dep_graph

let build forest =
  let env = {forest; follow = false; graph = Forest_graph.create ()} in
  let@ () = Analysis_env.run ~env in
  env.forest
  |> State.get_all_code
  |> Seq.iter analyse_tree;
  env.graph
