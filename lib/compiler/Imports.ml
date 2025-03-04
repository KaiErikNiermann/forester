(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude

(* Think hard about the usage of imperative graphs here. *)

module T = Types

type analysis_env = {
  follow: bool;
  forest: State.t;
}

module Analysis_env = Algaeff.Reader.Make(struct type t = analysis_env end)

let resolve_iri_to_code iri (forest : State.t) =
  let dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
  match Forest.find_opt forest.parsed iri with
  | None ->
    begin
      match Dir_scanner.find_tree dirs iri with
      | Some path ->
        Iri_tbl.add forest.resolver iri path;
        begin
          match Parse.parse_file path with
          | Ok code ->
            let timestamp = Eio.Path.(stat ~follow: true @@ forest.env#fs / path).mtime in
            Some Code.{code; iri = Some iri; source_path = Some path; timestamp = Some timestamp}
          | Error _ -> None
        end
      | None -> None
    end
  | Some tree -> Some tree

let rec analyse_tree roots (tree : Code.tree) =
  let env = Analysis_env.read () in
  let iri_opt = tree.iri in
  let code = tree.code in
  let roots =
    Option.fold
      ~none: roots
      ~some: (fun x -> x :: roots
      )
      iri_opt
  in
  analyse_code roots code;
  let@ iri = Option.iter @~ iri_opt in
  Forest_graph.add_vertex env.forest.import_graph (T.Iri_vertex iri)

and analyse_code roots (code : Code.t) =
  List.iter (analyse_node roots) code

and analyse_node roots (node : Code.node Asai.Range.located) =
  let env = Analysis_env.read () in
  let host = env.forest.config.host in
  match node.value with
  | Import (_, dep) ->
    let dep_iri = Iri_scheme.user_iri ~host dep in
    let dependency = T.Iri_vertex dep_iri in
    let@ iri = List.iter @~ roots in
    let target = T.Iri_vertex iri in
    Forest_graph.add_edge_exn env.forest.import_graph dependency target;
    begin
      if env.follow then
        match resolve_iri_to_code dep_iri env.forest with
        | None -> assert false
        | Some tree -> analyse_tree [] tree
      else ()
    end
  | Subtree (addr, code) ->
    let iri = Option.map (Iri_scheme.user_iri ~host) addr in
    analyse_tree
      roots
      (* Consider using the env to keep track of the current source path *)
      (* FIXME: not passing timestamp of parent tree. Need to modify Analysis_env for that *)
      {iri; code; source_path = None; timestamp = None;}
  | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
    analyse_code roots code
  | Object {methods; _} | Patch {methods; _} ->
    let@ _, code = List.iter @~ methods in
    analyse_code roots code
  | Dx_prop (rel, args) ->
    analyse_code roots rel;
    List.iter (analyse_code roots) args
  | Dx_sequent (concl, premises) ->
    analyse_code roots concl;
    List.iter (analyse_code roots) premises
  | Dx_query (_, positives, negatives) ->
    List.iter (analyse_code roots) positives;
    List.iter (analyse_code roots) negatives
  | Text _ | Hash_ident _ | Xml_ident (_, _) | Verbatim _ | Ident _ | Open _ | Put (_, _) | Default (_, _) | Get _ | Decl_xmlns (_, _) | Call (_, _) | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_iri _ | Comment _ | Error _ -> ()

let dependencies tree forest =
  let env = {forest; follow = true} in
  let@ () = Analysis_env.run ~env in
  analyse_tree [] tree;
  env.forest.import_graph

let fixup (tree : Code.tree) (forest : State.t) =
  let graph = forest.import_graph in
  let this_iri = Option.get tree.iri in
  let this_vertex = T.Iri_vertex this_iri in
  let old_deps = Vertex_set.of_list @@ Forest_graph.immediate_dependencies graph this_vertex in
  let new_deps =
    let env = {
      forest;
      follow = false;
    }
    in
    let@ () = Analysis_env.run ~env in
    begin
      analyse_tree [] tree;
      Vertex_set.of_list @@ Forest_graph.immediate_dependencies env.forest.import_graph this_vertex
    end;
  in
  let unchanged_deps = Vertex_set.inter new_deps old_deps in
  let added_deps = Vertex_set.diff new_deps unchanged_deps in
  let removed_deps = Vertex_set.diff old_deps unchanged_deps in
  Vertex_set.iter (fun v -> Forest_graph.remove_edge graph v this_vertex) removed_deps;
  Vertex_set.iter (fun v -> Forest_graph.add_edge graph v this_vertex) added_deps

let _minimal_dependency_graph
  : addr: iri -> Forest_graph.t
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
        match resolve_iri_to_code iri env.forest with
        | None ->
          let@ () = Reporter.trace "when building import graph" in
          Reporter.fatalf
            Resource_not_found
            "could not find tree `%a'"
            pp_iri
            iri
        | Some tree -> analyse_tree [] tree
      end
    | None ->
      env.forest.parsed
      |> Forest.to_seq_values
      |> Seq.iter (analyse_tree [])
  end;
  env.forest.import_graph

let build forest =
  let env = {forest; follow = false} in
  run_builder env
