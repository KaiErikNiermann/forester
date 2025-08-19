(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open struct
  module T = Types
  module Dx = Datalog_expr
end

include URI.Tbl

type article = T.content T.article
type env = (module Forest_graphs.S)

let execute_datalog_script graphs script =
  let module Graphs = (val graphs : Forest_graphs.S) in
  let@ sequent = List.iter @~ script in
  Datalog_engine.db_add Graphs.dl_db (Datalog_eval.eval_sequent sequent)

(* TODO: Why is this not run at the top level? *)
(* let () = execute_datalog_script Builtin_relation.axioms *)

let run_datalog_query (graphs : env) (q : (string, Vertex.t) Dx.query) : Vertex_set.t =
  let@ () = Reporter.trace "when running query" in
  (* TODO: See above *)
  let () = execute_datalog_script graphs Builtin_relation.axioms in
  let module Graphs = (val graphs) in
  Datalog_eval.run_query Graphs.dl_db q

let add_edge graphs rel ~source ~target =
  let module Graphs = (val graphs : Forest_graphs.S) in
  let premises = [] in
  let conclusion =
    let args = [Dx.Const source; Dx.Const target] in
    Dx.{rel; args}
  in
  execute_datalog_script graphs [{conclusion; premises}]

let add_fact graphs rel node =
  let module Graphs = (val graphs : Forest_graphs.S) in
  let premises = [] in
  let conclusion =
    let args = [Dx.Const node] in
    Dx.{rel; args}
  in
  execute_datalog_script graphs [{conclusion; premises}]

let rec analyse_content_node graphs (scope : URI.t) (node : 'a T.content_node) : unit =
  match node with
  | Text _ | CDATA _ | Route_of_uri _ | Uri _ | Results_of_datalog_query _ | Contextual_number _ -> ()
  | Transclude transclusion ->
    analyse_transclusion graphs scope transclusion
  | Xml_elt elt ->
    begin
      let@ attr = List.iter @~ elt.attrs in
      analyse_content graphs scope attr.value
    end;
    analyse_content graphs scope elt.content
  | Section section ->
    analyse_section graphs scope section
  | Link link ->
    add_edge graphs Builtin_relation.links_to ~source: (Uri_vertex scope) ~target: (Uri_vertex link.href);
    analyse_content graphs scope link.content
  | KaTeX (_, content) ->
    analyse_content graphs scope content
  | Artefact artefact ->
    analyse_artefact graphs scope artefact
  | Datalog_script script ->
    execute_datalog_script graphs script

and analyse_artefact graphs scope artefact =
  analyse_content graphs scope artefact.content

and analyse_transclusion graphs (scope : URI.t) (transclusion : T.transclusion) : unit =
  match transclusion.target with
  | Full _ | Mainmatter ->
    add_edge graphs Builtin_relation.transcludes ~source: (Uri_vertex scope) ~target: (Uri_vertex transclusion.href)
  | Title _ | Taxon -> ()

and analyse_content (graphs : env) (scope : URI.t) (content : T.content) : unit =
  T.extract_content content |> List.iter @@ analyse_content_node graphs scope

and analyse_attribution graphs (scope : URI.t) (attr : _ T.attribution) =
  let rel =
    match attr.role with
    | Author -> Builtin_relation.has_author
    | Contributor -> Builtin_relation.has_direct_contributor
  in
  add_edge graphs rel ~source: (Uri_vertex scope) ~target: attr.vertex;
  analyse_vertex graphs scope attr.vertex

and analyse_vertex graphs scope vtx =
  match vtx with
  | Uri_vertex _ -> ()
  | Content_vertex content -> analyse_content graphs scope content

and analyse_tag graphs (scope : URI.t) (tag : _ T.vertex) =
  analyse_vertex graphs scope tag;
  add_edge graphs Builtin_relation.has_tag ~source: (Uri_vertex scope) ~target: tag

and analyse_taxon graphs (scope : URI.t) (taxon_opt : T.content option) =
  let@ taxon = Option.iter @~ taxon_opt in
  analyse_content graphs scope taxon;
  add_edge graphs Builtin_relation.has_taxon ~source: (Uri_vertex scope) ~target: (Content_vertex taxon)

and analyse_attributions graphs (scope : URI.t) =
  List.iter @@ analyse_attribution graphs scope

and analyse_tags graphs (scope : URI.t) =
  List.iter @@ analyse_tag graphs scope

and analyse_frontmatter graphs (scope : URI.t) (fm : T.content T.frontmatter) : unit =
  Option.iter (analyse_content graphs scope) fm.title;
  analyse_taxon graphs scope fm.taxon;
  analyse_attributions graphs scope fm.attributions;
  analyse_tags graphs scope fm.tags;
  analyse_metas graphs scope fm.metas

and analyse_metas graphs (scope : URI.t) =
  List.iter @@ analyse_meta graphs scope

and analyse_meta graphs (scope : URI.t) (_, content) : unit =
  analyse_content graphs scope content

and analyse_section graphs (scope : URI.t) (section : T.content T.section) : unit =
  begin
    let@ target = Option.iter @~ section.frontmatter.uri in
    add_edge graphs Builtin_relation.transcludes ~source: (Uri_vertex scope) ~target: (Uri_vertex target)
  end;
  let scope = Option.value ~default: scope section.frontmatter.uri in
  analyse_frontmatter graphs scope section.frontmatter;
  analyse_content graphs scope section.mainmatter

let analyse_article graphs (article : article) : unit =
  let@ scope = Option.iter @~ article.frontmatter.uri in
  add_fact graphs Builtin_relation.is_article (T.Uri_vertex scope);
  analyse_frontmatter graphs scope article.frontmatter;
  analyse_content graphs scope article.mainmatter;
  analyse_content graphs scope article.backmatter

let analyse_asset graphs (asset : T.asset) : unit =
  add_fact graphs Builtin_relation.is_asset (T.Uri_vertex asset.uri)

let analyse_resource graphs = function
  | T.Article article -> analyse_article graphs article
  | T.Asset asset -> analyse_asset graphs asset
  | _ -> ()
