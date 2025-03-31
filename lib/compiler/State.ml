(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open Tree
open Forester_core

type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  dev: bool;
  config: Config.t;
  index: Tree.t URI.Tbl.t;
  diagnostics: Reporter.Message.t Asai.Diagnostic.t list URI.Tbl.t;
  graphs: (module Forest_graphs.S);
  import_graph: Forest_graph.t;
  dependency_cache: Cache.t;
  resolver: string URI.Tbl.t;
  search_index: Forester_search.Index.t;
  usages: (Tree.exports, URI.t Asai.Range.located) Hashtbl.t;
  history: Action.t list;
}

let make
  ~(env : Eio_unix.Stdenv.base)
  ~(config : Config.t)
  ~(dev : bool)
  ?(graphs = (module Forest_graphs.Make (): Forest_graphs.S))
  ?(import_graph = Forest_graph.create ~size: 1000 ())
  ?(resolver = URI.Tbl.create 1000)
  ?(index = URI.Tbl.create 1000)
  ?(diagnostics = URI.Tbl.create 1000)
  ?(usages = Hashtbl.create 1000)
  ?(search_index = Forester_search.Index.create [])
  ?(dependency_cache = Cache.empty)
  ()
= {env; dev; config; index; diagnostics; resolver; import_graph; graphs; search_index; dependency_cache; usages; history = []}

module Syntax = struct
  let (.={}) state uri =
    URI.Tbl.find_opt state.index uri

  let (.={} <-) state uri item =
    URI.Tbl.replace state.index uri item

  (* / for units*)
  let (./{}) state uri =
    Option.bind
      (URI.Tbl.find_opt state.index uri)
      Tree.get_units

  (* updating units*)
  let (./{} <-) state uri units =
    let@ () = Reporter.tracef "when updating units for %a" URI.pp uri in
    match URI.Tbl.find_opt state.index uri with
    | None -> Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "Updating units: %a not found" URI.pp uri]
    | Some (Document _)
    | Some (Parsed _) ->
      Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "%a has not been expanded yet" URI.pp uri]
    | Some (Expanded expanded) ->
      URI.Tbl.replace
        state.index
        uri
        (Expanded {expanded with units})
    | Some (Resource _) -> ()

  (* ? for diagnostics*)
  let (.?{}) state uri =
    Option.value ~default: [] (URI.Tbl.find_opt state.diagnostics uri)

  let (.?{} <-) state uri diagnostics = URI.Tbl.add state.diagnostics uri diagnostics

  (* @ for article/resource *)
  let (.@{}) state uri =
    match URI.Tbl.find_opt state.index uri with
    | Some (Document _) -> None
    | Some (Parsed _)
    | Some (Expanded (_))
    | None ->
      None
    | Some (Resource res) -> Some res.tree
end

open Syntax

let update_history forest action = {forest with history = action :: forest.history}

let find_opt state uri = URI.Tbl.find_opt state.index uri
let to_seq state = URI.Tbl.to_seq state.index

let get_all_unparsed state =
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter is_unparsed

let get_all_code state =
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter_map to_code

let get_all_unexpanded state =
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter is_unexpanded

let get_all_expanded state =
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter_map to_expanded

let get_all_unevaluated state =
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter is_unevaluated

let get_all_articles : t -> T.content T.article Seq.t = fun state ->
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter_map to_article

let get_all_resources : t -> T.content T.resource Seq.t = fun state ->
  state.index
  |> URI.Tbl.to_seq_values
  |> Seq.filter_map to_resource

let get_resource state uri =
  match state.={uri} with
  | None -> None
  | Some tree -> to_resource tree

let get_code state uri =
  match state.={uri} with
  | None -> None
  | Some tree -> to_code tree

let get_article : URI.t -> t -> T.content T.article option = fun uri forest ->
  match URI.Tbl.find_opt forest.index uri with
  | None
  | Some (Document _)
  | Some (Parsed _)
  | Some (Expanded _) ->
    None
  | Some (Resource {tree; _}) ->
    match tree with
    | T.Asset _ -> None
    | T.Article a -> Some a

let section_symbol = "§"

let rec get_expanded_title ?scope ?(flags = T.{empty_when_untitled = false}) (frontmatter : _ T.frontmatter) forest =
  let short_title =
    match frontmatter.title with
    | Some content -> content
    | None when not flags.empty_when_untitled ->
      begin
        match frontmatter.uri with
        | Some uri -> T.Content [T.Uri uri]
        | _ -> T.Content [T.Text "Untitled"]
      end
    | _ -> T.Content []
  in
  Option.value ~default: short_title @@
    match frontmatter.designated_parent with
    | Some parent_uri when not (Option.equal URI.equal scope frontmatter.designated_parent) ->
      let@ parent = Option.map @~ get_article parent_uri forest in
      let parent_title = get_expanded_title parent.frontmatter forest in
      let parent_link = T.Link {href = parent_uri; content = parent_title} in
      let chevron = T.Text " › " in
      T.map_content (fun xs -> parent_link :: chevron :: xs) short_title
    | _ -> None

let get_content_of_transclusion (transclusion : T.transclusion) forest =
  match transclusion.target with
  | Full flags ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    T.Content [T.Section (T.article_to_section article ~flags)]
  | Mainmatter ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    article.mainmatter
  | Title flags ->
    Option.some @@
      begin
        match get_article transclusion.href forest with
        | None -> T.Content [T.Uri transclusion.href]
        | Some article -> get_expanded_title ~flags article.frontmatter forest
      end
  | Taxon ->
    let@ article = Option.map @~ get_article transclusion.href forest in
    let default = T.Content [T.Text section_symbol] in
    Option.value ~default article.frontmatter.taxon

let get_title_or_content_of_vertex ?(not_found = fun _ -> None) vertex forest =
  match vertex with
  | T.Content_vertex content -> Some content
  | T.Uri_vertex uri ->
    begin
      match get_article uri forest with
      | Some article -> article.frontmatter.title
      | None -> not_found uri
    end

let plant_resource resource forest =
  let module Graphs = (val forest.graphs) in
  Forest.analyse_resource forest.graphs resource;
  let@ uri = Option.iter @~ T.uri_for_resource resource in
  let uri = URI.canonicalise uri in
  Graphs.register_uri uri;
  match forest.={uri} with
  | None ->
    forest.={uri} <- Resource {tree = resource; expanded = None}
  | Some (Tree.Expanded syn) ->
    forest.={uri} <- Resource {tree = resource; expanded = Some syn}
  | _ ->
    forest.={uri} <- Resource {tree = resource; expanded = None}

let serialize_graphs
  : (module Forest_graphs.S) -> 'a
= fun s ->
  let module Graphs = (val s) in
  Graphs.dl_db

let batch_write : t -> _ = function
  | {import_graph;
    _
  } ->
    (* let dl_db = serialize_graphs graphs in *)
    let open Cache in
    let module Gmap = Forest_graph.Map(Cache.Dependecy_graph) in
    let tbl = Dependency_tbl.create 100 in
    let now = Unix.time () in
    let g =
      Gmap.map
        (function
          | T.Content_vertex _ ->
            (*Import graph has no content vertices*)
            assert false
          | T.Uri_vertex uri ->
            let item = Item.Tree uri in
            Dependency_tbl.add tbl item Item.{timestamp = Some now; color = Green};
            item
        )
        import_graph
    in
    {Cache.empty with graph = g; tbl;}

let reconstruct = fun ~env: _ ~(_config : Config.t) paths cache ->
  match cache with
  | {search_index = _; _} ->
    (* let init = Phases.init ~env ~config ~dev: true in *)
    (* let graphs = Forest_graphs.init dl_db in *)
    paths
    |> Seq.iter (fun _path ->
        (* let uri = URI_scheme.path_to_uri ~base: config.url (Eio.Path.native_exn path) in *)
        (* match URI.Tbl.find_opt forest uri with *)
        (* | None -> () *)
        (* | Some tree -> *)
        (*   match check_timestamp path tree.timestamp with *)
        (*   | _ -> () *)
        ()
      )
