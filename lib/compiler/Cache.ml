(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_search
module T = Types

type tree = {
  timestamp: float option;
  syn: Syn.t option;
  article: T.content T.article;
  deps: iri list;
}

type forest = tree Iri_tbl.t

let serialize_graphs
  : (module Forest_graphs.S) -> 'a
= fun s ->
  let module Graphs = (val s) in
  Graphs.dl_db

type t = {
  search_index: Index.t;
  forest: forest;
  dl_db: Datalog_engine.db;
}

let serialize_state : State.t -> t = function
  | {graphs; resources; search_index; parsed; import_graph; expanded; _} ->
    let dl_db = serialize_graphs graphs in
    let forest : tree Iri_tbl.t = Iri_tbl.create 1000 in
    Forest.get_all_articles resources
    |> List.iter
        (function
          | (T.{frontmatter = {iri = Some iri; _}; _} as article) ->
            begin
              (* match Iri_tbl.find_opt expanded iri with *)
              match Iri_tbl.find_opt parsed iri with
              | Some {timestamp; _} ->
                let deps =
                  List.filter_map
                    (function
                      | T.Iri_vertex iri -> Some iri
                      | _ -> None
                    ) @@
                    Forest_graph.safe_pred import_graph (T.Iri_vertex iri)
                in
                let syn = Iri_tbl.find_opt expanded iri in
                Iri_tbl.add
                  forest
                  iri
                  {
                    article;
                    syn;
                    timestamp;
                    deps;
                  }
              | None ->
                ()
            end
          | _ ->
            (* This does not appear to happen?*)
            assert false
        );
    {search_index; forest; dl_db;}

let reconstruct : env: Eio_unix.Stdenv.base -> config: Config.t -> t -> State.t = fun ~env ~config forest ->
  match forest with
  | {search_index; forest; dl_db} ->
    let init =
      Phases.init
        ~env
        ~config
        ~dev: true
    in
    let graphs = Forest_graphs.init dl_db in
    Iri_tbl.iter
      (fun iri v ->
        Iri_tbl.add init.resources iri (T.Article v.article);
        let _ = Option.get @@ Option.map (Iri_tbl.add init.expanded iri) v.syn in
        ()
      )
      forest;
    {init with search_index; graphs; resources = init.resources}

let marshal filename (v : t) =
  let oc = open_out_bin filename in
  Fun.protect
    ~finally: (fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc v [])

let unmarshal filename : t =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally: (fun () -> close_in ic)
    (fun () -> Marshal.from_channel ic)
