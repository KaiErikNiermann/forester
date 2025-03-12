(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
include G
include Graph.Oper.I(G)
module Map = Graph.Gmap.Vertex(G)

module Reachability = Graph.Fixpoint.Make(G)(struct
  type vertex = G.E.vertex
  type edge = G.E.t
  type g = G.t
  type data = bool
  let direction = Graph.Fixpoint.Forward
  let equal = (=)
  let join = (||)
  let analyze _ = (fun x -> x)
end)

module Topo = Graph.Topological.Make(G)
let topo_fold = Topo.fold

let safe_succ g x =
  if mem_vertex g x then succ g x else []

let safe_dependents = safe_succ

let safe_pred g x =
  if mem_vertex g x then pred g x else []

let immediate_dependencies = safe_pred

let add_edge_safe g v w = if mem_vertex g v && mem_vertex g w then add_edge g v w

let dependencies graph vertex : t =
  let dep_graph = create () in
  let rec go v =
    iter_pred
      (fun dep ->
        if mem_vertex dep_graph dep then ()
        else
          begin
            add_edge dep_graph dep v;
            go dep
          end
      )
      graph
      v
  in
  go vertex;
  dep_graph

module Graphviz = Graph.Graphviz.Dot(struct
  include G
  module V = Vertex

  let vertex_name v =
    match Vertex.uri_of_vertex v with
    | Some uri -> "\"" ^ URI.to_string uri ^ "\""
    | None -> ""

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes e = [`Label ""]
  let get_subgraph _ = None
end)
