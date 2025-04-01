open Forester_prelude
open Forester_core
open Forester_compiler
open struct module T = Types end

let get_sorted_articles (forest : State.t) addrs =
  let module C = Types.Comparators(struct
    let string_of_content x = Plain_text_client.string_of_content ~forest x
  end) in
  addrs
  |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.uri_of_vertex
  |> Seq.filter_map (fun uri -> State.get_article uri forest)
  |> List.of_seq
  |> List.sort C.compare_article

let collect_attributions (forest : State.t) (uri_opt : URI.t option) (primary_attributions : _ T.attribution list) =
  match uri_opt with
  | None -> primary_attributions
  | Some uri ->
    let indirect_attributions =
      let open Datalog_expr.Notation in
      let articles =
        let x = "X" in
        let positives = [Builtin_relation.has_indirect_contributor @* [const (T.Uri_vertex uri); var x]] in
        let negatives = [] in
        Datalog_expr.{var = x; positives; negatives}
        |> Forest.run_datalog_query forest.graphs
        |> get_sorted_articles forest
      in
      let@ biotree : _ T.article = List.filter_map @~ articles in
      let@ uri = Option.map @~ biotree.frontmatter.uri in
      T.{vertex = T.Uri_vertex uri; role = Contributor}
    in
    primary_attributions @
      let@ attribution = List.filter_map @~ indirect_attributions in
      if List.exists (fun (existing : _ T.attribution) -> Vertex.equal attribution.vertex existing.vertex) primary_attributions then None
      else
        Some attribution
