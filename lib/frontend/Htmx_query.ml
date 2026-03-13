(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct
  module T = Types
end

open Pure_html
open HTML

let render_query_result
    ~route
    ~render_section
    (forest : State.t)
    (vs : Vertex_set.t)
  =
  let module C = Types.Comparators(struct
    let string_of_content =
      Plain_text_client.string_of_content ~forest ~router: (route forest)
  end) in
  let make_section =
    T.article_to_section
      ~flags: T.{default_section_flags with
        expanded = Some false;
        numbered = Some false;
        included_in_toc = Some false;
        metadata_shown = Some true;
      }
  in
  let nodes =
    vs |> Vertex_set.to_seq
    |> Seq.filter_map Vertex.uri_of_vertex
    |> Seq.filter_map (State.get_article @~ forest)
    |> List.of_seq
    |> List.sort C.compare_article
    |> List.map (Fun.compose (render_section forest) make_section)
  in
  if List.length nodes = 0 then None
  else Some (div [class_ "tree-content"] nodes)
