(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
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

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
[@@deriving repr]

module Content = Htmx_content
module Frontmatter = Htmx_frontmatter
module Toc = Htmx_toc
module Query = Htmx_query

let route = Content.route

let rec render_article
    (forest : State.t)
    (article : T.content T.article)
    : node
  =
  let@ () = Content.Xmlns.run ~reserved: [] in
  HTML.article
    [id "tree-container"]
    [
      HTML.section
        [class_ "block"]
        [
          details
            [open_]
            (
              summary [] [render_frontmatter forest article.frontmatter] :: render_content forest article.mainmatter
            );
        ];
      (
        match article.frontmatter.uri with
        | None -> footer [] @@ render_backmatter forest article.backmatter
        | Some uri ->
          if URI.equal (Config.home_uri forest.config) uri then null []
          else footer [] @@ render_backmatter forest article.backmatter
      );
    ]

and render_section (forest : State.t) (section : T.content T.section) : node =
  match section with
  | {frontmatter; mainmatter; flags} ->
    let test fallback = function
      | Some true -> true
      | Some false -> false
      | None -> fallback
    in
    let class_ =
      if test false flags.metadata_shown then class_ "block"
      else class_ "block hide-metadata"
    in
    let data_taxon =
      match frontmatter.taxon with None -> null_ | Some _content -> null_
    in
    HTML.section
      [class_; data_taxon]
      [
        (
          if test true flags.header_shown then
            details
              [(if test true flags.expanded then open_ else null_)]
              [
                summary [] [render_frontmatter forest frontmatter];
                null @@ render_content forest mainmatter;
              ]
          else null @@ render_content forest mainmatter
        );
      ]

and render_backmatter (forest : State.t) backmatter =
  let@ node = List.map @~ render_content forest backmatter in
  let attrs = Format.asprintf "%s backmatter-section" node.@["class"] in
  node +@ class_ "%s" attrs

and render_content forest content =
  Content.render_content ~callbacks: content_callbacks forest content

and render_link forest link =
  Content.render_link ~callbacks: content_callbacks forest link

and render_frontmatter forest frontmatter =
  Frontmatter.render_frontmatter
    ~callbacks: frontmatter_callbacks
    forest
    frontmatter

and render_toc section = Toc.render_toc section

and render_query_result forest vs =
  Query.render_query_result ~route ~render_section forest vs

and content_callbacks =
  Content.{render_section;
    render_contextual_number = (fun section ->
      Toc.contextual_number section (Toc.default_toc_config ())
    );
  }

and frontmatter_callbacks = Frontmatter.{route; render_content; render_link}
