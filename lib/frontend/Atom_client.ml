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
  module P = Pure_html
end

module Atom = struct
  let null = P.HTML.null

  let feed attrs =
    P.std_tag "feed" @@
      P.string_attr "xmlns" "http://www.w3.org/2005/Atom" :: attrs

  let title fmt = P.text_tag "title" fmt
  let link = P.void_tag "link"

  let href fmt = P.string_attr "href" fmt

  let updated fmt = P.text_tag "updated" fmt
  let published fmt = P.text_tag "published" fmt
  let author = P.std_tag "author"
  let contributor = P.std_tag "contributor"
  let name fmt = P.text_tag "name" fmt
  let uri fmt = P.uri_tag "uri" fmt
  let email fmt = P.uri_tag "email" fmt
  let id fmt = P.text_tag "id" fmt
  let entry = P.std_tag "entry"
  let summary = P.std_tag "summary"
  let content = P.std_tag "content"
  let type_ fmt = P.string_attr "type" fmt
  let rel fmt = P.string_attr "rel" fmt
end

open struct module A = Atom end

let get_date_range (article : _ T.article) : (Human_datetime.t * Human_datetime.t) option =
  let dates = List.sort Human_datetime.compare article.frontmatter.dates in
  try
    Some (List.hd dates, List.hd (List.rev dates))
  with
    | _ -> None

let render_title forest ?scope (frontmatter : _ T.frontmatter)  =
  A.title
    []
    "%s" @@
    Plain_text_client.string_of_content ~forest ~router: Fun.id @@
      State.get_expanded_title ?scope frontmatter forest

let render_dates_exn dates =
  let sorted_dates = List.sort Human_datetime.compare dates in
  let oldest, newest = List.hd sorted_dates, List.hd (List.rev sorted_dates) in
  A.null
    [
      A.published [] "%s" @@ Format.asprintf "%a" Human_datetime.pp_rfc_3399 oldest;
      A.updated [] "%s" @@ Format.asprintf "%a" Human_datetime.pp_rfc_3399 newest
    ]

let render_updated_date dates =
    let sorted_dates = List.sort Human_datetime.compare dates in
    let newest = List.hd (List.rev sorted_dates) in
    A.updated [] "%s" @@ Format.asprintf "%a" Human_datetime.pp newest

let render_dates dates =
  try render_dates_exn dates with _ -> A.null []

let string_of_content forest = Plain_text_client.string_of_content ~forest ~router: Fun.id

let render_attribution forest (attribution : _ T.attribution) =
  let tag =
    match attribution.role with
    | T.Author -> A.author
    | T.Contributor -> A.contributor
  in
  let body =
    match attribution.vertex with
    | T.Content_vertex content ->
      [A.name [] "%s" @@ string_of_content forest content]
    | T.Uri_vertex href ->
      let content = T.Content [T.Transclude {href; target = Title {empty_when_untitled = false}}] in
      [A.name [] "%s" @@ string_of_content forest content; A.uri [] "%s" @@ URI.to_string href]
  in
  tag [] body

let render_attributions (forest : State.t) uri_opt attributions : P.node =
  A.null @@
  List.map (render_attribution forest) @@
  Forest_util.collect_attributions forest uri_opt attributions


let get_embedded_articles (forest : State.t) (article : _ T.article) =
  let visit_node = function
    | T.Transclude {href; target = Full _; _} ->
      Vertex_set.add @@ Uri_vertex href
    | T.Section section ->
      Option.fold ~none: Fun.id ~some: (fun x -> Vertex_set.add @@ T.Uri_vertex x) section.frontmatter.uri
    | T.Results_of_datalog_query query ->
      Vertex_set.union @@ Forest.run_datalog_query forest.graphs query
    | _ -> Fun.id
  in
  let vertices = List.fold_left (Fun.flip visit_node) Vertex_set.empty @@ T.extract_content article.mainmatter in
  Forest_util.get_sorted_articles forest vertices

let render_entry ~(forest : State.t) ?(scope : URI.t option) (article : T.content T.article) : P.node =
  A.entry
    []
    [
      render_title forest ?scope article.frontmatter;
      render_dates article.frontmatter.dates;
      render_attributions forest article.frontmatter.uri article.frontmatter.attributions;
      begin
        match article.frontmatter.uri with
        | None -> A.null []
        | Some uri ->
          let uri_string = URI.to_string uri in
          A.null
            [
              A.link
                [
                  A.rel "alternate";
                  A.type_ "text/html";
                  A.href "%s" uri_string
                ];
              A.id [] "%s" uri_string
            ]
      end;
      A.content
        [
          A.type_ "xhtml"
        ]
        [
          Html_client.render_article_as_div ~heading_level: 1 forest article
        ]
    ]

let render_feed (forest : State.t) ~(source_uri : URI.t) ~(feed_uri : URI.t) : P.node =
  match State.get_article source_uri forest with
  | None -> Reporter.fatal @@ Resource_not_found source_uri
  | Some blog ->
    let articles = get_embedded_articles forest blog in
    let all_dates =
      let@ article = List.concat_map @~ blog :: articles in
      article.frontmatter.dates
    in
    let blog_uri_string = URI.to_string source_uri in
    A.feed
      []
      [
        render_attributions forest blog.frontmatter.uri blog.frontmatter.attributions;
        render_updated_date all_dates;
        render_title forest blog.frontmatter;
        A.id [] "%s" blog_uri_string;
        A.link [A.rel "alternate"; A.href "%s" blog_uri_string];
        A.link [A.rel "self"; A.href "%s" @@ URI.to_string feed_uri];
        A.null @@
          let@ article = List.map @~ articles in
          render_entry ~forest ~scope:source_uri article
      ]
