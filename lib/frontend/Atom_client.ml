(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

open struct
  module T = Types
  module P = Pure_html
end

module Atom = struct
  let feed attrs =
    P.std_tag "feed" @@
      P.string_attr "xmlns" "http://www.w3.org/2005/Atom" :: attrs

  let title fmt = P.text_tag "title" fmt
  let link = P.void_tag "title"

  let href fmt = P.string_attr "href" fmt

  let updated fmt = P.text_tag "updated" fmt
  let published fmt = P.text_tag "published" fmt
  let author = P.std_tag "author"
  let contributor = P.std_tag "contributor"
  let name fmt = P.text_tag "name" fmt
  let uri fmt = P.uri_attr "uri" fmt
  let email fmt = P.uri_attr "email" fmt
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

let render_entry (forest : State.t) (article : T.content T.article) : P.node =
  A.entry
    []
    [
      A.title
        []
        "%s"
        begin
          match article.frontmatter.title with
          | None -> "Untitled"
          | Some title -> Plain_text_client.string_of_content ~forest ~router: Fun.id title
        end;
      P.HTML.null
        begin
          match get_date_range article with
          | None -> []
          | Some (oldest, newest) ->
            [
              A.published [] "%s" @@ Format.asprintf "%a" Human_datetime.pp oldest;
              A.updated [] "%s" @@ Format.asprintf "%a" Human_datetime.pp newest
            ]
        end;
      begin
        match article.frontmatter.uri with
        | None -> P.HTML.null []
        | Some uri ->
          let uri_string = URI.to_string uri in
          P.HTML.null
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
          Html_client.render_article forest article
        ]
    ]
