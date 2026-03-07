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

type render_callbacks = {
  route : State.t -> URI.t -> URI.t;
  render_content : State.t -> T.content -> node list;
  render_link : State.t -> T.content T.link -> node list;
}

let render_date (date : Human_datetime.t) =
  let year = txt "%i" (Human_datetime.year date) in
  let month =
    match Human_datetime.month date with
    | None -> None
    | Some i ->
        Some
          (txt "%s"
             (match i with
             | 1 -> "January"
             | 2 -> "February"
             | 3 -> "March"
             | 4 -> "April"
             | 5 -> "May"
             | 6 -> "June"
             | 7 -> "July"
             | 8 -> "August"
             | 9 -> "September"
             | 10 -> "October"
             | 11 -> "November"
             | 12 -> "December"
             | _ -> assert false))
  in
  let day =
    match Human_datetime.day date with None -> null [] | Some i -> txt "%i" i
  in
  li
    [ class_ "meta-item" ]
    [
      a
        [ class_ "link local" ]
        [
          Option.value ~default:(null []) month;
          (if Option.is_some month then txt " " else null []);
          day;
          (if Option.is_some month then txt ", " else null []);
          year;
        ];
    ]

let render_attributions ~callbacks forest
    (attributions : T.content T.attribution list) =
  let render_attribution attribution =
    match attribution with
    | T.{ vertex; _ } -> begin
        match vertex with
        | T.Uri_vertex href ->
            let content =
              T.Content
                [
                  T.Transclude
                    { href; target = Title { empty_when_untitled = false } };
                ]
            in
            null @@ callbacks.render_link forest T.{ href; content }
        | T.Content_vertex content ->
            null @@ callbacks.render_content forest content
      end
  in
  let authors, contributors =
    attributions
    |> List.partition_map @@ fun attribution ->
       match T.(attribution.role) with
       | T.Author -> Left attribution
       | Contributor -> Right attribution
  in
  li
    [ class_ "meta-item" ]
    [
      address [ class_ "author" ]
      @@ List.map render_attribution authors
      @ (if List.length contributors > 0 then [ txt "with contributions from " ]
         else [])
      @ List.map render_attribution contributors;
    ]

let render_frontmatter ~callbacks (forest : State.t)
    (frontmatter : T.content T.frontmatter) : node =
  let taxon =
    Option.value ~default:[]
    @@
    let@ content = Option.map @~ frontmatter.taxon in
    callbacks.render_content forest content @ [ txt ". " ]
  in
  let title =
    Option.value ~default:[]
    @@
    let@ content = Option.map @~ frontmatter.title in
    callbacks.render_content forest content
  in
  let uri =
    match frontmatter.uri with
    | None -> null []
    | Some uri ->
        let uri_str =
          Format.asprintf "%a" URI.pp (callbacks.route forest uri)
        in
        a [ class_ "slug"; href "%s" uri_str ] [ txt "[%s]" uri_str ]
  in
  let source_path =
    match frontmatter.source_path with
    | Some path ->
        [
          a
            [ class_ "edit-button"; href "vscode://file%s" path ]
            [ txt "[edit]" ];
        ]
    | None -> []
  in
  let find_meta key =
    let@ str, content = List.find_map @~ frontmatter.metas in
    if str = key then Some content else None
  in
  let render_meta key render_item =
    Option.value ~default:(null []) (Option.map render_item (find_meta key))
  in
  let default_meta_item content =
    li [ class_ "meta-item" ] (callbacks.render_content forest content)
  in
  let labelled_external_link ~href ~label =
    li
      [ class_ "meta-item" ]
      [ a [ class_ "link external"; href ] [ txt "%s" label ] ]
  in
  let to_string =
    Plain_text_client.string_of_content ~forest
      ~router:(Legacy_xml_client.route forest)
  in
  let position = render_meta "position" default_meta_item in
  let institution = render_meta "institution" default_meta_item in
  let venue = render_meta "venue" default_meta_item in
  let source = render_meta "source" default_meta_item in
  let doi = render_meta "doi" default_meta_item in
  let orcid =
    render_meta "orcid" @@ fun content ->
    let content = to_string content in
    li
      [ class_ "meta-item" ]
      [
        a
          [ class_ "doi link"; href "https://www.doi.org/%s" content ]
          [ txt "%s" content ];
      ]
  in
  let external_ =
    render_meta "external" @@ fun content ->
    let content = to_string content in
    li
      [ class_ "meta-item" ]
      [ a [ class_ "link external"; href "%s" content ] [ txt "%s" content ] ]
  in
  let slides =
    render_meta "slides" @@ fun content ->
    labelled_external_link ~href:(href "%s" (to_string content)) ~label:"Slides"
  in
  let video =
    render_meta "video" @@ fun content ->
    labelled_external_link ~href:(href "%s" (to_string content)) ~label:"Video"
  in
  header []
    [
      h1 []
      @@ [ span [ class_ "taxon" ] taxon ]
      @ title
      @ [ txt " "; uri ]
      @ source_path;
      div
        [ class_ "metadata" ]
        [
          ul []
          @@ List.map render_date frontmatter.dates
          @ [
              render_attributions ~callbacks forest frontmatter.attributions;
              position;
              institution;
              venue;
              source;
              doi;
              orcid;
              external_;
              slides;
              video;
            ];
        ];
    ]
