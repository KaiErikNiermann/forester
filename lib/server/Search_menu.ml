(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_frontend

open Pure_html
open HTML

let v =
  let markup =
    div
      [
        class_ "modal-overlay";
        Hx.trigger "click target:.modal-overlay, keyup[key=='Escape'] from:body";
        Hx.target "#modal-container";
        Hx.get "/nil";
      ]
      [
        div
          [class_ "modal-content";]
          [
            form
              [
                class_ "search-form";
                Hx.post "/search";
                Hx.trigger "input changed delay:500ms, keyup[key=='Enter'], load";
                Hx.target "#search-results";
              ]
              [
                input
                  [
                    autofocus;
                    class_ "search";
                    type_ "search";
                    name "search";
                    placeholder "Start typing a note title or ID";
                  ];
                span
                  []
                  [
                    input [type_ "radio"; name "search-for"; value "full-text"];
                    label [for_ "full-text"] [txt "Full text"];
                  ];
                span
                  []
                  [
                    input [type_ "radio"; name "search-for"; value "title"];
                    label [for_ "title-text"] [txt "title"];
                  ];
              ];
            ul
              [id "search-results";]
              [];
          ];
      ]
  in
  Pure_html.to_string markup

let results (forest : State.t) (links : iri list) =
  Pure_html.to_string @@
    ul
      [id "search-results"]
      (
        List.filter_map
          (fun iri ->
            let title = Forest.get_content_of_transclusion {href = iri; target = Title {empty_when_untitled = false}; modifier = Sentence_case;} forest.resources in
            Option.map
              (fun t ->
                a
                  [
                    class_ "search-result-item";
                    href "/trees%s" (Iri.path_string iri);
                    Hx.target "#tree-container";
                    Hx.swap "outerHTML";
                  ] @@
                  Htmx_client.render_content forest t
              )
              title
          )
          links
      )
