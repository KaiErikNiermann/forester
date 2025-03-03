(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Pure_html
open HTML

let v ?c () =
  html
    []
    [
      head
        []
        [
          meta [name "viewport"; content "width=device-width";];
          link [rel "stylesheet"; href "/style.css";];
          link [rel "icon"; type_ "image/x-icon"; href "/favicon.ico";];
          script [type_ "module"; src "/min.js";] "";
          script [src "/htmx.js"] "";
          link [rel "stylesheet"; href "https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.css"; integrity "sha384-zh0CIslj+VczCZtlzBcjt5ppRcsAmDnRem7ESsYwWwg3m/OaJ2l4x7YBZl9Kxxib"; crossorigin `anonymous;];
          script [src "https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.js"; integrity "sha384-CAltQiu9myJj3FAllEacN6FT+rOyXo+hFZKGuR2p4HB8JvJlyUHm31eLfL4eEiJL"; crossorigin `anonymous;] "";
          title [] "";
        ];
      body
        [Hx.boost true;]
        [
          header [] [];
          div
            [id "grid-wrapper";]
            [
              match c with
              | Some stuff -> stuff
              | None ->
                article
                  [
                    id "tree-container";
                    Hx.get "/home";
                    Hx.trigger "load";
                    Hx.target "this";
                    Hx.swap "outerHTML";
                  ]
                  [];
            ];
          div [id "modal-container";] [];
        ];
    ]
