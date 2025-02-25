let v =
  let markup =
    let open Pure_html in
    let open HTML in
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
            div
              [class_ "search-wrapper";]
              [
                input
                  [
                    autofocus;
                    class_ "search";
                    type_ "search";
                    name "search";
                    Hx.post "/search";
                    Hx.trigger "input changed delay:500ms, keyup[key=='Enter'], load";
                    Hx.target "#search-results";
                    placeholder "Start typing a note title or ID";
                  ];
              ];
            ul
              [id "search-results";]
              [];
          ];
      ]
  in
  Pure_html.to_string markup
