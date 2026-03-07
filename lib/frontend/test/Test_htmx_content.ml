(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

module Content = Forester_frontend.Htmx_content
open Forester_core

let check_json label expected actual =
  Alcotest.(check bool) label true (Yojson.Safe.equal expected actual)

let test_title_flags_to_http_header () =
  check_json "title header"
    (`Assoc [ ("Empty-When-Untitled", `String "true") ])
    (Content.title_flags_to_http_header { Types.empty_when_untitled = true })

let test_section_flags_to_http_header_omits_none () =
  let flags =
    Types.
      {
        default_section_flags with
        hidden_when_empty = Some true;
        numbered = Some false;
      }
  in
  check_json "section headers"
    (`Assoc
       [
         ("Hidden-When-Empty", `String "true");
         ("Metadata-Shown", `String "false");
         ("Numbered", `String "false");
       ])
    (Content.section_flags_to_http_header flags)

let test_content_target_to_http_header_tags_target () =
  let flags = Types.{ empty_when_untitled = false } in
  check_json "title target"
    (`Assoc
       [ ("Title", `String "true"); ("Empty-When-Untitled", `String "false") ])
    (Content.content_target_to_http_header (Types.Title flags))

let () =
  Alcotest.run "HTMX content"
    [
      ( "headers",
        [
          Alcotest.test_case "title flags" `Quick
            test_title_flags_to_http_header;
          Alcotest.test_case "section flags" `Quick
            test_section_flags_to_http_header_omits_none;
          Alcotest.test_case "content target" `Quick
            test_content_target_to_http_header_tags_target;
        ] );
    ]
