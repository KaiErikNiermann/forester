(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
module Document_format = Forester_lsp.Document_format

let trim_trailing_newlines text =
  let rec loop i =
    if i > 0 && (text.[i - 1] = '\n' || text.[i - 1] = '\r') then loop (i - 1)
    else i
  in
  let len = loop (String.length text) in
  String.sub text 0 len

let format input =
  match parse_string_no_loc input with
  | Ok code -> Document_format.format_code code |> trim_trailing_newlines
  | Error _ -> Alcotest.fail "Expected formatter fixture to parse"

let test_formatter_normalizes_def_header_sugar () =
  Alcotest.(check string)
    "def header sugar normalizes to square binders"
    {|\def\macro[x][~y]{body}|}
    (format {|\def\macro(x, ~y){body}|})

let test_formatter_normalizes_fun_header_sugar () =
  Alcotest.(check string)
    "fun header sugar normalizes to square binders"
    {|\fun[x][y]{body}|}
    (format {|\fun(x, y){body}|})

let test_formatter_normalizes_object_header_sugar () =
  Alcotest.(check string)
    "object header sugar normalizes to square binders"
    {|\object[self]{
  [render]{ok}
}|}
    (format {|\object(self){[render]{ok}}|})

let test_formatter_normalizes_patch_header_sugar () =
  Alcotest.(check string)
    "patch header sugar normalizes to square binders"
    {|\patch{\get\base}[self][super]{
  [render]{ok}
}|}
    (format {|\patch{\get\base}(self, super){[render]{ok}}|})

let test_formatter_def_command_body_stays_inline () =
  Alcotest.(check string)
    "def command body stays inline when simple"
    {|\def\macro{\get\item}|}
    (format {|\def\macro{\get\item}|})

let test_formatter_object_multiple_methods () =
  Alcotest.(check string)
    "object method list stays structured"
    {|\object[self]{
  [render]{ok}
  [other]{fine}
}|}
    (format {|\object(self){[render]{ok}[other]{fine}}|})

let () =
  Alcotest.run
    "Document format"
    [
      (
        "header sugar",
        [Alcotest.test_case
          "normalizes def header sugar"
          `Quick
          test_formatter_normalizes_def_header_sugar;
        Alcotest.test_case
          "normalizes fun header sugar"
          `Quick
          test_formatter_normalizes_fun_header_sugar;
        Alcotest.test_case
          "normalizes object header sugar"
          `Quick
          test_formatter_normalizes_object_header_sugar;
        Alcotest.test_case
          "normalizes patch header sugar"
          `Quick
          test_formatter_normalizes_patch_header_sugar;
        Alcotest.test_case
          "def command body stays inline"
          `Quick
          test_formatter_def_command_body_stays_inline;
        Alcotest.test_case
          "object multiple methods"
          `Quick
          test_formatter_object_multiple_methods;
        ]
      );
    ]
