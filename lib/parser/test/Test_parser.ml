(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_test
open Forester_core
open Testables
open Forester_frontend.DSL.Code

let test_prim () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          ident ["p"];
          braces
            [
              ident ["ul"];
              braces
                [
                  ident ["li"];
                  braces
                    [text "foo"]
                ]
            ]
        ]
    )
    (
      parse_string_no_loc
        {|\p{\ul{\li{foo}}}|}
    )

let test_open () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [open_ ["foo"]])
    (parse_string_no_loc {|\open\foo|});
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [open_ ["foo"; "bar"; "baz"]])
    (parse_string_no_loc {|\open\foo/bar/baz|})

let test_scope () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          scope
            [
              ident ["p"];
              braces []
            ]
        ]
    )
    (parse_string_no_loc {|\scope{\p{}}|})

let test_verbatim () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [verbatim "asdf"])
    (parse_string_no_loc {|\verb<<|asdf<<|})

let test_math () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          math
            Inline
            [
              (text "a^2");
              (text " ");
              (text "+");
              (text " ");
              (text "b^2");
              (text " ");
              (text "=");
              (text " ");
              (text "c^2")
            ]
        ]
    )
    (parse_string_no_loc {|#{a^2 + b^2 = c^2}|});
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          math
            Display
            [
              (text "a^2");
              (text " ");
              (text "+");
              (text " ");
              (text "b^2");
              (text " ");
              (text "=");
              (text " ");
              (text "c^2")
            ]
        ]
    )
    (parse_string_no_loc {|##{a^2 + b^2 = c^2}|})

let test_object () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          object_
            {
              self = (Some ["self"]);
              methods = [
                (
                  "foo",
                  []
                )
              ]
            }
        ]
    )
    (
      parse_string_no_loc
        {|
        \object[self]{
          [foo]{}
        }|}
    )

let () =
  let open Alcotest in
  run
    "Parser"
    [
      "nodes", [test_case "open" `Quick test_open;];
      "scope", [test_case "scope" `Quick test_scope;];
      "text", [test_case "text" `Quick test_prim];
      "verbatim", [test_case "verbatim" `Quick test_verbatim];
      "math", [test_case "math" `Quick test_math];
      "object", [test_case "object" `Quick test_object];
    ]
