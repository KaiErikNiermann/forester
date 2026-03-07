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

let test_verbatim_unterminated_errors () =
  Alcotest.(check bool)
    "unterminated inline verbatim returns parse error"
    true
    (Result.is_error (parse_string_no_loc {|\verb<||}));
  Alcotest.(check bool)
    "unterminated block verbatim returns parse error"
    true
    (Result.is_error (parse_string_no_loc {|\startverb|}))

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

let test_hashtag () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [hash_ident "abc"])
    (parse_string_no_loc {|#abc|})

let test_object () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          object_
            {
              self = (Some "self");
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

let test_parenthesized_header_binders () =
  Alcotest.(check @@ result code diagnostic)
    "def parenthesized binders desugar to same AST"
    (Ok [def ["macro"] [ (Strict, "x"); (Lazy, "y") ] [text "body"]])
    (parse_string_no_loc {|\def\macro(x, ~y){body}|});
  Alcotest.(check @@ result code diagnostic)
    "def empty parenthesized binders desugar to zero binders"
    (Ok [def ["macro"] [] [text "body"]])
    (parse_string_no_loc {|\def\macro(){body}|});
  Alcotest.(check @@ result code diagnostic)
    "fun parenthesized binders desugar to same AST"
    (Ok [({ Range.loc = None; value = Code.Fun ([ (Strict, "x"); (Strict, "y") ], [text "body"]) } : Code.node Range.located)])
    (parse_string_no_loc {|\fun(x, y){body}|});
  Alcotest.(check @@ result code diagnostic)
    "object parenthesized self desugars to same AST"
    (Ok [object_ { self = Some "self"; methods = [ ("render", []) ] }])
    (parse_string_no_loc {|\object(self){[render]{}}|});
  Alcotest.(check @@ result code diagnostic)
    "patch parenthesized bindings desugar to same AST"
    (Ok [
      ({ Range.loc = None; value = Code.Patch {
          obj = [({ Range.loc = None; value = Code.Get ["base"] } : Code.node Range.located)];
          self = Some "self";
          super = Some "super";
          methods = [ ("render", [text "ok"]) ];
        } } : Code.node Range.located)
    ])
    (parse_string_no_loc {|\patch{\get\base}(self, super){[render]{ok}}|})

let test_parenthesized_header_binders_reject_invalid_forms () =
  Alcotest.(check bool)
    "let does not accept parenthesized binders in phase 1"
    true
    (Result.is_error (parse_string_no_loc {|\let\macro(x){body}|}));
  Alcotest.(check bool)
    "mixed parenthesized and square binders are rejected"
    true
    (Result.is_error (parse_string_no_loc {|\def\macro(x)[y]{body}|}));
  Alcotest.(check bool)
    "mixed square and parenthesized binders are rejected"
    true
    (Result.is_error (parse_string_no_loc {|\def\macro[x](y){body}|}));
  Alcotest.(check bool)
    "object requires exactly one parenthesized self binder"
    true
    (Result.is_error (parse_string_no_loc {|\object(){[render]{}}|}));
  Alcotest.(check bool)
    "patch rejects too many parenthesized binders"
    true
    (Result.is_error (parse_string_no_loc {|\patch{\get\base}(self, super, extra){[render]{}}|}));
  Alcotest.(check bool)
    "parenthesized binder entries reject empty items"
    true
    (Result.is_error (parse_string_no_loc {|\def\macro(x,){body}|}));
  Alcotest.(check bool)
    "parenthesized binder entries reject internal whitespace"
    true
    (Result.is_error (parse_string_no_loc {|\def\macro(x y){body}|}))

let () =
  let open Alcotest in
  run
    "Parser"
    [
      "nodes", [test_case "open" `Quick test_open;];
      "scope", [test_case "scope" `Quick test_scope;];
      "text", [test_case "text" `Quick test_prim];
      "verbatim",
      [test_case "verbatim" `Quick test_verbatim;
      test_case
        "unterminated verbatim returns error"
        `Quick
        test_verbatim_unterminated_errors;
      ];
      "math", [test_case "math" `Quick test_math];
      "hashtag", [test_case "hashtag" `Quick test_hashtag];
      "object", [test_case "object" `Quick test_object];
      "header-binders",
      [ test_case "parenthesized binders" `Quick test_parenthesized_header_binders;
        test_case
          "parenthesized binders reject invalid forms"
          `Quick
          test_parenthesized_header_binders_reject_invalid_forms
      ];
    ]
