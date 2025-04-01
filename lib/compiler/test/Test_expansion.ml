(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test
(* open Forester_lsp *)
open Testables

module T = Types

open struct
  module S = Resolver.Scope
  module P = Resolver.P
  let _data = Alcotest.testable P.pp_data (=)
  let _tag = Alcotest.testable (Format.pp_print_option Asai.Range.dump) (=)
end

let test () =
  let result =
    let@ code =
      Result.map @~
        parse_string
          {|
      \namespace\foo{
        \let\greet[name]{Hello, \name!}
        \greet{Jon}
      }
    |}
    in
    let@ () = S.easy_run in
    Expand.expand code
  in
  Alcotest.(check @@ result syn diagnostic)
    ""
    (
      Ok
        [
          Range.locate_opt
            None
            (
              Syn.Fun
                (
                  [Strict, (["name"], 6)],
                  (
                    List.map
                      (Range.locate_opt None)
                      [
                        (Syn.Text "Hello,");
                        (Syn.Text " ");
                        (Syn.Var (["name"], 6));
                        (Syn.Text "!")
                      ]
                  )
                )
            );
          Range.locate_opt
            None
            (
              Syn.Group
                (
                  Braces,
                  List.map
                    (Range.locate_opt None)
                    [(Syn.Text "Jon")]
                )
            )
        ]
    )
    result

(* let test_visible () = *)
(*   let code = *)
(*     Result.get_ok @@ *)
(*       parse_string *)
(*         {| *)
(* \def\greet[name]{Hello, \name!} *)
(* \p{\greet{Jon}} *)
(*     |} *)
(*   in *)
(*   let result = *)
(*     Trie.to_seq @@ *)
(*       Analysis.get_visible ~position: {line = 2; character = 5;} code *)
(*   in *)
(*   Alcotest.(check @@ (seq (pair path (pair data tag)))) *)
(*     "" *)
(*     (List.to_seq []) *)
(*     result *)

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let open Alcotest in
  run
    "Test_expansion"
    [
      "",
      [
        test_case "expand" `Quick test;
        (* test_case "get_visible" `Quick test_visible; *)
      ]
    ]
