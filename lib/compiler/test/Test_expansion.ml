(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test
open Forester_lsp
open Forester_frontend
open Testables

module T = Types

open struct
  module S = Resolver.Scope
  module P = Resolver.P
  let config = Config.default
  let _data = Alcotest.testable P.pp_data (=)
end

open State.Syntax

let expand ~forest src =
  let@ code = Result.map @~ parse_string_no_loc src in
  let@ () = S.easy_run in
  Expand.expand ~forest code

let render ~forest expanded =
  Result.map
    (fun expanded ->
      let Eval.{articles; _}, _ =
        Eval.eval_tree
          ~config: Config.default
          ~uri: (URI.of_string_exn "http://localhost/test")
          ~source_path: None
          expanded
      in
      let () =
        List.iter
          (fun article ->
            let@ uri = Option.iter @~ T.(article.frontmatter.uri) in
            forest.={uri} <- Resource {tree = Article article; expanded = None}
          )
          articles
      in
      let rendered =
        List.map
          (fun article ->
            Plain_text_client.string_of_content ~forest T.(article.mainmatter)
          )
          articles
      in
      String.concat "" rendered
    )
    expanded

let test ~env () =
  let forest = State.make ~env ~config ~dev: false () in
  let expanded =
    expand ~forest
      {|
      \namespace\foo{
        \let\greet[name]{Hello, \name!}
        \greet{Jon}
      }
    |}
  in
  let evaluated = render ~forest expanded in
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
                      [Syn.Text "Hello,"; Text " "; Var (["name"], 6); Text "!"]
                  )
                )
            );
          Range.locate_opt None (Syn.Group (Braces, List.map (Range.locate_opt None) [(Syn.Text "Jon")]))
        ]
    )
    expanded;
  Alcotest.(check @@ result string diagnostic)
    ""
    (Ok "Hello, Jon!")
    evaluated

let test_subtree ~env () =
  let@ () = Reporter.easy_run in
  let forest = State.make ~env ~config ~dev: false () in
  let expanded =
    expand ~forest
      {|
  \subtree[foo]{
    \title{Hello}
    \taxon{Example}
  }
  |}
  in
  let evaluated = render ~forest expanded in
  Alcotest.(check @@ result string diagnostic)
    ""
    (
      Ok
        {|<omitted content>\title {Hello}\taxon {Example}|}
    )
    evaluated

let test_visible ~env () =
    let forest = State.make ~env ~config ~dev: false () in
  let code =
    Result.get_ok @@
      parse_string
        {|
\def\greet[name]{Hello, \name!}
\p{\greet{Jon}}
    |}
  in
  let result =
    Trie.to_seq @@
      Analysis.get_visible ~forest ~position: {line = 2; character = 5;} code
  in
  let greet =
    let@ (path, _) = Option.map @~ Seq.find (fun (p, _) -> p = ["greet"]) result in
    path
  in
  Alcotest.(check @@ (option path))
    "greet is visible"
    (Some (["greet"]))
    greet

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  let open Alcotest in
  let@ env = Eio_main.run in
  run
    "Test_expansion"
    [
      "",
      [
        test_case "expand" `Quick (test ~env);
        test_case "subtree" `Quick (test_subtree ~env);
        test_case "get_visible" `Quick (test_visible ~env);
      ]
    ]
