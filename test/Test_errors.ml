(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Testables
open Prelude

let parse_string str =
  let lexbuf = Lexing.from_string str in
  let res = Parse.parse ~source: (`String {title = None; content = str}) lexbuf in
  Result.map strip_loc res

let test_parse_error_explanation src expect =
  Alcotest.(check @@ result code string)
    ""
    (Result.Error expect)
    (
      parse_string src
      |> Result.map_error
          (fun d -> Asai.Diagnostic.string_of_text d.explanation.value)
    )

let () =
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = {Config.default with trees = ["errors"]} in
  let _tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let _mk_iri addr = Iri_scheme.user_iri ~host: config.host addr in
  let _, forest =
    State.make ~env ~config ~dev: false ()
    |> Driver.(update Load_all_configured_dirs)
  in
  let parse_error_uri =
    forest.documents
    |> Hashtbl.to_seq_keys
    |> Seq.find_map
        (fun uri ->
          if String.ends_with
            ~suffix: "parse_error.tree"
            (Lsp.Uri.to_string uri) then Some uri
          else None
        )
    |> Option.get
  in
  let test () =
    Alcotest.(check string)
      ""
      {|syntax error, unexpected "<"|}
      (
        Hashtbl.find forest.documents parse_error_uri
        |> Parse.parse_document
        |> Result.get_error |> fun d ->
        Asai.Diagnostic.string_of_text d.explanation.value
      )
  in
  let open Alcotest in
  run
    "verify error reporting"
    [
      "parsing",
      [
        test_case "diagnostic" `Quick test
      ];
      "expanding",
      [
      ];
      "evaluating",
      [
      ];
    ]
