(******************************************************************************)
(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors             *)
(* SPDX-License-Identifier: GPL-3.0-or-later                                  *)
(******************************************************************************)

open Forester_core
open Forester_compiler
open Forester_test
open Testables

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

module Support = Forester_test.Test_support

let with_temp_script ~body =
  Support.with_temp_script ~prefix:"forester_mock_converter_" ~header:"" body

let with_converter_path path f =
  let old = Sys.getenv_opt "FORESTER_PANDOC_PATH" in
  Unix.putenv "FORESTER_PANDOC_PATH" path;
  Fun.protect
    ~finally:(fun () ->
      match old with
      | Some value -> Unix.putenv "FORESTER_PANDOC_PATH" value
      | None -> Unix.putenv "FORESTER_PANDOC_PATH" "")
    f

let expect_external_error ~label = function
  | Ok _ -> Alcotest.fail (label ^ ": expected Error, got Ok")
  | Error (diagnostic : diagnostic) ->
      Alcotest.(check message)
        (label ^ ": message class")
        Reporter.Message.External_error diagnostic.message;
      diagnostic

let test_markdown_parse_success () =
  with_temp_script
    ~body:
      {|#!/usr/bin/env sh
set -eu
command="$1"
if [ "$command" != "markdown-to-forester" ]; then
  echo "unexpected command: $command" >&2
  exit 3
fi
cat <<'OUT'
\title{From Markdown}

\p{Converted content}
OUT
|}
  @@ fun script_path ->
  with_converter_path script_path @@ fun () ->
  let result =
    Parse.parse_content ~filename:"sample.md" "# ignored by fixture"
  in
  Alcotest.(check bool)
    "markdown conversion returns Ok" true (Result.is_ok result)

let test_missing_converter_is_external_error () =
  with_converter_path "/tmp/forester_converter_missing_binary" @@ fun () ->
  let result = Parse.parse_content ~filename:"missing.md" "# hello" in
  ignore (expect_external_error ~label:"missing converter" result)

let test_non_zero_converter_exit_is_external_error () =
  with_temp_script
    ~body:
      {|#!/usr/bin/env sh
set -eu
echo "converter fixture failure" >&2
exit 17
|}
  @@ fun script_path ->
  with_converter_path script_path @@ fun () ->
  let result = Parse.parse_content ~filename:"nonzero.md" "# hello" in
  ignore (expect_external_error ~label:"non-zero converter exit" result)

let test_invalid_converter_output_is_external_error () =
  with_temp_script ~body:{|#!/usr/bin/env sh
set -eu
cat <<'OUT'
\verb<|
OUT
|}
  @@ fun script_path ->
  with_converter_path script_path @@ fun () ->
  let result = Parse.parse_content ~filename:"invalid-output.md" "# hello" in
  ignore (expect_external_error ~label:"invalid converter output" result)

let () =
  let open Alcotest in
  run "Markdown conversion parser integration"
    [
      ( "parse-content",
        [
          test_case "markdown conversion success" `Quick
            test_markdown_parse_success;
          test_case "missing converter reports external error" `Quick
            test_missing_converter_is_external_error;
          test_case "non-zero converter exit reports external error" `Quick
            test_non_zero_converter_exit_is_external_error;
          test_case "invalid converter output reports external error" `Quick
            test_invalid_converter_output_is_external_error;
        ] );
    ]
