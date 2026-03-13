(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

module Rust_parser = Forester_parser.Rust_parser
module Support = Forester_test.Test_support

let string_contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let expect_contains ~label ~needle haystack =
  Alcotest.(check bool) label true (string_contains ~needle haystack)

let with_rust_parser_settings ?timeout path f =
  let previous_path = !Rust_parser.rust_parser_path in
  let previous_timeout = !Rust_parser.rust_parser_timeout_seconds in
  Fun.protect
    ~finally: (fun () ->
      Rust_parser.set_rust_parser_path previous_path;
      Rust_parser.set_rust_parser_timeout_seconds previous_timeout
    )
    @@ fun () ->
    Rust_parser.set_rust_parser_path path;
    Option.iter Rust_parser.set_rust_parser_timeout_seconds timeout;
    f ()

let with_temp_script body =
  Support.with_temp_script ~prefix: "forester-rust-parser-test-" body

let test_is_available_accepts_exit_zero_without_stdout () =
  with_temp_script "exit 0\n" @@ fun script_path ->
  with_rust_parser_settings script_path @@ fun () ->
  Alcotest.(check bool)
    "exit-zero parser should count as available"
    true
    (Rust_parser.is_available ())

let test_parse_reports_unavailable_binary_predictably () =
  let missing_path =
    Filename.concat (Filename.get_temp_dir_name ()) "missing-rust-parser"
  in
  with_rust_parser_settings missing_path @@ fun () ->
  Alcotest.(check bool)
    "missing parser is unavailable"
    false
    (Rust_parser.is_available ());
  (
    match Rust_parser.parse "\\title{Hello}" with
    | Ok _ ->
      Alcotest.fail "Expected strict parse to fail when parser is unavailable"
    | Error [error] ->
      expect_contains
        ~label: "strict unavailable message"
        ~needle: "is unavailable"
        error.message;
      expect_contains
        ~label: "strict unavailable path"
        ~needle: missing_path
        error.message;
      Alcotest.(check bool)
        "strict unavailable details"
        true
        (error.details = None)
    | Error errors ->
      Alcotest.failf
        "Expected one unavailable error, got %d"
        (List.length errors)
  );
  (
    match Rust_parser.parse_recovery "\\title{Hello}" with
    | Rust_parser.Failed [error] ->
      expect_contains
        ~label: "recovery unavailable message"
        ~needle: "is unavailable"
        error.message
    | Rust_parser.Parsed _ | Rust_parser.Recovered _ ->
      Alcotest.fail "Expected recovery parse to fail when parser is unavailable"
    | Rust_parser.Failed errors ->
      Alcotest.failf
        "Expected one recovery unavailable error, got %d"
        (List.length errors)
  );
  match Rust_parser.parse_check "\\title{Hello}" with
  | Ok() ->
    Alcotest.fail "Expected parse_check to fail when parser is unavailable"
  | Error message ->
    expect_contains
      ~label: "parse_check unavailable message"
      ~needle: "is unavailable"
      message

let test_parse_surfaces_exit_code_and_split_streams () =
  with_temp_script
    {|
printf 'fatal stderr\n' >&2
printf 'debug stdout\n'
exit 17
|}
    @@ fun script_path ->
    with_rust_parser_settings script_path @@ fun () ->
    match Rust_parser.parse "\\title{Hello}" with
    | Ok _ -> Alcotest.fail "Expected strict parse to fail on nonzero exit"
    | Error [error] ->
      expect_contains
        ~label: "exit code included"
        ~needle: "exited with code 17"
        error.message;
      expect_contains
        ~label: "stderr included"
        ~needle: "stderr:\nfatal stderr"
        error.message;
      expect_contains
        ~label: "stdout included"
        ~needle: "stdout:\ndebug stdout"
        error.message;
      Alcotest.(check string)
        "report mirrors message"
        error.message
        error.report
    | Error errors ->
      Alcotest.failf
        "Expected one process-exit error, got %d"
        (List.length errors)

  let test_parse_times_out_and_returns_predictable_error () =
    with_temp_script "sleep 5\n" @@ fun script_path ->
    with_rust_parser_settings ~timeout: 0.1 script_path @@ fun () ->
    let started_at = Unix.gettimeofday () in
    match Rust_parser.parse "\\title{Hello}" with
    | Ok _ -> Alcotest.fail "Expected strict parse to fail on timeout"
    | Error [error] ->
      let elapsed = Unix.gettimeofday () -. started_at in
      Alcotest.(check bool)
        "timeout should trigger promptly"
        true
        (elapsed < 2.0);
      expect_contains
        ~label: "timeout message"
        ~needle: "timed out after 0.10s"
        error.message
    | Error errors ->
      Alcotest.failf "Expected one timeout error, got %d" (List.length errors)

  let test_parse_reports_invalid_json_cleanly () =
    with_temp_script "printf 'not-json'\n" @@ fun script_path ->
    with_rust_parser_settings script_path @@ fun () ->
    match Rust_parser.parse "\\title{Hello}" with
    | Ok _ -> Alcotest.fail "Expected invalid JSON output to fail"
    | Error [error] ->
      expect_contains
        ~label: "invalid json message"
        ~needle: "Parse error:"
        error.message
    | Error errors ->
      Alcotest.failf
        "Expected one invalid-json error, got %d"
        (List.length errors)

  let () =
    Alcotest.run
      "Rust parser process"
      [
        (
          "process",
          [Alcotest.test_case
            "availability uses exit status"
            `Quick
            test_is_available_accepts_exit_zero_without_stdout;
          Alcotest.test_case
            "unavailable parser fallback"
            `Quick
            test_parse_reports_unavailable_binary_predictably;
          Alcotest.test_case
            "exit code and stream classification"
            `Quick
            test_parse_surfaces_exit_code_and_split_streams;
          Alcotest.test_case
            "timeout handling"
            `Quick
            test_parse_times_out_and_returns_predictable_error;
          Alcotest.test_case
            "invalid json classification"
            `Quick
            test_parse_reports_invalid_json_cleanly;
          ]
        );
      ]
