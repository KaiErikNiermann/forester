(******************************************************************************)
(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors             *)
(* SPDX-License-Identifier: GPL-3.0-or-later                                  *)
(******************************************************************************)

open Forester_compiler
open Forester_core
open Forester_test
module Rust_parser = Forester_parser.Rust_parser
module Support = Forester_test.Test_support

let resolve_converter_path () =
  Support.resolve_binary ~env_var:"FORESTER_PANDOC_PATH" "forester-pandoc"

let resolve_rust_parser_path () =
  let root = Support.repo_root () in
  Support.resolve_binary ~env_var:"FORESTER_RUST_PARSER_PATH"
    ~candidates:
      [
        Filename.concat root
          "tools/rust-parser/target/debug/forester-rust-parser";
        Filename.concat root
          "tools/rust-parser/target/release/forester-rust-parser";
      ]
    "forester-rust-parser"

type fixture_case = {
  stem : string;
  markdown_path : string;
  forester_path : string;
}

let load_fixture_cases () =
  let fixtures_root =
    Support.repo_root () |> fun root ->
    Filename.concat root "tools/pandoc-converter/fixtures/markdown"
  in
  let manifest_path = Filename.concat fixtures_root "manifest.txt" in
  let stems = Support.load_manifest_entries manifest_path in
  List.map
    (fun stem ->
      {
        stem;
        markdown_path = Filename.concat fixtures_root (stem ^ ".md");
        forester_path = Filename.concat fixtures_root (stem ^ ".forester");
      })
    stems

let run_converter ~converter_path ~markdown_path =
  let command =
    Printf.sprintf "%s markdown-to-forester --diagnostics-format none %s 2>&1"
      (Filename.quote converter_path)
      (Filename.quote markdown_path)
  in
  Support.run_command_capture command

let with_temp_markdown content f =
  Support.with_temp_file ~prefix:"forester_markdown_fuzz_" ~suffix:".md" content
    f

let parse_forester_or_fail ~fixture output =
  match Parse.parse_content ~filename:(fixture ^ ".tree") output with
  | Ok _ -> ()
  | Error diagnostic ->
      let explanation =
        Asai.Diagnostic.string_of_text
          diagnostic.Asai.Diagnostic.explanation.value
      in
      Alcotest.failf
        "Fixture %s produced Forester that failed parser validation:\n%s"
        fixture explanation

let rec strip_code_locations (code : Code.t) : Code.t =
  List.map
    (fun ({ Asai.Range.value; _ } : Code.node Asai.Range.located) ->
      { Asai.Range.loc = None; value = Code.map strip_code_locations value })
    code

let rec normalize_bridge_shape (code : Code.t) : Code.t =
  let is_glue_after = function '/' | '-' | '@' | '\'' -> true | _ -> false in
  let is_glue_before = function
    | '/' | ':' | '-' | '@' | '.' | ',' | ';' | '!' | '?' | '\'' -> true
    | _ -> false
  in
  let escaped_literal_text = function
    | ([ "{" ] | [ "}" ] | [ "[" ] | [ "]" ] | [ "#" ] | [ "%" ] | [ "\\" ]) as
      path ->
        Some (List.hd path)
    | _ -> None
  in
  let collapse_text_nodes nodes =
    let rec take_text_run acc = function
      | ({ Asai.Range.value = Code.Text text; _ } :
          Code.node Asai.Range.located)
        :: rest ->
          take_text_run (text :: acc) rest
      | rest -> (List.rev acc, rest)
    in
    let rec loop acc = function
      | [] -> List.rev acc
      | ({ Asai.Range.value = Code.Text text; _ } :
          Code.node Asai.Range.located)
        :: rest ->
          let texts, tail = take_text_run [ text ] rest in
          let normalized_text =
            let buffer = Buffer.create 32 in
            let pending_space = ref false in
            List.iter
              (fun value ->
                if String.trim value = "" then pending_space := true
                else (
                  (if Buffer.length buffer > 0 then
                     let previous_char =
                       Buffer.nth buffer (Buffer.length buffer - 1)
                     in
                     let current_char = value.[0] in
                     if
                       !pending_space
                       || not
                            (is_glue_after previous_char
                            || is_glue_before current_char)
                     then Buffer.add_char buffer ' ');
                  Buffer.add_string buffer value;
                  pending_space := false))
              texts;
            Buffer.contents buffer
          in
          if normalized_text = "" then loop acc tail
          else
            loop
              ({ Asai.Range.loc = None; value = Code.Text normalized_text }
              :: acc)
              tail
      | node :: rest -> loop (node :: acc) rest
    in
    loop [] nodes
  in
  let normalize_node ({ Asai.Range.value; loc } : Code.node Asai.Range.located)
      =
    let normalized_value =
      match value with
      | Code.Ident path -> (
          match escaped_literal_text path with
          | Some text -> Code.Text text
          | None -> value)
      | _ -> Code.map normalize_bridge_shape value
    in
    { Asai.Range.loc; value = normalized_value }
  in
  let normalized_nodes = List.map normalize_node code in
  List.concat_map
    (fun ({ Asai.Range.value; _ } as node) ->
      match value with
      | Code.Group
          (Braces, ({ Asai.Range.value = Code.Ident _; _ } :: _ as body)) ->
          body
      | _ -> [ node ])
    normalized_nodes
  |> collapse_text_nodes

let render_code_shape (code : Code.t) = Format.asprintf "%a" Code.pp code

let parse_forester_with_ocaml_or_fail ~fixture output =
  match Parse.parse_content ~filename:(fixture ^ ".tree") output with
  | Ok code -> code
  | Error diagnostic ->
      let explanation =
        Asai.Diagnostic.string_of_text
          diagnostic.Asai.Diagnostic.explanation.value
      in
      Alcotest.failf
        "Fixture %s produced Forester that failed OCaml parser validation:\n%s"
        fixture explanation

let parse_forester_with_rust_or_fail ~rust_parser_path ~fixture output =
  Rust_parser.set_rust_parser_path rust_parser_path;
  match Rust_parser.parse output with
  | Ok code -> code
  | Error errors ->
      let rendered_errors =
        errors
        |> List.map (fun (error : Rust_parser.parse_error) ->
            if error.report <> "" then error.report else error.message)
        |> String.concat "\n\n"
      in
      Alcotest.failf
        "Fixture %s produced Forester that failed Rust parser validation:\n%s"
        fixture rendered_errors

let rec find_substring haystack needle start =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then Some start
  else if start + needle_len > haystack_len then None
  else if String.sub haystack start needle_len = needle then Some start
  else find_substring haystack needle (start + 1)

let assert_well_formed_verbatim ~fixture output =
  let output_len = String.length output in
  let rec loop index =
    if index >= output_len then ()
    else
      match find_substring output "\\verb" index with
      | None -> ()
      | Some start -> (
          let herald_start = start + 5 in
          let rec scan_for_pipe i =
            if i >= output_len then None
            else
              match output.[i] with
              | '|' -> Some i
              | ' ' | '\t' | '\n' | '\r' -> None
              | _ -> scan_for_pipe (i + 1)
          in
          match scan_for_pipe herald_start with
          | None ->
              Alcotest.failf
                "Fixture %s produced malformed verbatim opener near index %d"
                fixture start
          | Some pipe_index ->
              let herald_len = pipe_index - herald_start in
              if herald_len <= 0 then
                Alcotest.failf
                  "Fixture %s produced empty verbatim herald near index %d"
                  fixture start
              else loop (pipe_index + 1))
  in
  if find_substring output "\\verb<|" 0 <> None then
    Alcotest.failf
      "Fixture %s contains known bad verbatim boundary token \\verb<|" fixture
  else loop 0

let assert_closed_math_delimiters ~fixture output =
  let rec check_from needle index =
    match find_substring output needle index with
    | None -> ()
    | Some start -> (
        let after_delimiter = start + String.length needle in
        let maybe_close = find_substring output "}" after_delimiter in
        match maybe_close with
        | None ->
            Alcotest.failf
              "Fixture %s has unterminated math delimiter starting at index %d"
              fixture start
        | Some close_index -> check_from needle (close_index + 1))
  in
  check_from "##{" 0;
  check_from "#{" 0

let assert_no_known_hazards ~fixture output =
  assert_well_formed_verbatim ~fixture output;
  assert_closed_math_delimiters ~fixture output

let next_seed seed =
  let value =
    Int64.(add (mul 1103515245L (of_int seed)) 12345L |> rem 2147483647L)
  in
  Int64.to_int value

let pick choices seed = choices.(seed mod Array.length choices)

let generate_markdown_case seed =
  let words =
    [|
      "atlas"; "bridge"; "compass"; "delta"; "echo"; "forest"; "glyph"; "harbor";
    |]
  in
  let symbols = [| "{"; "}"; "["; "]"; "#"; "%"; "|"; "~" |] in
  let seed1 = next_seed seed in
  let seed2 = next_seed seed1 in
  let seed3 = next_seed seed2 in
  let seed4 = next_seed seed3 in
  let seed5 = next_seed seed4 in
  let title = String.capitalize_ascii (pick words seed1) in
  let noun1 = pick words seed2 in
  let noun2 = pick words seed3 in
  let symbol = pick symbols seed4 in
  let base_paragraph =
    Printf.sprintf "Random *%s* with **%s** and `%s|%s` plus symbol %s." noun1
      noun2 noun1 noun2 symbol
  in
  let list_block =
    if seed4 mod 2 = 0 then
      Printf.sprintf "\\n- item %s\\n- item [%s](%s/%s)" noun1 noun2 noun1 noun2
    else ""
  in
  let math_block =
    if seed5 mod 3 = 0 then "\\n\\nEquation: $x^2 + y^2$." else ""
  in
  let footnote_block =
    if seed3 mod 4 = 0 then
      "\\n\\nWith note.[^n]\\n\\n[^n]: generated note body"
    else ""
  in
  Printf.sprintf "# %s\\n\\n%s%s%s%s\\n" title base_paragraph list_block
    math_block footnote_block

let with_converter_or_skip test_name f =
  match resolve_converter_path () with
  | Some converter_path -> f converter_path
  | None ->
      if Support.bool_of_env "FORESTER_PANDOC_REQUIRE_BINARY" then
        Alcotest.fail
          (Printf.sprintf
             "%s requires converter binary; set FORESTER_PANDOC_PATH or \
              install forester-pandoc"
             test_name)
      else (
        Printf.eprintf
          "%s skipped because converter binary is unavailable (set \
           FORESTER_PANDOC_REQUIRE_BINARY=1 to enforce)\n\
           %!"
          test_name;
        Alcotest.skip ())

let with_rust_parser_or_skip test_name f =
  match resolve_rust_parser_path () with
  | Some rust_parser_path -> f rust_parser_path
  | None ->
      if Support.bool_of_env "FORESTER_RUST_PARSER_REQUIRE_BINARY" then
        Alcotest.fail
          (Printf.sprintf
             "%s requires rust parser binary; set FORESTER_RUST_PARSER_PATH or \
              build tools/rust-parser"
             test_name)
      else (
        Printf.eprintf
          "%s skipped because rust parser binary is unavailable (set \
           FORESTER_RUST_PARSER_REQUIRE_BINARY=1 to enforce)\n\
           %!"
          test_name;
        Alcotest.skip ())

let test_fixture_goldens () =
  with_converter_or_skip "golden fixture parity" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, actual =
        run_converter ~converter_path ~markdown_path:fixture.markdown_path
      in
      let expected = Support.read_file fixture.forester_path in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "Fixture %s converter exited with code %d:\n%s"
            fixture.stem code actual
      | Unix.WSIGNALED signal_number ->
          Alcotest.failf "Fixture %s converter terminated by signal %d"
            fixture.stem signal_number
      | Unix.WSTOPPED signal_number ->
          Alcotest.failf "Fixture %s converter stopped by signal %d"
            fixture.stem signal_number);
      Alcotest.(check string) ("golden output " ^ fixture.stem) expected actual)
    (load_fixture_cases ())

let test_fixture_outputs_parse () =
  with_converter_or_skip "fixture parser validity" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, output =
        run_converter ~converter_path ~markdown_path:fixture.markdown_path
      in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "Fixture %s converter exited with code %d:\n%s"
            fixture.stem code output
      | Unix.WSIGNALED signal_number ->
          Alcotest.failf "Fixture %s converter terminated by signal %d"
            fixture.stem signal_number
      | Unix.WSTOPPED signal_number ->
          Alcotest.failf "Fixture %s converter stopped by signal %d"
            fixture.stem signal_number);
      parse_forester_or_fail ~fixture:fixture.stem output)
    (load_fixture_cases ())

let test_fixture_hazards () =
  with_converter_or_skip "fixture hazard checks" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, output =
        run_converter ~converter_path ~markdown_path:fixture.markdown_path
      in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "Fixture %s converter exited with code %d:\n%s"
            fixture.stem code output
      | Unix.WSIGNALED signal_number ->
          Alcotest.failf "Fixture %s converter terminated by signal %d"
            fixture.stem signal_number
      | Unix.WSTOPPED signal_number ->
          Alcotest.failf "Fixture %s converter stopped by signal %d"
            fixture.stem signal_number);
      assert_no_known_hazards ~fixture:fixture.stem output)
    (load_fixture_cases ())

let test_randomized_markdown_generation () =
  with_converter_or_skip "randomized markdown generation"
  @@ fun converter_path ->
  for seed = 0 to 119 do
    let markdown = generate_markdown_case seed in
    with_temp_markdown markdown @@ fun markdown_path ->
    let status, output = run_converter ~converter_path ~markdown_path in
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code ->
        Alcotest.failf
          "Fuzz seed %d converter exited with code %d:\\n%s\\nInput:\\n%s" seed
          code output markdown
    | Unix.WSIGNALED signal_number ->
        Alcotest.failf "Fuzz seed %d converter terminated by signal %d" seed
          signal_number
    | Unix.WSTOPPED signal_number ->
        Alcotest.failf "Fuzz seed %d converter stopped by signal %d" seed
          signal_number);
    parse_forester_or_fail ~fixture:(Printf.sprintf "fuzz-%d" seed) output;
    assert_no_known_hazards ~fixture:(Printf.sprintf "fuzz-%d" seed) output
  done

let test_dual_parser_equivalence () =
  with_converter_or_skip "dual parser equivalence" @@ fun converter_path ->
  with_rust_parser_or_skip "dual parser equivalence" @@ fun rust_parser_path ->
  let checked_fixtures = ref 0 in
  List.iter
    (fun fixture ->
      let status, output =
        run_converter ~converter_path ~markdown_path:fixture.markdown_path
      in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "Fixture %s converter exited with code %d:\n%s"
            fixture.stem code output
      | Unix.WSIGNALED signal_number ->
          Alcotest.failf "Fixture %s converter terminated by signal %d"
            fixture.stem signal_number
      | Unix.WSTOPPED signal_number ->
          Alcotest.failf "Fixture %s converter stopped by signal %d"
            fixture.stem signal_number);
      if find_substring output "\\verb" 0 <> None then
        Printf.eprintf
          "dual parser equivalence skipped for %s because current Rust parser \
           parity does not cover verbatim output yet\n\
           %!"
          fixture.stem
      else (
        incr checked_fixtures;
        let ocaml_code =
          parse_forester_with_ocaml_or_fail ~fixture:fixture.stem output
          |> strip_code_locations |> normalize_bridge_shape
        in
        let rust_code =
          parse_forester_with_rust_or_fail ~rust_parser_path
            ~fixture:fixture.stem output
          |> strip_code_locations |> normalize_bridge_shape
        in
        Alcotest.(check string)
          ("dual parser shape " ^ fixture.stem)
          (render_code_shape ocaml_code)
          (render_code_shape rust_code)))
    (load_fixture_cases ());
  Alcotest.(check bool)
    "dual parser equivalence checked at least one fixture" true
    (!checked_fixtures > 0)

let () =
  let open Alcotest in
  run "Markdown fixture converter integration"
    [
      ( "fixtures",
        [
          test_case "golden parity" `Quick test_fixture_goldens;
          test_case "parser validity" `Quick test_fixture_outputs_parse;
          test_case "known hazard checks" `Quick test_fixture_hazards;
          test_case "dual parser equivalence" `Quick
            test_dual_parser_equivalence;
          test_case "randomized markdown generation" `Quick
            test_randomized_markdown_generation;
        ] );
    ]
