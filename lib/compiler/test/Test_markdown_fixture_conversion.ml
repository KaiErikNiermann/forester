(******************************************************************************)
(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors             *)
(* SPDX-License-Identifier: GPL-3.0-or-later                                  *)
(******************************************************************************)

open Forester_compiler

let is_executable_file path =
  (not (Sys.is_directory path))
  && (
    try
      Unix.access path [Unix.X_OK];
      true
    with
      | Unix.Unix_error _ -> false
  )

let path_entries () =
  match Sys.getenv_opt "PATH" with
  | None -> []
  | Some value ->
    value
    |> String.split_on_char ':'
    |> List.filter (fun entry -> String.trim entry <> "")

let find_in_path executable =
  path_entries ()
  |> List.find_map (fun directory ->
      let candidate = Filename.concat directory executable in
      if Sys.file_exists candidate && is_executable_file candidate then
        Some candidate
      else None
    )

let resolve_converter_path () =
  match Sys.getenv_opt "FORESTER_PANDOC_PATH" with
  | Some path when String.trim path <> "" ->
    if Sys.file_exists path && is_executable_file path then
      Some path
    else
      None
  | _ -> find_in_path "forester-pandoc"

let bool_of_env name =
  match Sys.getenv_opt name with
  | Some "1"
  | Some "true"
  | Some "yes"
  | Some "on" ->
    true
  | _ -> false

let rec find_repo_root dir =
  let dune_project = Filename.concat dir "dune-project" in
  let opam_file = Filename.concat dir "forester.opam" in
  if Sys.file_exists dune_project && Sys.file_exists opam_file then
    Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then
      None
    else
      find_repo_root parent

let repo_root () =
  match find_repo_root (Sys.getcwd ()) with
  | Some root -> root
  | None -> Alcotest.fail "Unable to locate repository root from current directory"

let read_file path = In_channel.with_open_bin path In_channel.input_all

let trim s =
  let is_space = function
    | ' '
    | '\t'
    | '\n'
    | '\r' ->
      true
    | _ -> false
  in
  let len = String.length s in
  let rec left i =
    if i >= len then
      len
    else if is_space s.[i] then
      left (i + 1)
    else
      i
  in
  let rec right i =
    if i < 0 then
      -1
    else if is_space s.[i] then
      right (i - 1)
    else
      i
  in
  let i = left 0 in
  let j = right (len - 1) in
  if j < i then
    ""
  else
    String.sub s i ((j - i) + 1)

let strip_comment line =
  match String.index_opt line '#' with
  | None -> line
  | Some i -> String.sub line 0 i

type fixture_case = {
  stem: string;
  markdown_path: string;
  forester_path: string;
}

let load_fixture_cases () =
  let fixtures_root =
    repo_root () |> fun root -> Filename.concat root "tools/pandoc-converter/fixtures/markdown"
  in
  let manifest_path = Filename.concat fixtures_root "manifest.txt" in
  let stems =
    read_file manifest_path
    |> String.split_on_char '\n'
    |> List.map strip_comment
    |> List.map trim
    |> List.filter (fun line -> line <> "")
  in
  List.map
    (fun stem ->
      {
        stem;
        markdown_path = Filename.concat fixtures_root (stem ^ ".md");
        forester_path = Filename.concat fixtures_root (stem ^ ".forester");
      }
    )
    stems

let run_command_capture command =
  let input_channel = Unix.open_process_in command in
  let output = Buffer.create 1024 in
  (
    try
      while true do
        Buffer.add_string output (input_line input_channel);
        Buffer.add_char output '\n'
      done
    with
      | End_of_file -> ()
  );
  let status = Unix.close_process_in input_channel in
  status, Buffer.contents output

let run_converter ~converter_path ~markdown_path =
  let command =
    Printf.sprintf
      "%s markdown-to-forester --diagnostics-format none %s 2>&1"
      (Filename.quote converter_path)
      (Filename.quote markdown_path)
  in
  run_command_capture command

let with_temp_markdown content f =
  let path = Filename.temp_file "forester_markdown_fuzz_" ".md" in
  let oc = open_out_bin path in
  Fun.protect
    ~finally: (fun () ->
      close_out_noerr oc;
      if Sys.file_exists path then
        Sys.remove path
    )
    @@ fun () ->
    output_string oc content;
    close_out oc;
    f path

let parse_forester_or_fail ~fixture output =
  match Parse.parse_content ~filename: (fixture ^ ".tree") output with
  | Ok _ -> ()
  | Error diagnostic ->
    let explanation =
      Asai.Diagnostic.string_of_text diagnostic.Asai.Diagnostic.explanation.value
    in
    Alcotest.failf
      "Fixture %s produced Forester that failed parser validation:\n%s"
      fixture
      explanation

let rec find_substring haystack needle start =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  if needle_len = 0 then
    Some start
  else if start + needle_len > haystack_len then
    None
  else if String.sub haystack start needle_len = needle then
    Some start
  else
    find_substring haystack needle (start + 1)

let assert_well_formed_verbatim ~fixture output =
  let output_len = String.length output in
  let rec loop index =
    if index >= output_len then
        ()
    else
      match find_substring output "\\verb" index with
      | None -> ()
      | Some start ->
        let herald_start = start + 5 in
        let rec scan_for_pipe i =
          if i >= output_len then
            None
          else
            match output.[i] with
            | '|' -> Some i
            | ' '
            | '\t'
            | '\n'
            | '\r' ->
              None
            | _ -> scan_for_pipe (i + 1)
        in
        (
          match scan_for_pipe herald_start with
          | None ->
            Alcotest.failf
              "Fixture %s produced malformed verbatim opener near index %d"
              fixture
              start
          | Some pipe_index ->
            let herald_len = pipe_index - herald_start in
            if herald_len <= 0 then
              Alcotest.failf
                "Fixture %s produced empty verbatim herald near index %d"
                fixture
                start
            else
              loop (pipe_index + 1)
        )
  in
  if find_substring output "\\verb<|" 0 <> None then
    Alcotest.failf "Fixture %s contains known bad verbatim boundary token \\verb<|" fixture
  else
    loop 0

let assert_closed_math_delimiters ~fixture output =
  let rec check_from needle index =
    match find_substring output needle index with
    | None -> ()
    | Some start ->
      let after_delimiter = start + String.length needle in
      let maybe_close = find_substring output "}" after_delimiter in
      (
        match maybe_close with
        | None ->
          Alcotest.failf
            "Fixture %s has unterminated math delimiter starting at index %d"
            fixture
            start
        | Some close_index -> check_from needle (close_index + 1)
      )
  in
  check_from "##{" 0;
  check_from "#{" 0

let assert_no_known_hazards ~fixture output =
  assert_well_formed_verbatim ~fixture output;
  assert_closed_math_delimiters ~fixture output

let next_seed seed =
  let value =
    Int64.(
      add (mul 1103515245L (of_int seed)) 12345L
      |> rem 2147483647L
    )
  in
  Int64.to_int value

let pick choices seed =
  choices.(seed mod Array.length choices)

let generate_markdown_case seed =
  let words = [|"atlas"; "bridge"; "compass"; "delta"; "echo"; "forest"; "glyph"; "harbor"|] in
  let symbols = [|"{"; "}"; "["; "]"; "#"; "%"; "|"; "~"|] in
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
    Printf.sprintf
      "Random *%s* with **%s** and `%s|%s` plus symbol %s."
      noun1
      noun2
      noun1
      noun2
      symbol
  in
  let list_block =
    if seed4 mod 2 = 0 then
      Printf.sprintf
        "\\n- item %s\\n- item [%s](%s/%s)"
        noun1
        noun2
        noun1
        noun2
    else
      ""
  in
  let math_block =
    if seed5 mod 3 = 0 then
      "\\n\\nEquation: $x^2 + y^2$."
    else
      ""
  in
  let footnote_block =
    if seed3 mod 4 = 0 then
      "\\n\\nWith note.[^n]\\n\\n[^n]: generated note body"
    else
      ""
  in
  Printf.sprintf
    "# %s\\n\\n%s%s%s%s\\n"
    title
    base_paragraph
    list_block
    math_block
    footnote_block

let with_converter_or_skip test_name f =
  match resolve_converter_path () with
  | Some converter_path -> f converter_path
  | None ->
    if bool_of_env "FORESTER_PANDOC_REQUIRE_BINARY" then
      Alcotest.fail
        (
          Printf.sprintf
            "%s requires converter binary; set FORESTER_PANDOC_PATH or install forester-pandoc"
            test_name
        )
    else
      (
        Printf.eprintf
          "%s skipped because converter binary is unavailable (set FORESTER_PANDOC_REQUIRE_BINARY=1 to enforce)\n%!"
          test_name;
        Alcotest.skip ()
      )

let test_fixture_goldens () =
  with_converter_or_skip "golden fixture parity" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, actual = run_converter ~converter_path ~markdown_path: fixture.markdown_path in
      let expected = read_file fixture.forester_path in
      (
        match status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED code ->
          Alcotest.failf
            "Fixture %s converter exited with code %d:\n%s"
            fixture.stem
            code
            actual
        | Unix.WSIGNALED signal_number ->
          Alcotest.failf
            "Fixture %s converter terminated by signal %d"
            fixture.stem
            signal_number
        | Unix.WSTOPPED signal_number ->
          Alcotest.failf
            "Fixture %s converter stopped by signal %d"
            fixture.stem
            signal_number
      );
      Alcotest.(check string)
        ("golden output " ^ fixture.stem)
        expected
        actual
    )
    (load_fixture_cases ())

let test_fixture_outputs_parse () =
  with_converter_or_skip "fixture parser validity" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, output = run_converter ~converter_path ~markdown_path: fixture.markdown_path in
      (
        match status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED code ->
          Alcotest.failf
            "Fixture %s converter exited with code %d:\n%s"
            fixture.stem
            code
            output
        | Unix.WSIGNALED signal_number ->
          Alcotest.failf
            "Fixture %s converter terminated by signal %d"
            fixture.stem
            signal_number
        | Unix.WSTOPPED signal_number ->
          Alcotest.failf
            "Fixture %s converter stopped by signal %d"
            fixture.stem
            signal_number
      );
      parse_forester_or_fail ~fixture: fixture.stem output
    )
    (load_fixture_cases ())

let test_fixture_hazards () =
  with_converter_or_skip "fixture hazard checks" @@ fun converter_path ->
  List.iter
    (fun fixture ->
      let status, output = run_converter ~converter_path ~markdown_path: fixture.markdown_path in
      (
        match status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED code ->
          Alcotest.failf
            "Fixture %s converter exited with code %d:\n%s"
            fixture.stem
            code
            output
        | Unix.WSIGNALED signal_number ->
          Alcotest.failf
            "Fixture %s converter terminated by signal %d"
            fixture.stem
            signal_number
        | Unix.WSTOPPED signal_number ->
          Alcotest.failf
            "Fixture %s converter stopped by signal %d"
            fixture.stem
            signal_number
      );
      assert_no_known_hazards ~fixture: fixture.stem output
    )
    (load_fixture_cases ())

let test_randomized_markdown_generation () =
  with_converter_or_skip "randomized markdown generation" @@ fun converter_path ->
  for seed = 0 to 119 do
    let markdown = generate_markdown_case seed in
    with_temp_markdown markdown @@ fun markdown_path ->
    let status, output = run_converter ~converter_path ~markdown_path in
    (
      match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
        Alcotest.failf
          "Fuzz seed %d converter exited with code %d:\\n%s\\nInput:\\n%s"
          seed
          code
          output
          markdown
      | Unix.WSIGNALED signal_number ->
        Alcotest.failf
          "Fuzz seed %d converter terminated by signal %d"
          seed
          signal_number
      | Unix.WSTOPPED signal_number ->
        Alcotest.failf
          "Fuzz seed %d converter stopped by signal %d"
          seed
          signal_number
    );
    parse_forester_or_fail ~fixture: (Printf.sprintf "fuzz-%d" seed) output;
    assert_no_known_hazards ~fixture: (Printf.sprintf "fuzz-%d" seed) output
  done

let () =
  let open Alcotest in
  run
    "Markdown fixture converter integration"
    [
      (
        "fixtures",
        [test_case "golden parity" `Quick test_fixture_goldens;
        test_case "parser validity" `Quick test_fixture_outputs_parse;
        test_case "known hazard checks" `Quick test_fixture_hazards;
        test_case "randomized markdown generation" `Quick test_randomized_markdown_generation;
        ]
      );
    ]
