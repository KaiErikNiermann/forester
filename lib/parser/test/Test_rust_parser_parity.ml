(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
open Forester_core
open Testables
module Rust_parser = Forester_parser.Rust_parser
module Lsp_shims = Forester_lsp.Lsp_shims

let bool_of_env name =
  match Sys.getenv_opt name with
  | Some "1" | Some "true" | Some "yes" | Some "on" -> true
  | _ -> false

let rec find_repo_root dir =
  let dune_project = Filename.concat dir "dune-project" in
  let opam_file = Filename.concat dir "forester.opam" in
  if Sys.file_exists dune_project && Sys.file_exists opam_file then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_repo_root parent

let repo_root () =
  match find_repo_root (Sys.getcwd ()) with
  | Some root -> root
  | None ->
      Alcotest.fail "Unable to locate repository root from current directory"

let read_file path = In_channel.with_open_bin path In_channel.input_all

let resolve_rust_parser_path () =
  let root = repo_root () in
  let local_candidates =
    [
      Filename.concat root "tools/rust-parser/target/debug/forester-rust-parser";
      Filename.concat root
        "tools/rust-parser/target/release/forester-rust-parser";
    ]
  in
  match Sys.getenv_opt "FORESTER_RUST_PARSER_PATH" with
  | Some path when String.trim path <> "" && Sys.file_exists path -> Some path
  | _ -> List.find_opt Sys.file_exists local_candidates

let with_rust_parser_or_skip test_name f =
  match resolve_rust_parser_path () with
  | Some rust_parser_path ->
      Rust_parser.set_rust_parser_path rust_parser_path;
      if Rust_parser.is_available () then f rust_parser_path
      else if bool_of_env "FORESTER_RUST_PARSER_REQUIRE_BINARY" then
        Alcotest.failf "%s requires rust parser binary at %s" test_name
          rust_parser_path
      else
        Printf.eprintf
          "%s skipped because rust parser binary is unavailable at %s\n%!"
          test_name rust_parser_path
  | None ->
      if bool_of_env "FORESTER_RUST_PARSER_REQUIRE_BINARY" then
        Alcotest.failf
          "%s requires rust parser binary; set FORESTER_RUST_PARSER_PATH or \
           build tools/rust-parser"
          test_name
      else
        Printf.eprintf
          "%s skipped because rust parser binary is unavailable\n%!" test_name

let fixture_paths kind =
  let fixture_dir =
    repo_root () |> fun root ->
    Filename.concat root "tools/rust-parser/tests/fixtures" |> fun root ->
    Filename.concat root kind
  in
  Sys.readdir fixture_dir |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".tree")
  |> List.sort String.compare
  |> List.map (Filename.concat fixture_dir)

let render_diagnostic diagnostic =
  let _ = diagnostic in
  "OCaml parser returned a diagnostic"

let render_rust_errors errors =
  errors
  |> List.map (fun (error : Rust_parser.parse_error) ->
      if error.report <> "" then error.report else error.message)
  |> String.concat "\n\n"

let check_node_span ~(label : string) ~(source_path : string)
    ~(start_offset : int) ~(start_line : int) ~(start_column : int)
    ~(end_offset : int) ~(end_line : int) ~(end_column : int)
    ({ Range.loc; _ } : Code.node Range.located) =
  let loc =
    match loc with
    | Some loc -> loc
    | None -> Alcotest.failf "%s should carry a source range" label
  in
  let Lsp.Types.Range.{ start; end_ } =
    Lsp_shims.Loc.lsp_range_of_range (Some loc)
  in
  let Lsp.Types.Position.
        { line = actual_start_line; character = actual_start_character } =
    start
  in
  let Lsp.Types.Position.
        { line = actual_end_line; character = actual_end_character } =
    end_
  in
  let actual_start_column = actual_start_character + 1 in
  let actual_end_column = actual_end_character + 1 in
  let _ = (source_path, start_offset, end_offset) in
  Alcotest.(check int)
    (label ^ " start line") (start_line - 1) actual_start_line;
  Alcotest.(check int)
    (label ^ " start column") start_column actual_start_column;
  Alcotest.(check int) (label ^ " end line") (end_line - 1) actual_end_line;
  Alcotest.(check int) (label ^ " end column") end_column actual_end_column

let rec strip_code_locations (code : Code.t) : Code.t =
  List.map
    (fun ({ Range.value; _ } : Code.node Range.located) ->
      {
        Range.loc = None;
        value =
          (match value with
          | Code.Group (delim, body) ->
              Code.Group (delim, strip_code_locations body)
          | Code.Math (mode, body) -> Code.Math (mode, strip_code_locations body)
          | Code.Subtree (addr, body) ->
              Code.Subtree (addr, strip_code_locations body)
          | Code.Let (path, bindings, body) ->
              Code.Let (path, bindings, strip_code_locations body)
          | Code.Scope body -> Code.Scope (strip_code_locations body)
          | Code.Put (path, body) -> Code.Put (path, strip_code_locations body)
          | Code.Default (path, body) ->
              Code.Default (path, strip_code_locations body)
          | Code.Fun (bindings, body) ->
              Code.Fun (bindings, strip_code_locations body)
          | Code.Object { self; methods } ->
              Code.Object
                {
                  self;
                  methods =
                    List.map
                      (fun (name, body) -> (name, strip_code_locations body))
                      methods;
                }
          | Code.Patch { obj; self; super; methods } ->
              Code.Patch
                {
                  obj = strip_code_locations obj;
                  self;
                  super;
                  methods =
                    List.map
                      (fun (name, body) -> (name, strip_code_locations body))
                      methods;
                }
          | Code.Call (target, method_name) ->
              Code.Call (strip_code_locations target, method_name)
          | Code.Def (path, bindings, body) ->
              Code.Def (path, bindings, strip_code_locations body)
          | Code.Namespace (path, body) ->
              Code.Namespace (path, strip_code_locations body)
          | Code.Dx_sequent (conclusion, premises) ->
              Code.Dx_sequent
                ( strip_code_locations conclusion,
                  List.map strip_code_locations premises )
          | Code.Dx_query (var, positives, negatives) ->
              Code.Dx_query
                ( var,
                  List.map strip_code_locations positives,
                  List.map strip_code_locations negatives )
          | Code.Dx_prop (relation, args) ->
              Code.Dx_prop
                ( strip_code_locations relation,
                  List.map strip_code_locations args )
          | Code.Dx_const_content body ->
              Code.Dx_const_content (strip_code_locations body)
          | Code.Dx_const_uri body ->
              Code.Dx_const_uri (strip_code_locations body)
          | ( Code.Text _ | Code.Verbatim _ | Code.Ident _ | Code.Hash_ident _
            | Code.Xml_ident _ | Code.Open _ | Code.Get _ | Code.Import _
            | Code.Decl_xmlns _ | Code.Alloc _ | Code.Dx_var _ | Code.Comment _
            | Code.Error _ ) as node ->
              node);
      })
    code

let test_positive_fixture_parity () =
  with_rust_parser_or_skip "rust parser positive fixture parity"
  @@ fun _rust_parser_path ->
  fixture_paths "positive"
  |> List.iter (fun path ->
      let fixture = Filename.basename path in
      let input = read_file path in
      match
        ( Result.map strip_code_locations (parse_string_no_loc input),
          Result.map strip_code_locations (Rust_parser.parse input) )
      with
      | Ok ocaml_code, Ok rust_code ->
          Alcotest.(check code)
            ("positive parity " ^ fixture)
            ocaml_code rust_code
      | Error diagnostic, Ok rust_code ->
          Alcotest.failf
            "Positive fixture %s failed OCaml parse:\n%s\nRust parsed as:\n%a"
            fixture
            (render_diagnostic diagnostic)
            Code.pp rust_code
      | Ok ocaml_code, Error errors ->
          Alcotest.failf
            "Positive fixture %s failed Rust parse:\n%s\nOCaml parsed as:\n%a"
            fixture
            (render_rust_errors errors)
            Code.pp ocaml_code
      | Error diagnostic, Error errors ->
          Alcotest.failf
            "Positive fixture %s failed in both parsers.\n\
             OCaml:\n\
             %s\n\n\
             Rust:\n\
             %s"
            fixture
            (render_diagnostic diagnostic)
            (render_rust_errors errors))

let test_negative_fixture_parity () =
  with_rust_parser_or_skip "rust parser negative fixture parity"
  @@ fun _rust_parser_path ->
  fixture_paths "negative"
  |> List.iter (fun path ->
      let fixture = Filename.basename path in
      let input = read_file path in
      match (parse_string_no_loc input, Rust_parser.parse input) with
      | Error _, Error _ -> ()
      | Ok ocaml_code, Error errors ->
          Alcotest.failf
            "Negative fixture %s parsed in OCaml but failed in Rust.\n\
             OCaml:\n\
             %a\n\n\
             Rust:\n\
             %s"
            fixture Code.pp ocaml_code
            (render_rust_errors errors)
      | Error diagnostic, Ok rust_code ->
          Alcotest.failf
            "Negative fixture %s failed in OCaml but parsed in Rust.\n\
             OCaml:\n\
             %s\n\n\
             Rust:\n\
             %a"
            fixture
            (render_diagnostic diagnostic)
            Code.pp rust_code
      | Ok ocaml_code, Ok rust_code ->
          Alcotest.failf
            "Negative fixture %s parsed in both parsers.\n\
             OCaml:\n\
             %a\n\n\
             Rust:\n\
             %a"
            fixture Code.pp ocaml_code Code.pp rust_code)

let test_rust_bridge_preserves_spans () =
  with_rust_parser_or_skip "rust parser bridge preserves spans"
  @@ fun _rust_parser_path ->
  let source_path = "rust-parser-span-check.tree" in
  match Rust_parser.parse_tree ~source_path {|\title{Hello}
\p{}|} with
  | Error errors ->
      Alcotest.failf "Rust parser span fixture failed:\n%s"
        (render_rust_errors errors)
  | Ok
      {
        Code.source_path = Some actual_source_path;
        code = [ title_ident; title_group; paragraph_ident; paragraph_group ];
        _;
      } ->
      Alcotest.(check string) "tree source_path" source_path actual_source_path;
      check_node_span ~label:"title ident" ~source_path ~start_offset:1
        ~start_line:1 ~start_column:2 ~end_offset:6 ~end_line:1 ~end_column:7
        title_ident;
      check_node_span ~label:"title group" ~source_path ~start_offset:6
        ~start_line:1 ~start_column:7 ~end_offset:13 ~end_line:1 ~end_column:14
        title_group;
      check_node_span ~label:"paragraph ident" ~source_path ~start_offset:15
        ~start_line:2 ~start_column:2 ~end_offset:16 ~end_line:2 ~end_column:3
        paragraph_ident;
      check_node_span ~label:"paragraph group" ~source_path ~start_offset:16
        ~start_line:2 ~start_column:3 ~end_offset:18 ~end_line:2 ~end_column:5
        paragraph_group
  | Ok { Code.source_path = None; _ } ->
      Alcotest.fail "Expected parse_tree to preserve source_path"
  | Ok { Code.code; _ } ->
      Alcotest.failf "Expected four top-level nodes in span fixture, got %d"
        (List.length code)

let test_rust_bridge_surfaces_structured_errors () =
  with_rust_parser_or_skip "rust parser bridge surfaces structured errors"
  @@ fun _rust_parser_path ->
  match Rust_parser.parse "(abc]" with
  | Ok code ->
      Alcotest.failf "Expected mismatched delimiter parse failure, got %d nodes"
        (List.length code)
  | Error [ error ] -> (
      match error.details with
      | None ->
          Alcotest.fail "Expected structured error details from Rust bridge"
      | Some details ->
          Alcotest.(check string)
            "error kind" "mismatched_delimiter" details.kind;
          Alcotest.(check (list string))
            "expected delimiters" [ "')'" ] details.expected;
          Alcotest.(check (option string))
            "found delimiter" (Some "']'") details.found;
          Alcotest.(check int) "label count" 2 (List.length details.labels))
  | Error errors ->
      Alcotest.failf "Expected single structured error, got %d"
        (List.length errors)

let () =
  let open Alcotest in
  run "Rust parser parity"
    [
      ( "fixtures",
        [
          test_case "positive fixture parity" `Quick
            test_positive_fixture_parity;
          test_case "negative fixture parity" `Quick
            test_negative_fixture_parity;
          test_case "bridge preserves spans" `Quick
            test_rust_bridge_preserves_spans;
          test_case "bridge surfaces structured errors" `Quick
            test_rust_bridge_surfaces_structured_errors;
        ] );
    ]
