(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
open Forester_core
open Testables

module Rust_parser = Forester_parser.Rust_parser

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

let resolve_rust_parser_path () =
  let root = repo_root () in
  let local_candidates =
    [
      Filename.concat root "tools/rust-parser/target/debug/forester-rust-parser";
      Filename.concat root "tools/rust-parser/target/release/forester-rust-parser";
    ]
  in
  match Sys.getenv_opt "FORESTER_RUST_PARSER_PATH" with
  | Some path when String.trim path <> "" && Sys.file_exists path -> Some path
  | _ -> List.find_opt Sys.file_exists local_candidates

let with_rust_parser_or_skip test_name f =
  match resolve_rust_parser_path () with
  | Some rust_parser_path ->
    Rust_parser.set_rust_parser_path rust_parser_path;
    if Rust_parser.is_available () then
      f rust_parser_path
    else if bool_of_env "FORESTER_RUST_PARSER_REQUIRE_BINARY" then
      Alcotest.failf
        "%s requires rust parser binary at %s"
        test_name
        rust_parser_path
    else
      Printf.eprintf
        "%s skipped because rust parser binary is unavailable at %s\n%!"
        test_name
        rust_parser_path
  | None ->
    if bool_of_env "FORESTER_RUST_PARSER_REQUIRE_BINARY" then
      Alcotest.failf
        "%s requires rust parser binary; set FORESTER_RUST_PARSER_PATH or build tools/rust-parser"
        test_name
    else
      Printf.eprintf
        "%s skipped because rust parser binary is unavailable\n%!"
        test_name

let fixture_paths kind =
  let fixture_dir =
    repo_root ()
    |> fun root -> Filename.concat root "tools/rust-parser/tests/fixtures"
    |> fun root -> Filename.concat root kind
  in
  Sys.readdir fixture_dir
  |> Array.to_list
  |> List.filter (fun name -> Filename.check_suffix name ".tree")
  |> List.sort String.compare
  |> List.map (Filename.concat fixture_dir)

let render_diagnostic diagnostic =
  let _ = diagnostic in
  "OCaml parser returned a diagnostic"

let render_rust_errors errors =
  errors
  |> List.map (fun (error : Rust_parser.parse_error) ->
         if error.report <> "" then
           error.report
         else
           error.message
       )
  |> String.concat "\n\n"

let rec strip_code_locations (code : Code.t) : Code.t =
  List.map
    (fun ({Range.value; _} : Code.node Range.located) ->
      {
        Range.loc = None;
        value =
          match value with
          | Code.Group (delim, body) -> Code.Group (delim, strip_code_locations body)
          | Code.Math (mode, body) -> Code.Math (mode, strip_code_locations body)
          | Code.Subtree (addr, body) -> Code.Subtree (addr, strip_code_locations body)
          | Code.Let (path, bindings, body) ->
            Code.Let (path, bindings, strip_code_locations body)
          | Code.Scope body -> Code.Scope (strip_code_locations body)
          | Code.Put (path, body) -> Code.Put (path, strip_code_locations body)
          | Code.Default (path, body) -> Code.Default (path, strip_code_locations body)
          | Code.Fun (bindings, body) -> Code.Fun (bindings, strip_code_locations body)
          | Code.Object {self; methods} ->
            Code.Object
              {
                self;
                methods =
                  List.map
                    (fun (name, body) -> (name, strip_code_locations body))
                    methods;
              }
          | Code.Patch {obj; self; super; methods} ->
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
              (strip_code_locations conclusion, List.map strip_code_locations premises)
          | Code.Dx_query (var, positives, negatives) ->
            Code.Dx_query
              ( var,
                List.map strip_code_locations positives,
                List.map strip_code_locations negatives )
          | Code.Dx_prop (relation, args) ->
            Code.Dx_prop
              (strip_code_locations relation, List.map strip_code_locations args)
          | Code.Dx_const_content body ->
            Code.Dx_const_content (strip_code_locations body)
          | Code.Dx_const_uri body -> Code.Dx_const_uri (strip_code_locations body)
          | ( Code.Text _
            | Code.Verbatim _
            | Code.Ident _
            | Code.Hash_ident _
            | Code.Xml_ident _
            | Code.Open _
            | Code.Get _
            | Code.Import _
            | Code.Decl_xmlns _
            | Code.Alloc _
            | Code.Dx_var _
            | Code.Comment _
            | Code.Error _ ) as node ->
            node;
      } )
    code

let test_positive_fixture_parity () =
  with_rust_parser_or_skip "rust parser positive fixture parity" @@ fun _rust_parser_path ->
  fixture_paths "positive"
  |> List.iter (fun path ->
         let fixture = Filename.basename path in
         let input = read_file path in
         match
           Result.map strip_code_locations (parse_string_no_loc input),
           Result.map strip_code_locations (Rust_parser.parse input)
         with
         | Ok ocaml_code, Ok rust_code ->
           Alcotest.(check code)
             ("positive parity " ^ fixture)
             ocaml_code
             rust_code
         | Error diagnostic, Ok rust_code ->
           Alcotest.failf
             "Positive fixture %s failed OCaml parse:\n%s\nRust parsed as:\n%a"
             fixture
             (render_diagnostic diagnostic)
             Code.pp
             rust_code
         | Ok ocaml_code, Error errors ->
           Alcotest.failf
             "Positive fixture %s failed Rust parse:\n%s\nOCaml parsed as:\n%a"
             fixture
             (render_rust_errors errors)
             Code.pp
             ocaml_code
         | Error diagnostic, Error errors ->
           Alcotest.failf
             "Positive fixture %s failed in both parsers.\nOCaml:\n%s\n\nRust:\n%s"
             fixture
             (render_diagnostic diagnostic)
             (render_rust_errors errors)
       )

let test_negative_fixture_parity () =
  with_rust_parser_or_skip "rust parser negative fixture parity" @@ fun _rust_parser_path ->
  fixture_paths "negative"
  |> List.iter (fun path ->
         let fixture = Filename.basename path in
         let input = read_file path in
         match parse_string_no_loc input, Rust_parser.parse input with
         | Error _, Error _ -> ()
         | Ok ocaml_code, Error errors ->
           Alcotest.failf
             "Negative fixture %s parsed in OCaml but failed in Rust.\nOCaml:\n%a\n\nRust:\n%s"
             fixture
             Code.pp
             ocaml_code
             (render_rust_errors errors)
         | Error diagnostic, Ok rust_code ->
           Alcotest.failf
             "Negative fixture %s failed in OCaml but parsed in Rust.\nOCaml:\n%s\n\nRust:\n%a"
             fixture
             (render_diagnostic diagnostic)
             Code.pp
             rust_code
         | Ok ocaml_code, Ok rust_code ->
           Alcotest.failf
             "Negative fixture %s parsed in both parsers.\nOCaml:\n%a\n\nRust:\n%a"
             fixture
             Code.pp
             ocaml_code
             Code.pp
             rust_code
       )

let () =
  let open Alcotest in
  run
    "Rust parser parity"
    [
      ( "fixtures",
        [
          test_case
            "positive fixture parity"
            `Quick
            test_positive_fixture_parity;
          test_case
            "negative fixture parity"
            `Quick
            test_negative_fixture_parity;
        ] );
    ]
