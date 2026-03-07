(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

let read_input path = In_channel.with_open_bin path In_channel.input_all

let explanation_of_diagnostic diagnostic =
  Asai.Diagnostic.string_of_text diagnostic.Asai.Diagnostic.explanation.value

let parse_file path =
  let input = read_input path in
  let lexbuf = Lexing.from_string input in
  match Forester_parser.Parse.parse lexbuf with
  | Ok code ->
      ignore (List.length code);
      Ok ()
  | Error diagnostic -> Error (explanation_of_diagnostic diagnostic)

let usage () =
  prerr_endline "Usage: Parser_benchmark.exe <tree-file>";
  exit 2

let () =
  if Array.length Sys.argv <> 2 then usage ();
  match parse_file Sys.argv.(1) with
  | Ok () -> ()
  | Error message ->
      prerr_endline message;
      exit 1
