(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

let read_all_stdin () =
  In_channel.input_all In_channel.stdin

let () =
  let input = read_all_stdin () in
  let lexbuf = Lexing.from_string input in
  match Forester_parser.Parse.parse lexbuf with
  | Ok _ -> print_endline "ok"
  | Error _ -> print_endline "error"
