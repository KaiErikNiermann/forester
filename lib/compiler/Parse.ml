(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

include Forester_parser.Parse

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  if filename = "" then assert false;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename};
  parse ~source: (`File filename) lexbuf

let parse_document ~host doc =
  let uri = Lsp.Text_document.documentUri doc in
  let path = Lsp.Uri.to_path uri in
  let text = Lsp.Text_document.text doc in
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = path};
  Result.map (fun code ->
    Code.{
      code;
      source_path = Some path;
      uri = Some (URI_scheme.path_to_uri ~host path);
      timestamp = Some (Unix.time ());
    }
  ) @@
    parse ~source: (`File path) lexbuf

let parse_file filename =
  let@ () = Reporter.tracef "when parsing file `%s`" filename in
  let ch = open_in filename in
  Fun.protect ~finally: (fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch
