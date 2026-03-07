(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

include Forester_parser.Parse

exception Diagnostic_failure of Reporter.diagnostic

let parse_string ~filename content =
  let lexbuf = Lexing.from_string content in
  if filename = "" then assert false;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = filename};
  parse lexbuf

let is_markdown_source filename =
  match String.lowercase_ascii (Filename.extension filename) with
  | ".md"
  | ".markdown" ->
    true
  | _ ->
    false

let to_external_error message =
  let emit _ = () in
  let fatal diagnostic = raise (Diagnostic_failure diagnostic) in
  try
    Reporter.run ~emit ~fatal @@ fun () ->
    Reporter.fatal
      Reporter.Message.External_error
      ~extra_remarks: [Asai.Diagnostic.loctext message]
  with
    | Diagnostic_failure diagnostic -> diagnostic

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

let convert_markdown_to_forester ~filename content =
  let converter_path =
    match Sys.getenv_opt "FORESTER_PANDOC_PATH" with
    | Some path when String.trim path <> "" -> path
    | _ -> "forester-pandoc"
  in
  let temp_file = Filename.temp_file "forester_markdown_" (Filename.extension filename) in
  Fun.protect
    ~finally: (fun () ->
      if Sys.file_exists temp_file then
        Sys.remove temp_file
    )
    @@ fun () ->
    let output_channel = open_out_bin temp_file in
    Fun.protect
      ~finally: (fun () -> close_out_noerr output_channel)
      @@ fun () ->
      output_string output_channel content;
      close_out output_channel;
      let command =
        Printf.sprintf
          "%s markdown-to-forester %s 2>&1"
          (Filename.quote converter_path)
          (Filename.quote temp_file)
      in
      let status, output = run_command_capture command in
      match status with
      | Unix.WEXITED 0 -> Ok output
      | Unix.WEXITED code ->
        Error
          (
            Printf.sprintf
              "The Markdown converter `%s` exited with code %d while processing `%s`.\n%s"
              converter_path
              code
              filename
              output
          )
      | Unix.WSIGNALED signal_number ->
        Error
          (
            Printf.sprintf
              "The Markdown converter `%s` was terminated by signal %d while processing `%s`."
              converter_path
              signal_number
              filename
          )
      | Unix.WSTOPPED signal_number ->
        Error
          (
            Printf.sprintf
              "The Markdown converter `%s` was stopped by signal %d while processing `%s`."
              converter_path
              signal_number
              filename
          )

let parse_content ~filename content =
  let markdown_source = is_markdown_source filename in
  let parsed_content =
    if markdown_source then
      convert_markdown_to_forester ~filename content
    else Ok content
  in
  match parsed_content with
  | Ok source ->
    let parsed = parse_string ~filename source in
    if markdown_source then
      Result.map_error
        (fun (diagnostic : Reporter.diagnostic) ->
          let explanation =
            Asai.Diagnostic.string_of_text diagnostic.Asai.Diagnostic.explanation.value
          in
          to_external_error
            (
              Printf.sprintf
                "The Markdown converter produced invalid Forester while processing `%s`.\n%s"
                filename
                explanation
            )
        )
        parsed
    else
      parsed
  | Error message ->
    Error (to_external_error message)

let parse_channel filename ch =
  parse_content ~filename (In_channel.input_all ch)

let parse_document ~(config : Config.t) doc =
  let uri = Lsp.Text_document.documentUri doc in
  let path = Lsp.Uri.to_path uri in
  let text = Lsp.Text_document.text doc in
  parse_content ~filename: path text
  |> Result.map (fun nodes ->
      Tree.{
        nodes;
        origin = Physical doc;
        identity = URI (URI_scheme.path_to_uri ~base: config.url path);
        timestamp = Some (Unix.time ());
      }
    )

let parse_file filename =
  let@ () = Reporter.tracef "when parsing file `%s`" filename in
  let ch = open_in filename in
  Fun.protect ~finally: (fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch
