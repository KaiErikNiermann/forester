(* SPDX-FileCopyrightText: 2024 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

(** Interface to the Rust-based Forester parser

    This module provides an interface to an alternative parser implementation
    written in Rust. The Rust parser is invoked as a subprocess and communicates
    via JSON.

    Note: This is a proof-of-concept integration. The Rust parser binary must
    be available in the PATH as 'forester-rust-parser'.
*)

(** The path to the Rust parser binary *)
let rust_parser_path = ref "forester-rust-parser"

(** Set the path to the Rust parser binary *)
let set_rust_parser_path path =
  rust_parser_path := path

(** Check if the Rust parser is available *)
let is_available () : bool =
  try
    let ic = Unix.open_process_in (!rust_parser_path ^ " --version 2>/dev/null") in
    let result = try
      let _ = input_line ic in
      true
    with End_of_file -> false
    in
    let _ = Unix.close_process_in ic in
    result
  with
  | Unix.Unix_error _ -> false
  | _ -> false

(** Parse using the Rust parser, returning raw JSON string

    This is a low-level function that invokes the Rust parser and returns
    the raw JSON output. Use this when you need access to the Rust AST
    directly rather than converting to OCaml types.
*)
let parse_to_json (input : string) : (string, string) result =
  if not (is_available ()) then
    Error "Rust parser not available"
  else
    try
      (* Create temp file for input *)
      let tmp_file = Filename.temp_file "forester_rust_" ".tree" in
      let oc = open_out tmp_file in
      output_string oc input;
      close_out oc;

      (* Run the Rust parser *)
      let cmd = Printf.sprintf "%s --json %s 2>&1" !rust_parser_path tmp_file in
      let ic = Unix.open_process_in cmd in
      let buf = Buffer.create 1024 in
      (try
        while true do
          Buffer.add_string buf (input_line ic);
          Buffer.add_char buf '\n'
        done
      with End_of_file -> ());
      let status = Unix.close_process_in ic in

      (* Cleanup *)
      Sys.remove tmp_file;

      match status with
      | Unix.WEXITED 0 -> Ok (Buffer.contents buf)
      | Unix.WEXITED code -> Error (Printf.sprintf "Rust parser exited with code %d: %s" code (Buffer.contents buf))
      | Unix.WSIGNALED sig_num -> Error (Printf.sprintf "Rust parser killed by signal %d" sig_num)
      | Unix.WSTOPPED sig_num -> Error (Printf.sprintf "Rust parser stopped by signal %d" sig_num)
    with
    | exn -> Error (Printf.sprintf "Error invoking Rust parser: %s" (Printexc.to_string exn))

(** Parse and return error messages from the Rust parser

    This function parses the input and returns either Ok on success
    or Error with a list of error messages on failure.
*)
let parse_check (input : string) : (unit, string) result =
  match parse_to_json input with
  | Ok _ -> Ok ()
  | Error msg -> Error msg
