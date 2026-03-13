(* SPDX-FileCopyrightText: 2024 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

(** Interface to the Rust-based Forester parser

    This module provides an interface to an alternative parser implementation
    written in Rust. The Rust parser is invoked as a subprocess and communicates
    via JSON.

    The Rust parser binary must be available in the PATH as
    'forester-rust-parser' or configured via {!set_rust_parser_path}. *)

open Forester_core
module Types = Rust_parser_types
module Process = Rust_parser_process
module Json = Rust_parser_json

(** The path to the Rust parser binary *)
let rust_parser_path = ref "forester-rust-parser"

(** Maximum time to wait for a Rust parser subprocess before killing it. *)
let rust_parser_timeout_seconds = ref 5.0

(** Set the path to the Rust parser binary *)
let set_rust_parser_path path = rust_parser_path := path

(** Set the Rust parser subprocess timeout. *)
let set_rust_parser_timeout_seconds seconds =
  if seconds <= 0. then
    invalid_arg "Rust_parser.set_rust_parser_timeout_seconds"
  else rust_parser_timeout_seconds := seconds

type parse_mode = Types.parse_mode = Strict | Recovery

type parse_error_label = Types.parse_error_label = {
  kind: string;
  message: string;
  start_offset: int;
  end_offset: int;
}

type parse_error_details = Types.parse_error_details = {
  kind: string;
  expected: string list;
  found: string option;
  labels: parse_error_label list;
  notes: string list;
}

type parse_error = Types.parse_error = {
  message: string;
  start_offset: int;
  end_offset: int;
  report: string;
  details: parse_error_details option;
}

type parse_outcome = Types.parse_outcome =
  | Parsed of Code.t
  | Recovered of Code.t * parse_error list
  | Failed of parse_error list

let source_of_input = Json.source_of_input
let range_of_json = Json.range_of_json
let node_of_json = Json.node_of_json
let located_node_of_json = Json.located_node_of_json
let parse_outcome_of_json = Json.parse_outcome_of_json

let failed ?(report = "") message =
  Failed [Types.make_parse_error ~report message]

(** Check if the Rust parser is available *)
let is_available () : bool = Process.is_available ~program: !rust_parser_path

(** Parse using the Rust parser, returning raw JSON string *)
let parse_to_json
    ?(mode : parse_mode = Strict)
    (input : string)
    : (string, string) result
  =
  Process.parse_to_json
    ~program: !rust_parser_path
    ~timeout_seconds: !rust_parser_timeout_seconds
    ~mode
    input

let parse_with_mode
    ?(mode : parse_mode = Strict)
    ?source_path
    (input : string)
    : parse_outcome
  =
  match parse_to_json ~mode input with
  | Error msg -> failed ~report: msg msg
  | Ok json_str ->
    (
      try
        let json = Yojson.Safe.from_string json_str in
        parse_outcome_of_json ?source_path input json
      with
        | Yojson.Safe.Util.Type_error (msg, _) ->
          failed ("JSON type error: " ^ msg)
        | Failure msg -> failed ("Conversion error: " ^ msg)
        | exn -> failed ("Parse error: " ^ Printexc.to_string exn)
    )

(** Parse input and return Code.t or errors *)
let parse ?source_path (input : string) : (Code.t, parse_error list) result =
  match parse_with_mode ?source_path ~mode: Strict input with
  | Parsed nodes -> Ok nodes
  | Failed errors -> Error errors
  | Recovered (_nodes, errors) -> Error errors

let parse_recovery ?source_path (input : string) : parse_outcome =
  parse_with_mode ?source_path ~mode: Recovery input

(** Parse and return Code.tree structure *)
let parse_tree
    ?(source_path : string option)
    (input : string)
    : (Code.tree, parse_error list) result
  =
  match parse ?source_path input with
  | Error errs -> Error errs
  | Ok code -> Ok {Code.source_path; uri = None; timestamp = None; code}

(** Parse and validate input (returns unit on success) *)
let parse_check (input : string) : (unit, string) result =
  match parse input with
  | Ok _ -> Ok ()
  | Error errs ->
    let msg =
      errs
      |> List.map (fun error ->
          if error.report <> "" then error.report else error.message
        )
      |> String.concat "\n"
    in
    Error msg
