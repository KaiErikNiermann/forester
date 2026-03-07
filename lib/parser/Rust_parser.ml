(* SPDX-FileCopyrightText: 2024 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

(** Interface to the Rust-based Forester parser

    This module provides an interface to an alternative parser implementation
    written in Rust. The Rust parser is invoked as a subprocess and communicates
    via JSON.

    The Rust parser binary must be available in the PATH as
    'forester-rust-parser' or configured via {!set_rust_parser_path}. *)

open Forester_core

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
  else
    rust_parser_timeout_seconds := seconds

type process_capture = {
  stdout : string;
  stderr : string;
  status : Unix.process_status;
}

type process_run_error =
  | Spawn_error of (Unix.error * string * string)
  | Timed_out of float

type stream_capture = {
  fd : Unix.file_descr;
  buffer : Buffer.t;
}

let format_unix_error (error, fn, arg) =
  let arg_suffix =
    if String.trim arg = "" then ""
    else Printf.sprintf " (%s)" arg
  in
  Printf.sprintf "%s%s: %s" fn arg_suffix (Unix.error_message error)

let nonempty_output text =
  let trimmed = String.trim text in
  if trimmed = "" then None else Some trimmed

let format_process_output ~stdout ~stderr =
  let sections =
    [ ("stderr", stderr); ("stdout", stdout) ]
    |> List.filter_map (fun (label, content) ->
           Option.map (fun text -> (label, text)) (nonempty_output content))
  in
  match sections with
  | [] -> None
  | [ label, text ] -> Some (Printf.sprintf "%s:\n%s" label text)
  | _ ->
      Some
        (sections
        |> List.map (fun (label, text) -> Printf.sprintf "%s:\n%s" label text)
        |> String.concat "\n\n")

let rec waitpid_with_timeout pid deadline =
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then
    Error (Timed_out 0.)
  else
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        Unix.sleepf (min 0.05 remaining);
        waitpid_with_timeout pid deadline
    | _, status -> Ok status

let read_streams_with_timeout ~stdout_ic ~stderr_ic ~timeout_seconds =
  let stdout = Buffer.create 1024 in
  let stderr = Buffer.create 1024 in
  let active_streams =
    ref
      [
        { fd = Unix.descr_of_in_channel stdout_ic; buffer = stdout };
        { fd = Unix.descr_of_in_channel stderr_ic; buffer = stderr };
      ]
  in
  let scratch = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let rec loop () =
    match !active_streams with
    | [] -> Ok (Buffer.contents stdout, Buffer.contents stderr, deadline)
    | streams ->
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then
          Error (Timed_out timeout_seconds)
        else
          let ready, _, _ =
            Unix.select
              (List.map (fun stream -> stream.fd) streams)
              [] [] remaining
          in
          if ready = [] then
            Error (Timed_out timeout_seconds)
          else (
            active_streams :=
              List.filter_map
                (fun stream ->
                  if List.exists (( = ) stream.fd) ready then
                    match Unix.read stream.fd scratch 0 (Bytes.length scratch) with
                    | 0 -> None
                    | read_count ->
                        Buffer.add_subbytes stream.buffer scratch 0 read_count;
                        Some stream
                  else
                    Some stream)
                streams;
            loop ())
  in
  loop ()

let run_process_capture ~program ~args ~timeout_seconds =
  try
    let stdout_ic, stdin_oc, stderr_ic =
      Unix.open_process_args_full program args (Unix.environment ())
    in
    let pid = Unix.process_full_pid (stdout_ic, stdin_oc, stderr_ic) in
    Fun.protect
      ~finally:(fun () ->
        close_in_noerr stdout_ic;
        close_in_noerr stderr_ic;
        close_out_noerr stdin_oc)
      @@ fun () ->
      close_out_noerr stdin_oc;
      match
        read_streams_with_timeout ~stdout_ic ~stderr_ic ~timeout_seconds
      with
      | Error error ->
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
          Error error
      | Ok (stdout, stderr, deadline) -> (
          match waitpid_with_timeout pid deadline with
          | Ok status -> Ok { stdout; stderr; status }
          | Error error ->
              (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
              (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
              Error
                (match error with
                | Timed_out 0. -> Timed_out timeout_seconds
                | _ -> error))
  with Unix.Unix_error (error, fn, arg) -> Error (Spawn_error (error, fn, arg))

(** Check if the Rust parser is available *)
let is_available () : bool =
  let args = [| !rust_parser_path; "--version" |] in
  match
    run_process_capture ~program:!rust_parser_path ~args ~timeout_seconds:1.0
  with
  | Ok { status = Unix.WEXITED 0; _ } -> true
  | Ok _ -> false
  | Error _ -> false

(** {2 JSON to Code.t conversion} *)

module Json = Yojson.Safe
module Util = Yojson.Safe.Util

type rust_position = { offset : int; line : int; column : int }

let source_of_input ?source_path input : Range.source =
  match source_path with
  | Some path -> `File path
  | None -> `String { title = None; content = input }

let position_of_json ~source json : Range.position =
  let offset = Util.member "offset" json |> Util.to_int in
  let line = Util.member "line" json |> Util.to_int in
  let column = Util.member "column" json |> Util.to_int in
  {
    Range.source;
    offset;
    start_of_line = offset - (column - 1);
    line_num = line;
  }

let range_of_json ~source json : Range.t =
  let start_pos = Util.member "start" json |> position_of_json ~source in
  let end_pos = Util.member "end" json |> position_of_json ~source in
  Range.make (start_pos, end_pos)

(** Convert JSON delim to Code delim *)
let delim_of_json json =
  match Util.to_string json with
  | "braces" -> Braces
  | "squares" -> Squares
  | "parens" -> Parens
  | s -> failwith ("Unknown delimiter: " ^ s)

(** Convert JSON math mode to Code math_mode *)
let math_mode_of_json json =
  match Util.to_string json with
  | "inline" -> Inline
  | "display" -> Display
  | s -> failwith ("Unknown math mode: " ^ s)

(** Convert JSON visibility to Code visibility *)
let visibility_of_json json =
  match Util.to_string json with
  | "private" -> Private
  | "public" -> Public
  | s -> failwith ("Unknown visibility: " ^ s)

(** Convert JSON binding info to Code binding *)
let binding_of_json json =
  let arr = Util.to_list json in
  match arr with
  | [ strictness_json; name_json ] ->
      let strictness = Util.to_string strictness_json in
      let name = Util.to_string name_json in
      let info =
        match strictness with
        | "strict" -> Strict
        | "lazy" -> Lazy
        | s -> failwith ("Unknown binding info: " ^ s)
      in
      (info, name)
  | _ -> failwith "Invalid binding format"

(** Convert JSON node to Code.node *)
let rec node_of_json ~source json : Code.node =
  let ty = Util.member "type" json |> Util.to_string in
  match ty with
  | "text" -> Code.Text (Util.member "content" json |> Util.to_string)
  | "verbatim" -> Code.Verbatim (Util.member "content" json |> Util.to_string)
  | "comment" -> Code.Comment (Util.member "content" json |> Util.to_string)
  | "error" -> Code.Error (Util.member "message" json |> Util.to_string)
  | "group" ->
      let delim = Util.member "delim" json |> delim_of_json in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Group (delim, body)
  | "math" ->
      let mode = Util.member "mode" json |> math_mode_of_json in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Math (mode, body)
  | "ident" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      Code.Ident path
  | "hash_ident" ->
      let name = Util.member "name" json |> Util.to_string in
      Code.Hash_ident name
  | "xml_ident" ->
      let prefix = Util.member "prefix" json |> Util.to_string_option in
      let name = Util.member "name" json |> Util.to_string in
      Code.Xml_ident (prefix, name)
  | "let" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      let bindings =
        Util.member "bindings" json |> Util.to_list |> List.map binding_of_json
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Let (path, bindings, body)
  | "def" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      let bindings =
        Util.member "bindings" json |> Util.to_list |> List.map binding_of_json
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Def (path, bindings, body)
  | "fun" ->
      let bindings =
        Util.member "bindings" json |> Util.to_list |> List.map binding_of_json
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Fun (bindings, body)
  | "scope" ->
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Scope body
  | "namespace" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Namespace (path, body)
  | "open" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      Code.Open path
  | "put" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Put (path, body)
  | "default" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Default (path, body)
  | "get" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      Code.Get path
  | "alloc" ->
      let path =
        Util.member "path" json |> Util.to_list |> List.map Util.to_string
      in
      Code.Alloc path
  | "object" ->
      let def = Util.member "def" json in
      let self = Util.member "self_name" def |> Util.to_string_option in
      let methods =
        Util.member "methods" def |> Util.to_list
        |> List.map (fun m ->
            let arr = Util.to_list m in
            match arr with
            | [ name_json; body_json ] ->
                let name = Util.to_string name_json in
                let body = nodes_of_json ~source body_json in
                (name, body)
            | _ -> failwith "Invalid method format")
      in
      Code.Object { self; methods }
  | "patch" ->
      let def = Util.member "def" json in
      let obj = Util.member "obj" def |> nodes_of_json ~source in
      let self = Util.member "self_name" def |> Util.to_string_option in
      let super = Util.member "super_name" def |> Util.to_string_option in
      let methods =
        Util.member "methods" def |> Util.to_list
        |> List.map (fun m ->
            let arr = Util.to_list m in
            match arr with
            | [ name_json; body_json ] ->
                let name = Util.to_string name_json in
                let body = nodes_of_json ~source body_json in
                (name, body)
            | _ -> failwith "Invalid method format")
      in
      Code.Patch { obj; self; super; methods }
  | "call" ->
      let target = Util.member "target" json |> nodes_of_json ~source in
      let method_ = Util.member "method" json |> Util.to_string in
      Code.Call (target, method_)
  | "subtree" ->
      let addr = Util.member "addr" json |> Util.to_string_option in
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Subtree (addr, body)
  | "import" ->
      let visibility = Util.member "visibility" json |> visibility_of_json in
      let target = Util.member "target" json |> Util.to_string in
      Code.Import (visibility, target)
  | "decl_xmlns" ->
      let prefix = Util.member "prefix" json |> Util.to_string in
      let uri = Util.member "uri" json |> Util.to_string in
      Code.Decl_xmlns (prefix, uri)
  | "dx_sequent" ->
      let conclusion = Util.member "conclusion" json |> nodes_of_json ~source in
      let premises =
        Util.member "premises" json
        |> Util.to_list
        |> List.map (nodes_of_json ~source)
      in
      Code.Dx_sequent (conclusion, premises)
  | "dx_query" ->
      let var = Util.member "var" json |> Util.to_string in
      let positives =
        Util.member "positives" json
        |> Util.to_list
        |> List.map (nodes_of_json ~source)
      in
      let negatives =
        Util.member "negatives" json
        |> Util.to_list
        |> List.map (nodes_of_json ~source)
      in
      Code.Dx_query (var, positives, negatives)
  | "dx_prop" ->
      let relation = Util.member "relation" json |> nodes_of_json ~source in
      let args =
        Util.member "args" json |> Util.to_list
        |> List.map (nodes_of_json ~source)
      in
      Code.Dx_prop (relation, args)
  | "dx_var" ->
      let name = Util.member "name" json |> Util.to_string in
      Code.Dx_var name
  | "dx_const_content" ->
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Dx_const_content body
  | "dx_const_uri" ->
      let body = Util.member "body" json |> nodes_of_json ~source in
      Code.Dx_const_uri body
  | _ ->
      (* Unknown node type - convert to error *)
      Code.Error ("Unknown Rust AST node type: " ^ ty)

(** Convert JSON located node to Code.t element *)
and located_node_of_json ~source json : Code.node Range.located =
  let value_json = Util.member "value" json in
  let value = node_of_json ~source value_json in
  let loc =
    match Util.member "span" json with
    | `Null -> None
    | span_json -> Some (range_of_json ~source span_json)
  in
  { loc; value }

(** Convert JSON node array to Code.t *)
and nodes_of_json ~source json : Code.t =
  Util.to_list json |> List.map (located_node_of_json ~source)

(** {2 Parsing functions} *)

type parse_mode = Strict | Recovery

(** Parse using the Rust parser, returning raw JSON string *)
let parse_to_json ?(mode : parse_mode = Strict) (input : string) :
    (string, string) result =
  let tmp_file = Filename.temp_file "forester_rust_" ".tree" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists tmp_file then
        Sys.remove tmp_file)
    @@ fun () ->
    let oc = open_out_bin tmp_file in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      @@ fun () ->
      output_string oc input;
      close_out oc;
      let args =
        match mode with
        | Strict -> [| !rust_parser_path; "--json"; tmp_file |]
        | Recovery ->
            [| !rust_parser_path; "--json"; "--recovery"; tmp_file |]
      in
      match
        run_process_capture ~program:!rust_parser_path ~args
          ~timeout_seconds:!rust_parser_timeout_seconds
      with
      | Ok { stdout; status = Unix.WEXITED 0; _ } -> Ok stdout
      | Ok { stdout; stderr; status = Unix.WEXITED code } ->
          let output_suffix =
            match format_process_output ~stdout ~stderr with
            | None -> ""
            | Some output -> "\n" ^ output
          in
          Error
            (Printf.sprintf
               "Rust parser `%s` exited with code %d.%s"
               !rust_parser_path code output_suffix)
      | Ok { stdout; stderr; status = Unix.WSIGNALED sig_num } ->
          let output_suffix =
            match format_process_output ~stdout ~stderr with
            | None -> ""
            | Some output -> "\n" ^ output
          in
          Error
            (Printf.sprintf
               "Rust parser `%s` was terminated by signal %d.%s"
               !rust_parser_path sig_num output_suffix)
      | Ok { stdout; stderr; status = Unix.WSTOPPED sig_num } ->
          let output_suffix =
            match format_process_output ~stdout ~stderr with
            | None -> ""
            | Some output -> "\n" ^ output
          in
          Error
            (Printf.sprintf
               "Rust parser `%s` was stopped by signal %d.%s"
               !rust_parser_path sig_num output_suffix)
      | Error (Timed_out timeout_seconds) ->
          Error
            (Printf.sprintf
               "Rust parser `%s` timed out after %.2fs."
               !rust_parser_path timeout_seconds)
      | Error (Spawn_error unix_error) ->
          Error
            (Printf.sprintf
               "Rust parser `%s` is unavailable: %s"
               !rust_parser_path (format_unix_error unix_error))

type parse_error = {
  message : string;
  start_offset : int;
  end_offset : int;
  report : string;  (** Pretty-printed ariadne error report *)
  details : parse_error_details option;
}
(** Error type for parse results *)

and parse_error_details = {
  kind : string;
  expected : string list;
  found : string option;
  labels : parse_error_label list;
  notes : string list;
}

and parse_error_label = {
  kind : string;
  message : string;
  start_offset : int;
  end_offset : int;
}

type parse_outcome =
  | Parsed of Code.t
  | Recovered of Code.t * parse_error list
  | Failed of parse_error list

let parse_error_label_of_json json =
  {
    kind = Util.member "kind" json |> Util.to_string;
    message = Util.member "message" json |> Util.to_string;
    start_offset = Util.member "start_offset" json |> Util.to_int;
    end_offset = Util.member "end_offset" json |> Util.to_int;
  }

let parse_error_details_of_json json =
  {
    kind = Util.member "kind" json |> Util.to_string;
    expected =
      (try
         Util.member "expected" json |> Util.to_list |> List.map Util.to_string
       with _ -> []);
    found = Util.member "found" json |> Util.to_string_option;
    labels =
      (try
         Util.member "labels" json |> Util.to_list
         |> List.map parse_error_label_of_json
       with _ -> []);
    notes =
      (try Util.member "notes" json |> Util.to_list |> List.map Util.to_string
       with _ -> []);
  }

let parse_errors_of_json json =
  Util.member "errors" json |> Util.to_list
  |> List.map (fun e ->
      {
        message = Util.member "message" e |> Util.to_string;
        start_offset = Util.member "start_offset" e |> Util.to_int;
        end_offset = Util.member "end_offset" e |> Util.to_int;
        report = (try Util.member "report" e |> Util.to_string with _ -> "");
        details =
          (match Util.member "details" e with
          | `Null -> None
          | details_json -> Some (parse_error_details_of_json details_json));
      })

let parse_outcome_of_json ?source_path input json : parse_outcome =
  let source = source_of_input ?source_path input in
  let status = Util.member "status" json |> Util.to_string in
  match status with
  | "ok" ->
      let doc = Util.member "document" json in
      let nodes = Util.member "nodes" doc |> nodes_of_json ~source in
      Parsed nodes
  | "recovered" ->
      let doc = Util.member "document" json in
      let nodes = Util.member "nodes" doc |> nodes_of_json ~source in
      let errors = parse_errors_of_json json in
      Recovered (nodes, errors)
  | "error" ->
      let errors = parse_errors_of_json json in
      Failed errors
  | _ ->
      Failed
        [
          {
            message = "Unknown status: " ^ status;
            start_offset = 0;
            end_offset = 0;
            report = "";
            details = None;
          };
        ]

let parse_with_mode ?(mode : parse_mode = Strict) ?source_path (input : string)
    : parse_outcome =
  match parse_to_json ~mode input with
  | Error msg ->
      Failed
        [
          {
            message = msg;
            start_offset = 0;
            end_offset = 0;
            report = msg;
            details = None;
          };
        ]
  | Ok json_str -> (
      try
        let json = Json.from_string json_str in
        parse_outcome_of_json ?source_path input json
      with
      | Json.Util.Type_error (msg, _) ->
          Failed
            [
              {
                message = "JSON type error: " ^ msg;
                start_offset = 0;
                end_offset = 0;
                report = "";
                details = None;
              };
            ]
      | Failure msg ->
          Failed
            [
              {
                message = "Conversion error: " ^ msg;
                start_offset = 0;
                end_offset = 0;
                report = "";
                details = None;
              };
            ]
      | exn ->
          Failed
            [
              {
                message = "Parse error: " ^ Printexc.to_string exn;
                start_offset = 0;
                end_offset = 0;
                report = "";
                details = None;
              };
            ])

(** Parse input and return Code.t or errors *)
let parse ?source_path (input : string) : (Code.t, parse_error list) result =
  match parse_with_mode ?source_path ~mode:Strict input with
  | Parsed nodes -> Ok nodes
  | Failed errors -> Error errors
  | Recovered (_nodes, errors) -> Error errors

let parse_recovery ?source_path (input : string) : parse_outcome =
  parse_with_mode ?source_path ~mode:Recovery input

(** Parse and return Code.tree structure *)
let parse_tree ?(source_path : string option) (input : string) :
    (Code.tree, parse_error list) result =
  match parse ?source_path input with
  | Error errs -> Error errs
  | Ok code -> Ok { Code.source_path; uri = None; timestamp = None; code }

(** Parse and validate input (returns unit on success) *)
let parse_check (input : string) : (unit, string) result =
  match parse input with
  | Ok _ -> Ok ()
  | Error errs ->
      let msg =
        errs
        |> List.map (fun e -> if e.report <> "" then e.report else e.message)
        |> String.concat "\n"
      in
      Error msg
