(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_core
module Types = Rust_parser_types
module Json = Yojson.Safe
module Util = Yojson.Safe.Util

let source_of_input ?source_path input : Range.source =
  match source_path with
  | Some path -> `File path
  | None -> `String {title = None; content = input}

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

let delim_of_json json =
  match Util.to_string json with
  | "braces" -> Braces
  | "squares" -> Squares
  | "parens" -> Parens
  | s -> failwith ("Unknown delimiter: " ^ s)

let math_mode_of_json json =
  match Util.to_string json with
  | "inline" -> Inline
  | "display" -> Display
  | s -> failwith ("Unknown math mode: " ^ s)

let visibility_of_json json =
  match Util.to_string json with
  | "private" -> Private
  | "public" -> Public
  | s -> failwith ("Unknown visibility: " ^ s)

let binding_of_json json =
  match Util.to_list json with
  | [strictness_json; name_json] ->
    let info =
      match Util.to_string strictness_json with
      | "strict" -> Strict
      | "lazy" -> Lazy
      | s -> failwith ("Unknown binding info: " ^ s)
    in
      (info, Util.to_string name_json)
  | _ -> failwith "Invalid binding format"

let string_list_of_json json = Util.to_list json |> List.map Util.to_string

let member_string_list field json =
  Util.member field json |> string_list_of_json

let member_string field json = Util.member field json |> Util.to_string

let member_string_option field json =
  Util.member field json |> Util.to_string_option

let member_list field json = Util.member field json |> Util.to_list

let rec decode_method_of_json ~source json =
  match Util.to_list json with
  | [name_json; body_json] ->
    let name = Util.to_string name_json in
    let body = nodes_of_json ~source body_json in
      (name, body)
  | _ -> failwith "Invalid method format"

and decode_methods_of_json ~source json =
  member_list "methods" json |> List.map (decode_method_of_json ~source)

and node_of_json ~source json : Code.node =
  let ty = member_string "type" json in
  match ty with
  | "text" -> Code.Text (member_string "content" json)
  | "verbatim" -> Code.Verbatim (member_string "content" json)
  | "comment" -> Code.Comment (member_string "content" json)
  | "error" -> Code.Error (member_string "message" json)
  | "group" ->
    let delim = Util.member "delim" json |> delim_of_json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Group (delim, body)
  | "math" ->
    let mode = Util.member "mode" json |> math_mode_of_json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Math (mode, body)
  | "ident" -> Code.Ident (member_string_list "path" json)
  | "hash_ident" -> Code.Hash_ident (member_string "name" json)
  | "xml_ident" ->
    Code.Xml_ident
      (member_string_option "prefix" json, member_string "name" json)
  | "let" ->
    let path = member_string_list "path" json in
    let bindings = member_list "bindings" json |> List.map binding_of_json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Let (path, bindings, body)
  | "def" ->
    let path = member_string_list "path" json in
    let bindings = member_list "bindings" json |> List.map binding_of_json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Def (path, bindings, body)
  | "fun" ->
    let bindings = member_list "bindings" json |> List.map binding_of_json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Fun (bindings, body)
  | "scope" ->
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Scope body
  | "namespace" ->
    let path = member_string_list "path" json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Namespace (path, body)
  | "open" -> Code.Open (member_string_list "path" json)
  | "put" ->
    let path = member_string_list "path" json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Put (path, body)
  | "default" ->
    let path = member_string_list "path" json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Default (path, body)
  | "get" -> Code.Get (member_string_list "path" json)
  | "alloc" -> Code.Alloc (member_string_list "path" json)
  | "object" ->
    let def = Util.member "def" json in
    let self = member_string_option "self_name" def in
    let methods = decode_methods_of_json ~source def in
    Code.Object {self; methods}
  | "patch" ->
    let def = Util.member "def" json in
    let obj = Util.member "obj" def |> nodes_of_json ~source in
    let self = member_string_option "self_name" def in
    let super = member_string_option "super_name" def in
    let methods = decode_methods_of_json ~source def in
    Code.Patch {obj; self; super; methods}
  | "call" ->
    let target = Util.member "target" json |> nodes_of_json ~source in
    let method_ = member_string "method" json in
    Code.Call (target, method_)
  | "subtree" ->
    let addr = member_string_option "addr" json in
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Subtree (addr, body)
  | "import" ->
    let visibility = Util.member "visibility" json |> visibility_of_json in
    let target = member_string "target" json in
    Code.Import (visibility, target)
  | "decl_xmlns" ->
    Code.Decl_xmlns (member_string "prefix" json, member_string "uri" json)
  | "dx_sequent" ->
    let conclusion = Util.member "conclusion" json |> nodes_of_json ~source in
    let premises =
      member_list "premises" json |> List.map (nodes_of_json ~source)
    in
    Code.Dx_sequent (conclusion, premises)
  | "dx_query" ->
    let var = member_string "var" json in
    let positives =
      member_list "positives" json |> List.map (nodes_of_json ~source)
    in
    let negatives =
      member_list "negatives" json |> List.map (nodes_of_json ~source)
    in
    Code.Dx_query (var, positives, negatives)
  | "dx_prop" ->
    let relation = Util.member "relation" json |> nodes_of_json ~source in
    let args = member_list "args" json |> List.map (nodes_of_json ~source) in
    Code.Dx_prop (relation, args)
  | "dx_var" -> Code.Dx_var (member_string "name" json)
  | "dx_const_content" ->
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Dx_const_content body
  | "dx_const_uri" ->
    let body = Util.member "body" json |> nodes_of_json ~source in
    Code.Dx_const_uri body
  | _ -> Code.Error ("Unknown Rust AST node type: " ^ ty)

and located_node_of_json ~source json : Code.node Range.located =
  let value = Util.member "value" json |> node_of_json ~source in
  let loc =
    match Util.member "span" json with
    | `Null -> None
    | span_json -> Some (range_of_json ~source span_json)
  in
    {loc; value}

and nodes_of_json ~source json : Code.t =
  Util.to_list json |> List.map (located_node_of_json ~source)

let parse_error_label_of_json json : Types.parse_error_label = {
  kind = member_string "kind" json;
  message = member_string "message" json;
  start_offset = Util.member "start_offset" json |> Util.to_int;
  end_offset = Util.member "end_offset" json |> Util.to_int;
}

let parse_error_details_of_json json : Types.parse_error_details = {
  kind = member_string "kind" json;
  expected =
    (try member_list "expected" json |> List.map Util.to_string with _ -> []);
  found = member_string_option "found" json;
  labels = (
    try
      member_list "labels" json |> List.map parse_error_label_of_json
    with
      | _ -> []
  );
  notes =
    (try member_list "notes" json |> List.map Util.to_string with _ -> []);
}

let parse_errors_of_json json : Types.parse_error list =
  member_list "errors" json
  |> List.map (fun error_json ->
      ({
        message = member_string "message" error_json;
        start_offset = Util.member "start_offset" error_json |> Util.to_int;
        end_offset = Util.member "end_offset" error_json |> Util.to_int;
        report =
          (try Util.member "report" error_json |> Util.to_string with _ -> "");
        details = (
          match Util.member "details" error_json with
          | `Null -> None
          | details_json -> Some (parse_error_details_of_json details_json)
        );
      }: Types.parse_error)
    )

let parse_outcome_of_json ?source_path input json : Types.parse_outcome =
  let source = source_of_input ?source_path input in
  match member_string "status" json with
  | "ok" ->
    let nodes =
      Util.member "document" json
      |> Util.member "nodes"
      |> nodes_of_json ~source
    in
    Types.Parsed nodes
  | "recovered" ->
    let nodes =
      Util.member "document" json
      |> Util.member "nodes"
      |> nodes_of_json ~source
    in
    let errors = parse_errors_of_json json in
    Types.Recovered (nodes, errors)
  | "error" ->
    let errors = parse_errors_of_json json in
    Types.Failed errors
  | status ->
    Types.Failed [Types.make_parse_error ("Unknown status: " ^ status)]
