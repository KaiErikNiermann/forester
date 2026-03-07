(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
open Forester_core

let string value = `String value
let list to_yojson values = `List (List.map to_yojson values)
let option to_yojson = function None -> `Null | Some value -> to_yojson value

let binding_info_to_yojson = function
  | Strict -> string "strict"
  | Lazy -> string "lazy"

let binding_to_yojson (info, name) =
  `List [ binding_info_to_yojson info; string name ]

let delim_to_yojson = function
  | Braces -> string "braces"
  | Squares -> string "squares"
  | Parens -> string "parens"

let math_mode_to_yojson = function
  | Inline -> string "inline"
  | Display -> string "display"

let visibility_to_yojson = function
  | Private -> string "private"
  | Public -> string "public"

let rec document_to_yojson (code : Code.t) =
  `Assoc [ ("nodes", list located_node_to_yojson code) ]

and located_node_to_yojson
    ({ Asai.Range.value; _ } : Code.node Asai.Range.located) =
  `Assoc [ ("value", node_to_yojson value) ]

and node_to_yojson (node : Code.node) =
  match node with
  | Code.Text content ->
      `Assoc [ ("type", string "text"); ("content", string content) ]
  | Code.Verbatim content ->
      `Assoc [ ("type", string "verbatim"); ("content", string content) ]
  | Code.Comment content ->
      `Assoc [ ("type", string "comment"); ("content", string content) ]
  | Code.Error message ->
      `Assoc [ ("type", string "error"); ("message", string message) ]
  | Code.Group (delim, body) ->
      `Assoc
        [
          ("type", string "group");
          ("delim", delim_to_yojson delim);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Math (mode, body) ->
      `Assoc
        [
          ("type", string "math");
          ("mode", math_mode_to_yojson mode);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Ident path ->
      `Assoc [ ("type", string "ident"); ("path", list string path) ]
  | Code.Hash_ident name ->
      `Assoc [ ("type", string "hash_ident"); ("name", string name) ]
  | Code.Xml_ident (prefix, name) ->
      `Assoc
        [
          ("type", string "xml_ident");
          ("prefix", option string prefix);
          ("name", string name);
        ]
  | Code.Subtree (addr, body) ->
      `Assoc
        [
          ("type", string "subtree");
          ("addr", option string addr);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Let (path, bindings, body) ->
      `Assoc
        [
          ("type", string "let");
          ("path", list string path);
          ("bindings", list binding_to_yojson bindings);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Def (path, bindings, body) ->
      `Assoc
        [
          ("type", string "def");
          ("path", list string path);
          ("bindings", list binding_to_yojson bindings);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Fun (bindings, body) ->
      `Assoc
        [
          ("type", string "fun");
          ("bindings", list binding_to_yojson bindings);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Scope body ->
      `Assoc
        [ ("type", string "scope"); ("body", list located_node_to_yojson body) ]
  | Code.Namespace (path, body) ->
      `Assoc
        [
          ("type", string "namespace");
          ("path", list string path);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Open path ->
      `Assoc [ ("type", string "open"); ("path", list string path) ]
  | Code.Put (path, body) ->
      `Assoc
        [
          ("type", string "put");
          ("path", list string path);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Default (path, body) ->
      `Assoc
        [
          ("type", string "default");
          ("path", list string path);
          ("body", list located_node_to_yojson body);
        ]
  | Code.Get path ->
      `Assoc [ ("type", string "get"); ("path", list string path) ]
  | Code.Alloc path ->
      `Assoc [ ("type", string "alloc"); ("path", list string path) ]
  | Code.Object { self; methods } ->
      `Assoc
        [
          ("type", string "object"); ("def", object_def_to_yojson self methods);
        ]
  | Code.Patch { obj; self; super; methods } ->
      `Assoc
        [
          ("type", string "patch");
          ("def", patch_def_to_yojson obj self super methods);
        ]
  | Code.Call (target, method_name) ->
      `Assoc
        [
          ("type", string "call");
          ("target", list located_node_to_yojson target);
          ("method", string method_name);
        ]
  | Code.Import (visibility, target) ->
      `Assoc
        [
          ("type", string "import");
          ("visibility", visibility_to_yojson visibility);
          ("target", string target);
        ]
  | Code.Decl_xmlns (prefix, uri) ->
      `Assoc
        [
          ("type", string "decl_xmlns");
          ("prefix", string prefix);
          ("uri", string uri);
        ]
  | Code.Dx_sequent (conclusion, premises) ->
      `Assoc
        [
          ("type", string "dx_sequent");
          ("conclusion", list located_node_to_yojson conclusion);
          ("premises", list (list located_node_to_yojson) premises);
        ]
  | Code.Dx_query (var, positives, negatives) ->
      `Assoc
        [
          ("type", string "dx_query");
          ("var", string var);
          ("positives", list (list located_node_to_yojson) positives);
          ("negatives", list (list located_node_to_yojson) negatives);
        ]
  | Code.Dx_prop (relation, args) ->
      `Assoc
        [
          ("type", string "dx_prop");
          ("relation", list located_node_to_yojson relation);
          ("args", list (list located_node_to_yojson) args);
        ]
  | Code.Dx_var name ->
      `Assoc [ ("type", string "dx_var"); ("name", string name) ]
  | Code.Dx_const_content body ->
      `Assoc
        [
          ("type", string "dx_const_content");
          ("body", list located_node_to_yojson body);
        ]
  | Code.Dx_const_uri body ->
      `Assoc
        [
          ("type", string "dx_const_uri");
          ("body", list located_node_to_yojson body);
        ]

and object_def_to_yojson self methods =
  `Assoc
    [
      ("self_name", option string self);
      ("methods", list method_to_yojson methods);
    ]

and patch_def_to_yojson obj self super methods =
  `Assoc
    [
      ("obj", list located_node_to_yojson obj);
      ("self_name", option string self);
      ("super_name", option string super);
      ("methods", list method_to_yojson methods);
    ]

and method_to_yojson (name, body) =
  `List [ string name; list located_node_to_yojson body ]

let failure_to_yojson diagnostic =
  let explanation =
    Asai.Diagnostic.string_of_text diagnostic.Asai.Diagnostic.explanation.value
  in
  `Assoc [ ("status", string "failed"); ("diagnostic", string explanation) ]

let parse_result_to_yojson = function
  | Ok code ->
      `Assoc
        [ ("status", string "parsed"); ("document", document_to_yojson code) ]
  | Error diagnostic -> failure_to_yojson diagnostic

let () =
  let input = In_channel.input_all In_channel.stdin in
  parse_string_no_loc input |> parse_result_to_yojson
  |> Yojson.Safe.pretty_to_channel stdout;
  output_char stdout '\n'
