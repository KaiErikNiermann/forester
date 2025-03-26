(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Base

module R = Resolver
module Sc = R.Scope

module Message = struct
  type t =
    | Import_not_found of URI.t
    | Invalid_URI
    | Tree_not_found of URI.t
    | Asset_has_no_content_address of string
    | Duplicate_tree of URI.t
    | Parse_error
    | Unbound_method of (string * Value.obj)
    | Type_warning
    | Type_error of
      {
        got: Value.t option;
        expected:
        [
          | `Content
          | `Text
          | `Obj
          | `Bool
          | `Sym
          | `Dx_query
          | `Dx_sequent
          | `Dx_prop
          | `Datalog_term
          | `Node
          | `URI
          | `Argument
        ] list
      }
    | Resolution_error of (Symbol.t * Value.t Value.Env.t)
    | Expansion_error of
      [
        | `Resolution_error of
        (((Sc.data, R.P.tag) Trie.t [@opaque])
        * (Yuujinchou.Trie.Untagged.path [@printer Format.(pp_print_list pp_print_string)]))
        | `Xmlns_error
      ]
    | Resolution_warning
    | Reference_error of URI.t
    | Duplicate_attribute
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling of float * float
    | External_error
    | Resource_not_found of URI.t
    | Broken_link of URI.t
    | IO_error
    | Log
    | Missing_argument
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity = function
    | Import_not_found _ -> Error
    | Expansion_error _ -> Error
    | Invalid_URI -> Error
    | Unbound_method _ -> Error
    | Asset_has_no_content_address _ -> Error
    | Reference_error _ -> Error
    | Duplicate_tree _ -> Error
    | Tree_not_found _ -> Error
    | Parse_error -> Error
    | Type_error _ -> Error
    | Type_warning -> Warning
    | Resolution_error _ -> Error
    | Resolution_warning -> Warning
    | Duplicate_attribute -> Error
    | Unhandled_case -> Bug
    | Transclusion_loop -> Error
    | Internal_error -> Bug
    | Configuration_error -> Error
    | Initialization_warning -> Warning
    | Routing_error -> Error
    | Profiling _ -> Info
    | External_error -> Error
    | Log -> Info
    | Resource_not_found _ -> Error
    | Broken_link _ -> Warning
    | IO_error -> Error
    | Missing_argument -> Error

  let short_code : t -> string = function
    | Import_not_found _ -> "import_not_found"
    | Invalid_URI -> "invalid_uri"
    | Tree_not_found _ -> "tree_not_found"
    | Asset_has_no_content_address _ -> "asset_not_found" (* This is taken from the original wording of the message, but I think this is very confusing.*)
    | Duplicate_tree _ -> "duplicate_tree"
    | Parse_error -> "parse_error"
    | Unbound_method _ -> "unbound_method"
    | Type_warning -> "type_warning"
    | Type_error _ -> "type_error"
    | Resolution_error _ -> "resolution_error"
    | Expansion_error _ -> "expansion_error"
    | Resolution_warning -> "resolution_warning"
    | Reference_error _ -> "reference_error"
    | Duplicate_attribute -> "duplicate_attribute"
    | Unhandled_case -> "unhandled_case"
    | Transclusion_loop -> "transclusion_loop"
    | Internal_error -> "internal_error"
    | Configuration_error -> "configuration_error"
    | Initialization_warning -> "initialization_warning"
    | Routing_error -> "routing_error"
    | Profiling (_, _) -> "profiling"
    | External_error -> "external_error"
    | Resource_not_found _ -> "resource_not_found"
    | Broken_link _ -> "broken_link"
    | IO_error -> "io_error"
    | Log -> "log"
    | Missing_argument -> "missing_argument"

  let default_text : t -> Asai.Diagnostic.text = function
    | Import_not_found uri -> Asai.Diagnostic.textf "%a not found" URI.pp uri
    | Expansion_error _
    | Invalid_URI
    | Unbound_method _
    | Asset_has_no_content_address _
    | Reference_error _
    | Tree_not_found _
    | Duplicate_tree _
    | Parse_error
    | Type_error _
    | Type_warning
    | Resolution_error _
    | Resolution_warning
    | Duplicate_attribute
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling _
    | External_error
    | Resource_not_found _
    | Broken_link _
    | IO_error
    | Log
    | Missing_argument ->
      Asai.Diagnostic.text ""
end

include Asai.StructuredReporter.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t

let log pp s =
  Logs.info (fun m -> m "%a...@." pp s)

let profile msg body =
  let before = Unix.gettimeofday () in
  let result = body () in
  let after = Unix.gettimeofday () in
  emit (Profiling (after, before));
  (* "[%fs] %s" (after -. before) msg; *)
  result

module Tty = Asai.Tty.Make(Message)

let easy_run k =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  run ~emit: Tty.display ~fatal k

let silence k =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  run ~emit: Tty.display ~fatal k

let test_run k =
  let fatal diagnostics =
    Tty.display
      ~use_color: false
      ~use_ansi: false
      diagnostics;
    exit 1
  in
  let emit _diagnostics = () in
  run ~emit ~fatal k

(* Reporting diagnostics requires a document URI to publish *)
let guess_uri (d : diagnostic) =
  match d with
  | {explanation; _} ->
    match explanation.loc with
    | None -> None
    | Some loc ->
      match Range.view loc with
      | `End_of_file {source; _}
      | `Range ({source; _}, _) ->
        match source with
        | `String _ -> None
        | `File path ->
          if path <> "" then
            Some (Lsp.Uri.of_path path)
          else None

let ignore =
  let emit _ = () in
  let fatal _ = fatal Message.Internal_error ~extra_remarks: [Asai.Diagnostic.loctext "ignoring error"] in
  run ~emit ~fatal
