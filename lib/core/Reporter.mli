(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Message :
sig
  type expected_value =
    | Content
    | Text
    | Obj
    | Bool
    | Sym
    | Dx_query
    | Dx_sequent
    | Dx_prop
    | Datalog_term
    | Node
    | URI
    | Argument
  [@@deriving show]

  type t =
    | Import_not_found of URI.t
    | Invalid_URI
    | Asset_has_no_content_address of string
    | Asset_not_found of string
    | Current_tree_has_no_uri
    | Duplicate_tree of Base.origin * Base.origin
    | Parse_error
    | Unbound_method of (string * Value.obj)
    | Type_warning
    | Type_error of
      {
        got: Value.t option;
        expected: expected_value list
      }
    | Unbound_fluid_symbol of Symbol.t
    | Unbound_variable of string
    | Unresolved_identifier of ((Resolver.Scope.data, Resolver.P.tag) Trie.t) * Trie.path
    | Unresolved_xmlns of string
    | Reference_error of URI.t
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling of float * float
    | External_error
    | Resource_not_found of URI.t
    | Broken_link of {uri: URI.t; suggestion: URI.t option}
    | IO_error
    | Log
    | Missing_argument
    | Uninterpreted_config_options of string list list
    | Using_default_option of string list
    | Required_config_option of string

  val pp :
    Ppx_deriving_runtime.Format.formatter ->
    t ->
    Ppx_deriving_runtime.unit
  val show : t -> Ppx_deriving_runtime.string
  val default_severity : t -> Asai.Diagnostic.severity
  val short_code : t -> string
  val default_text : t -> Asai.Diagnostic.text
end

include module type of Asai.StructuredReporter.Make(Message)
module Tty : module type of Asai.Tty.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t
val log : (Format.formatter -> 'a -> unit) -> 'a -> unit
val profile : string -> (unit -> 'a) -> 'a
val easy_run : (unit -> 'a) -> 'a
val silence : (unit -> 'a) -> 'a
val test_run : (unit -> 'a) -> 'a

val guess_uri : diagnostic -> Lsp.Uri.t option

val ignore :
  ?init_loc: Asai.Range.t ->
  ?init_backtrace: Asai.Diagnostic.backtrace ->
  (unit -> 'a) ->
  'a
