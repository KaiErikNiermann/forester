(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Message :
sig
  type t =
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
          | `Dx_sequent
          | `Dx_query
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
        (Resolver.Scope.data, Resolver.P.tag) Trie.t * Trie.Untagged.path
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
