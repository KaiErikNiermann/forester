(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Message :
sig
  type t =
    | Tree_not_found of URI.t
    | Duplicate_tree of URI.t
    | Parse_error
    | Type_error
    | Type_warning
    | Resolution_error
    | Resolution_warning
    | Duplicate_attribute
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling
    | External_error
    | Resource_not_found
    | Broken_link
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
end

include module type of Asai.Reporter.Make(Message)
module Tty : module type of Asai.Tty.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t
val log : (Format.formatter -> 'a -> unit) -> 'a -> unit
val profile : string -> (unit -> 'a) -> 'a
val easy_run : (unit -> 'a) -> 'a
val silence : (unit -> 'a) -> 'a
val test_run : (unit -> 'a) -> 'a

val guess_uri : diagnostic -> Lsp.Uri.t option

val lsp_run :
  ?init_loc: Range.t ->
  ?init_backtrace: Asai.Diagnostic.backtrace ->
  recover: (diagnostic -> 'a) ->
  ((Lsp.Uri.t, diagnostic list) Hashtbl.t -> unit) ->
  (unit -> 'a) ->
  'a

val ignore :
  ?init_loc: Asai.Range.t ->
  ?init_backtrace: Asai.Diagnostic.backtrace ->
  (unit -> 'a) ->
  'a
