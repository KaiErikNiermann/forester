(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Base

module Message = struct
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
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity = function
    | Duplicate_tree _ -> Error
    | Tree_not_found _ -> Error
    | Parse_error -> Error
    | Type_error -> Error
    | Type_warning -> Warning
    | Resolution_error -> Error
    | Resolution_warning -> Warning
    | Duplicate_attribute -> Error
    | Unhandled_case -> Bug
    | Transclusion_loop -> Error
    | Internal_error -> Bug
    | Configuration_error -> Error
    | Initialization_warning -> Warning
    | Routing_error -> Error
    | Profiling -> Info
    | External_error -> Error
    | Log -> Info
    | Resource_not_found -> Error
    | Broken_link -> Warning
    | IO_error -> Error
    | Missing_argument -> Error

  let short_code : t -> string =
    show
end

include Asai.Reporter.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t

let log pp s =
  Logs.info (fun m -> m "%a...@." pp s)

let profile msg body =
  let before = Unix.gettimeofday () in
  let result = body () in
  let after = Unix.gettimeofday () in
  emitf Profiling "[%fs] %s" (after -. before) msg;
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

let lsp_run ?init_loc ?init_backtrace ~recover publish k =
  let diagnostics = Hashtbl.create 100 in
  let push_diagnostic d =
    match guess_uri d with
    | None -> fatal Internal_error "dropped a diagnostic because URI could not be guessed."
    | Some uri ->
      match Hashtbl.find_opt diagnostics uri with
      | None ->
        Hashtbl.add diagnostics uri [d];
        recover d
      | Some previous ->
        Hashtbl.add diagnostics uri (d :: previous);
        recover d
  in
  let emit d = ignore @@ push_diagnostic d in
  let fatal = push_diagnostic in
  let@ () = run ~emit ~fatal ?init_loc ?init_backtrace in
  let result = k () in
  publish diagnostics;
  result

let ignore =
  let emit _ = () in
  let fatal _ = fatalf Message.Internal_error "ignoring error" in
  run ~emit ~fatal
