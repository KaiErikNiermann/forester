(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Base

module R = Resolver
module Sc = R.Scope

module Message = Reporter_message

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
