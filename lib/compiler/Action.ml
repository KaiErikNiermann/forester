(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type exit =
  Fail | Finished
[@@deriving show]

type t =
  | Quit of exit
  | Build_import_graph
  | Plant_assets
  | Plant_foreign
  | Done
  | Load_all_configured_dirs
  | Parse_all
  | Expand_all
  | Eval_all
  | Load_tree of (Eio.Fs.dir_ty Eio.Path.t [@printer Eio.Path.pp])
  | Parse of (Lsp.Uri.t [@printer fun fmt uri -> fprintf fmt "%s" (Lsp.Uri.to_string uri)])
  | Expand of URI.t
  | Eval of URI.t
  | Query of (string, Vertex.t) Datalog_expr.query
  | Query_results of (Vertex_set.t [@opaque])
  | Report_errors of ((Reporter.Message.t Asai.Diagnostic.t [@opaque]) list * t)
  | Run_jobs of Job.job Range.located list
[@@deriving show]

let report ~next_action ~errors =
  if List.length errors > 0 then
    Report_errors (errors, next_action)
  else next_action
