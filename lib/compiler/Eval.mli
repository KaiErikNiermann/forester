(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module T := Types

type result = {
  articles: T.content T.article list;
  jobs: Job.job Range.located list
}
[@@deriving show]

val eval_tree :
  host: string ->
  iri: iri ->
  source_path: string option ->
  Syn.t ->
  result * (Lsp.Uri.t, Reporter.diagnostic list) Hashtbl.t
