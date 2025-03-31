(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Types

type latex_to_svg_job = {
  hash: string;
  source: string;
}
[@@deriving show]

let uri_for_latex_to_svg_job ~(base : URI.t) (job : latex_to_svg_job) =
  URI_scheme.named_uri ~base @@ job.hash ^ ".svg"

type job =
  | LaTeX_to_svg of latex_to_svg_job
  | Syndicate of content syndication
[@@deriving show]
