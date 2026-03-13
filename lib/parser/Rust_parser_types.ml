(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_core

type parse_mode = Strict | Recovery

type parse_error_label = {
  kind: string;
  message: string;
  start_offset: int;
  end_offset: int;
}

type parse_error_details = {
  kind: string;
  expected: string list;
  found: string option;
  labels: parse_error_label list;
  notes: string list;
}

type parse_error = {
  message: string;
  start_offset: int;
  end_offset: int;
  report: string;
  details: parse_error_details option;
}

type parse_outcome =
  | Parsed of Code.t
  | Recovered of Code.t * parse_error list
  | Failed of parse_error list

let make_parse_error
  ?(start_offset = 0)
  ?(end_offset = 0)
  ?(report = "")
  ?details
  message
=
  {message; start_offset; end_offset; report; details}
