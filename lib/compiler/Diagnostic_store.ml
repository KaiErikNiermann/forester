(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module Table = Hashtbl.Make(Lsp.Uri)
include Table

type t = Reporter.diagnostic list Table.t

let replace (table : 'a list Table.t) (fresh_diagnostics : 'a list) : unit =
  let diags = Hashtbl.create 100 in
  begin
    let@ d = List.iter @~ fresh_diagnostics in
    match Reporter.guess_uri d with
    | None ->
      Reporter.fatal
        Internal_error
        ~extra_remarks: [Asai.Diagnostic.loctextf "Dropped a diagnostic because its URI could not be guessed"]
    | Some uri ->
      Hashtbl.replace diags uri [d]
  end;
  let@ uri, ds = Seq.iter @~ Hashtbl.to_seq diags in
  assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
  Table.replace table uri ds

let add (table : 'a list Table.t) (fresh_diagnostics : 'a list) : unit =
  let diagnostics = Hashtbl.create 100 in
  begin
    let@ d = List.iter @~ fresh_diagnostics in
    match Reporter.guess_uri d with
    | None ->
      Reporter.fatal
        Internal_error
        ~extra_remarks: [Asai.Diagnostic.loctextf "Dropped a diagnostic because its URI could not be guessed"]
    | Some uri ->
      let t = Option.value ~default: [] @@ Hashtbl.find_opt diagnostics uri in
      Hashtbl.replace diagnostics uri (d :: t)
  end;
  let@ uri, ds = Seq.iter @~ Hashtbl.to_seq diagnostics in
  assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
  Table.replace table uri @@
    match Table.find_opt table uri with
    | None -> ds
    | Some previous -> ds @ previous
