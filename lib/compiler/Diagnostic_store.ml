(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_core

module Table = Hashtbl.Make(Lsp.Uri)
include Table

type t = Reporter.diagnostic list Table.t

let replace
  : 'a list Table.t -> 'a list -> unit
= fun table fresh_diagnostics ->
  let diags = Hashtbl.create 100 in
  fresh_diagnostics
  |> List.iter
      (fun d ->
        match Reporter.guess_uri d with
        | None ->
          Reporter.fatal
            Internal_error
            ~extra_remarks: [Asai.Diagnostic.loctextf "Dropped a diagnostic because its URI could not be guessed"]
        | Some uri ->
          Hashtbl.replace diags uri [d]
      );
  diags
  |> Hashtbl.to_seq
  |> Seq.iter
      (fun (uri, ds) ->
        assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
        Table.replace table uri ds
      )

let add
  : 'a list Table.t -> 'a list -> unit
= fun table fresh_diagnostics ->
  let diagnostics = Hashtbl.create 100 in
  fresh_diagnostics
  |> List.iter
      (fun d ->
        match Reporter.guess_uri d with
        | None ->
          Reporter.fatal
            Internal_error
            ~extra_remarks: [Asai.Diagnostic.loctextf "Dropped a diagnostic because its URI could not be guessed"]
        | Some uri ->
          match Hashtbl.find_opt diagnostics uri with
          | None -> Hashtbl.replace diagnostics uri [d]
          | Some t -> Hashtbl.replace diagnostics uri (d :: t)
      );
  diagnostics
  |> Hashtbl.to_seq
  |> Seq.iter
      (fun (uri, ds) ->
        assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
        match Table.find_opt table uri with
        | None ->
          Table.replace table uri ds
        | Some previous ->
          Table.replace table uri (ds @ previous)
      )
