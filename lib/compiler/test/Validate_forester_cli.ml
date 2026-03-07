(******************************************************************************)
(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors             *)
(* SPDX-License-Identifier: GPL-3.0-or-later                                  *)
(******************************************************************************)

open Forester_compiler
open Forester_core

let usage () =
  prerr_endline "Usage: Validate_forester_cli.exe FILE..."

let parse_file path =
  let content = In_channel.with_open_bin path In_channel.input_all in
  match Parse.parse_content ~filename: path content with
  | Ok _ -> Ok ()
  | Error diagnostic ->
    Error (Asai.Diagnostic.string_of_text diagnostic.Asai.Diagnostic.explanation.value)

let () =
  let paths = Array.to_list Sys.argv |> List.tl in
  match paths with
  | [] ->
    usage ();
    exit 2
  | _ ->
    let failures =
      List.fold_left
        (fun count path ->
          match parse_file path with
          | Ok() -> count
          | Error explanation ->
            prerr_endline ("Validation failed for " ^ path ^ ":");
            prerr_endline explanation;
            count + 1
        )
        0
        paths
    in
    if failures > 0 then
      exit 1
