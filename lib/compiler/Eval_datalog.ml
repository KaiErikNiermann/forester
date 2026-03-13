(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module T = Types

type located = Value.t Range.located

let extract_dx_term (node : located) =
  match node.value with
  | Dx_var name -> Datalog_expr.Var name
  | Dx_const vtx -> Datalog_expr.Const vtx
  | other ->
    Reporter.fatal
      ?loc: node.loc
      (Type_error {expected = [Datalog_term]; got = Some other})

let extract_dx_prop (node : located) =
  match node.value with
  | Dx_prop prop -> prop
  | other ->
    Reporter.fatal
      ?loc: node.loc
      (Type_error {expected = [Dx_prop]; got = Some other})

let extract_dx_sequent (node : located) =
  match node.value with
  | Dx_sequent sequent -> sequent
  | other ->
    Reporter.fatal
      ?loc: node.loc
      (Type_error {expected = [Dx_sequent]; got = Some other})

let eval_prop ~loc ~rel ~args ~eval_tape ~extract_text ~focus =
  let rel = Range.locate_opt loc (eval_tape rel) |> extract_text in
  let args =
    let@ arg = List.map @~ args in
    Range.locate_opt loc (eval_tape arg) |> extract_dx_term
  in
  focus ?loc @@ Value.Dx_prop {rel; args}

let eval_sequent ~loc ~conclusion ~premises ~eval_tape ~focus =
  let conclusion =
    Range.locate_opt loc (eval_tape conclusion) |> extract_dx_prop
  in
  let premises =
    let@ premise = List.map @~ premises in
    Range.locate_opt loc (eval_tape premise) |> extract_dx_prop
  in
  focus ?loc @@ Value.Dx_sequent {conclusion; premises}

let eval_query ~loc ~var ~positives ~negatives ~eval_tape ~focus =
  let positives =
    let@ premise = List.map @~ positives in
    Range.locate_opt loc (eval_tape premise) |> extract_dx_prop
  in
  let negatives =
    let@ premise = List.map @~ negatives in
    Range.locate_opt loc (eval_tape premise) |> extract_dx_prop
  in
  focus ?loc @@ Value.Dx_query {var; positives; negatives}

let eval_const
    ~loc
    ~type_
    ~arg
    ~eval_tape
    ~extract_content
    ~extract_uri
    ~focus
  =
  let arg = Range.locate_opt loc (eval_tape arg) in
  let const =
    match type_ with
    | `Content -> T.Content_vertex (extract_content arg)
    | `Uri ->
      (
        match extract_uri arg with
        | Ok uri -> T.Uri_vertex uri
        | Error _ ->
          Reporter.fatal
            ?loc
            Invalid_URI
            ~extra_remarks: [
              Asai.Diagnostic.loctext
                "Expected valid URI in datalog constant expression.";
            ]
      )
  in
  focus ?loc @@ Value.Dx_const const

let eval_execute ~loc ~eval_pop_arg ~emit_content_node =
  let script = eval_pop_arg ~loc |> extract_dx_sequent in
  emit_content_node ~loc @@ T.Datalog_script [script]
