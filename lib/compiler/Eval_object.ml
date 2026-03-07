(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module String_map = Value.String_map
module Symbol_map = Value.Symbol_map

let build_object ~prototype ~env ~self ~super methods =
  let add (name, body) =
    Value.Method_table.add name Value.{ body; self; super; env }
  in
  let methods = List.fold_right add methods Value.Method_table.empty in
  let sym = Symbol.named [ "obj" ] in
  (sym, Value.{ prototype; methods })

let call_method ~loc ~sym ~method_name ~heap ~eval_body =
  let rec call_method_on (obj : Value.obj) =
    let proto_val = obj.prototype |> Option.map (fun ptr -> Value.Obj ptr) in
    match Value.Method_table.find_opt method_name obj.methods with
    | Some mthd ->
        let env =
          let env =
            match mthd.self with
            | None -> mthd.env
            | Some self -> String_map.add self (Value.Obj sym) mthd.env
          in
          match proto_val with
          | None -> env
          | Some proto_val -> (
              match mthd.super with
              | None -> env
              | Some super -> String_map.add super proto_val env)
        in
        eval_body env mthd.body
    | None -> (
        match obj.prototype with
        | Some proto -> call_method_on @@ Symbol_map.find proto heap
        | None -> Reporter.fatal ?loc (Unbound_method (method_name, obj)))
  in
  call_method_on @@ Symbol_map.find sym heap
