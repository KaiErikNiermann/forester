(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open struct
  module L = Lsp.Types
end

let rec random_not_in keys =
  let attempt = Random.int (36 * 36 * 36 * 36 - 1) in
  if List.fold_left (fun x y -> x || y) false (List.map (fun k -> k = attempt) keys) then
    random_not_in keys
  else
    attempt

let next_uri ~(prefix : string option) ~(mode : [< `Random | `Sequential]) ~(forest : State.t) : string =
  let addrs =
    forest.index
    |> URI.Tbl.to_seq
    |> Seq.map fst
    |> List.of_seq
  in
  let keys =
    let@ uri = List.filter_map @~ addrs in
    let@ prefix', key = Option.bind @@ URI_scheme.split_addr uri in
    if prefix = prefix' then Some key else None
  in
  let next =
    match mode with
    | `Sequential ->
      let last_sequential = List.fold_left (fun acc_i i -> if i > acc_i then i else acc_i ) 0 keys in
      last_sequential + 1
    | `Random -> random_not_in keys
  in
  (match prefix with (None | Some "") -> "" | Some prefix -> prefix ^ "-") ^ BaseN.Base36.string_of_int next

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
