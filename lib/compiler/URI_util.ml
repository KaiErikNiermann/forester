(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module L = Lsp.Types

let rec random_not_in keys =
  let attempt = Random.int (36 * 36 * 36 * 36 - 1) in
  if List.fold_left (fun x y -> x || y) false (List.map (fun k -> k = attempt) keys) then
    random_not_in keys
  else
    attempt

let next_uri
    ~(prefix : string option)
    ~(mode : [< `Random | `Sequential])
    ~(forest : State.t)
    : string * string option
  =
  let addrs =
    forest.index
    |> URI.Tbl.to_seq_keys
    |> Seq.filter_map
        (fun uri ->
          match URI.Tbl.find_opt forest.resolver uri with
          | None -> None
          | Some path ->
            Some (uri, path)
        )
    |> List.of_seq
  in
  let default_dir = Option.map Unix.realpath @@ List.nth_opt forest.config.trees 0 in
  let keys =
    let@ (addr, uri) = List.filter_map @~ addrs in
    let@ prefix', key = Option.bind @@ URI_scheme.split_addr addr in
    if prefix = prefix'
    then Some (key, Filename.dirname uri)
    else None
  in
  let last_sequential, dir =
    List.fold_left
      (fun (acc_i, acc_uri) (i, uri) ->
        if i > acc_i then (i, Some uri) else (acc_i, acc_uri)
      )
      (0, default_dir)
      keys
  in
  let next =
    match mode with
    | `Sequential -> last_sequential + 1
    | `Random -> random_not_in @@ List.map fst keys
  in
  (match prefix with None -> "" | Some "" -> "" | Some prefix -> prefix ^ "-") ^ BaseN.Base36.string_of_int next, dir

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
