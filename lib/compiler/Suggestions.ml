(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)
 open Forester_core
open Bwd

(* TODO: remove this in favor of https://github.com/ocaml/ocaml/pull/13760 *)
let edit_distance ~cutoff x y =
  let len_x, len_y = String.length x, String.length y in
  let grid = Array.make_matrix (len_x + 1) (len_y + 1) 0 in
  for i = 1 to len_x do
    grid.(i).(0) <- i;
  done;
  for j = 1 to len_y do
    grid.(0).(j) <- j;
  done;
  for j = 1 to len_y do
    for i = 1 to len_x do
      let cost = if x.[i - 1] = y.[j - 1] then 0 else 1 in
      let k = Int.min (grid.(i - 1).(j) + 1) (grid.(i).(j - 1) + 1) in
      grid.(i).(j) <- Int.min k (grid.(i - 1).(j - 1) + cost)
    done;
  done;
  let result = grid.(len_x).(len_y) in
  if result > cutoff then None
  else
    Some result

let suggestions
    ?prefix
    ~(cutoff : int)
    (p : Trie.bwd_path)
    : ('data, 'tag) Trie.t -> ('data, int) Trie.t
  =
  let compare p d =
    edit_distance ~cutoff (String.concat "" (Bwd.to_list p)) (String.concat "" (Bwd.to_list d))
  in
  Trie.filter_map
    ?prefix
    (fun q (data, _) ->
      match compare p q with
      | Some i ->
        if i > cutoff then
          None
        else
          (Some (data, i))
      | None -> None
    )

let suggestions ~visible path =
  suggestions ~cutoff: 2 (Bwd.of_list path) visible
  |> Trie.to_seq
  |> Seq.map (fun (path, (data, distance)) -> (path, data, distance))
  |> List.of_seq
  |> List.sort (fun (_, _, a) (_, _, b) -> Int.compare a b)

let create_suggestions ~visible path =
  let suggestions = suggestions ~visible path in
  let extra_remarks =
    if List.length suggestions > 0 then
      let (path, data, _) = List.hd suggestions in
      let location_hint =
        match data with
        | Syn.Term ({loc = Some loc; _} :: _) ->
          begin
            match Range.view loc with
            | `End_of_file {source; _}
            | `Range ({source; _}, _) ->
              match Range.title source with
              | Some string ->
                [Asai.Diagnostic.loctextf "defined in %s" string]
              | _ -> []
          end
        | _ -> []
      in
      [Asai.Diagnostic.loctextf "Did you mean %a?" Trie.pp_path path] @ location_hint
    else []
  in
  extra_remarks
