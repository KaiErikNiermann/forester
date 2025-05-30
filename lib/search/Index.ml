(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Spelll

open struct module T = Forester_core.Types end

module Ocurrences = Set.Make(struct
  type t = int list list * URI.t
  (* FIXME: *)
  let compare (_i, x) (_j, y) = URI.compare x y
end)

type t = {
  index: Ocurrences.t Index.t;
  number_of_tokens: int;
  number_of_docs: int;
}

let average_doc_length
  : t -> float
= fun {number_of_tokens; number_of_docs; _} ->
  Float.of_int number_of_tokens /. Float.of_int number_of_docs

let add_one
  : T.content T.article -> t -> t
= fun article ({index; number_of_tokens; number_of_docs;} as t) ->
  if Option.is_none T.(article.frontmatter.uri) then t
  else
    let tokens_in_article = Tokenizer.tokenize_article article in
    let uri = Option.get T.(article.frontmatter.uri) in
    let new_tokens = ref 0 in
    let new_index =
      List.fold_left
        (fun index (ocurrences, token) ->
          match Index.retrieve_l ~limit: 0 index token with
          | [] ->
            (* Unseen token*)
            (* TODO: add to list of ocurrences*)
            let ocurrence = Ocurrences.singleton ([ocurrences], uri) in
            new_tokens := !new_tokens + 1;
            Index.add index token ocurrence
          | ids :: [] ->
            Index.add index token (Ocurrences.add ([ocurrences], uri) ids)
          | _ ->
            (* We are using limit=0, so this shouldn't happen*)
            assert false
        )
        index
        tokens_in_article
    in
    {
      index = new_index;
      number_of_docs = number_of_docs + 1;
      number_of_tokens = number_of_tokens + !new_tokens
    }

let add
    : T.content T.article list -> t -> t
  =
  List.fold_right add_one

let search
  : ?fuzz: int -> t -> string -> (int list list * URI.t) list
= fun ?(fuzz = 0) index term ->
  Tokenizer.tokenize term
  |> List.concat_map
      (fun str ->
        List.concat_map Ocurrences.to_list @@
          Index.retrieve_l ~limit: fuzz index.index str
      )

module BM_25 = struct
  let sum = List.fold_left (+.) 0.

  (* Inverse document frequency *)
  let idf q (index : t) =
    let n = Float.of_int @@ List.length @@ search ~fuzz: 0 index q in
    log @@ ((Float.of_int index.number_of_docs -. n +. 0.5) /. n +. 0.5) +. 1.

  let doc_length d =
    Float.of_int @@
    List.length @@
    Tokenizer.tokenize_article d

  let score
    : T.content T.article -> string -> t -> float
  = fun d q index ->
    let tokens = Tokenizer.tokenize q in
    assert (List.length tokens > 0);
    let avg_len = average_doc_length index in
    let k_1 = 1.5 in
    let b = 0.75 in
    sum @@
      List.map
        (fun q_i ->
          let num_occurrences =
            Float.of_int @@
            List.length @@ search index q_i
          in
          (* Format.printf "num_occurrences: %f" num_occurrences; *)
          idf q index *.
            begin
              (num_occurrences *. k_1 +. 1.) /.
                (num_occurrences +. k_1 *. (1. -. b +. (b *. doc_length d /. avg_len))) +.
                1.
            end
        )
        tokens
end

let create articles =
  let index = {
    index = Index.empty;
    number_of_docs = 0;
    number_of_tokens = 0
  }
  in
  add articles index

let marshal (v : t) filename =
  let oc = open_out_bin filename in
  Fun.protect
    ~finally: (fun () -> close_out oc)
    (fun () -> Marshal.to_channel oc v [])

let unmarshal filename : t =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally: (fun () -> close_in ic)
    (fun () -> Marshal.from_channel ic)
