(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

include Asai.Range

let pp_located pp_arg fmt (x : 'a located) =
  pp_arg fmt x.value

let map f node = {node with value = f node.value}

type string_source = Asai.Range.string_source = {
  title: string option;
  content: string;
}
[@@deriving repr]

type source = [
  | `File of string
  | `String of string_source
]
[@@deriving repr]

type position = Asai.Range.position = {
  source: source;
  offset: int;
  start_of_line: int;
  line_num: int;
}
[@@deriving repr]

let t : t Repr.t =
  let open Repr in
  variant
    "t"
    begin
      fun range end_of_file t ->
        match view t with
        | `Range (x, y) -> range (x, y)
        | `End_of_file x -> end_of_file x
    end
  |~ case1 "Range" (pair position_t position_t) make
  |~ case1 "End_of_file" position_t eof
  |> sealv

type 'a located = 'a Asai.Range.located =
  {loc: t option; value: 'a}
[@@deriving repr]

(* Menhir uses [Lexing.dummy_pos] (whose [pos_fname] is "") for empty
   productions. Locating a node whose span pairs such a dummy endpoint with a
   real token's position makes [Asai.Range.make] raise [Invalid_argument]
   ("...source... do not match"), aborting the entire build — e.g. on a
   malformed datalog query. Unify the two endpoints' source before locating so
   the parse instead yields a clean diagnostic. Shadows [Asai.Range.locate_lex]. *)
let locate_lex ((start, stop) : Lexing.position * Lexing.position) (value : 'a) : 'a located =
  let fname =
    if String.length start.Lexing.pos_fname > 0 then start.Lexing.pos_fname
    else stop.Lexing.pos_fname
  in
  let fix (p : Lexing.position) =
    if String.equal p.Lexing.pos_fname fname then p
    else {p with Lexing.pos_fname = fname}
  in
  Asai.Range.locate_lex (fix start, fix stop) value
