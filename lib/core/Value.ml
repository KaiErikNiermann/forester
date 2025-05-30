(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Base

open struct module T = Types end

module Make_env (S : sig include Map.OrderedType val pp : Format.formatter -> t -> unit end) = struct
  include Map.Make(S)
  let pp (pp_el : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (map : 'a t) =
    Format.fprintf fmt "@[<v1>{";
    begin
      let@ k, v = Seq.iter @~ to_seq map in
      Format.fprintf fmt "@[%a ~> %a@]@;" S.pp k pp_el v
    end;
    Format.fprintf fmt "}@]"
end

module Env = Make_env (struct include String let pp = Format.pp_print_string end)
module Symbol_map = Make_env (Symbol)


type t =
  | Content of T.content
  | Clo of t Env.t * string option binding list * Syn.t
  | Dx_prop of (string, T.content T.vertex) Datalog_expr.prop
  | Dx_sequent of (string, T.content T.vertex) Datalog_expr.sequent
  | Dx_query of (string, T.content T.vertex) Datalog_expr.query
  | Dx_var of string
  | Dx_const of Vertex.t
  | Sym of Symbol.t
  | Obj of Symbol.t
[@@deriving show]

type obj_method = {
  body: Syn.t;
  self: string option;
  super: string option;
  env: t Env.t
}
[@@deriving show]

module Method_table = struct
  include Map.Make(String)
  let pp (pp_el : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (map : 'a t) =
    Format.fprintf fmt "@[<v1>{";
    begin
      let@ k, v = Seq.iter @~ to_seq map in
      Format.fprintf fmt "@[%s ~> %a@]@;" k pp_el v
    end;
    Format.fprintf fmt "}@]"
end

type obj = {
  prototype: Symbol.t option;
  methods: obj_method Method_table.t
}
[@@deriving show]
