(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
(* open Forester_compiler *)
open Forester_core

open struct
  module R = Resolver
  module Sc = R.Scope
end

module L = Lsp.Types

module Item = struct
  type t =
    | Path of Trie.path
    | Addr of string
  let addr str = Addr str
  let path p = Path p
end

module S = Algaeff.Sequencer.Make(struct
  type t = Item.t Range.located
end)

let flatten (tree : Code.t) : Code.t =
  List.concat_map Code.children tree

let paths_in_bindings =
  List.map snd

(* This function should not descend into the nodes!*)
let paths : Code.node Range.located -> _ = function
  | {value; loc;} ->
    match value with
    | Ident path
    | Open (path)
    | Put (path, _)
    | Default (path, _)
    | Get path
    | Alloc path
    | Namespace (path, _) ->
      Some ([path], loc)
    | Def (path, bindings, _)
    | Let (path, bindings, _) ->
      Some (path :: paths_in_bindings bindings, loc)
    | Patch {self; _}
    | Object {self; _;} ->
      Option.map (fun path -> [path], loc) self
    | Fun (bindings, _) -> Some (paths_in_bindings bindings, loc)
    | Subtree _
    | Group _
    | Scope _
    | Math _
    | Dx_sequent _
    | Dx_const_uri _
    | Dx_const_content _
    | Dx_query _
    | Dx_prop _
    | Text _
    | Verbatim _
    | Hash_ident _
    | Xml_ident _
    | Call _
    | Import _
    | Decl_xmlns _
    | Dx_var _
    | Comment _
    | Error _ ->
      None

let extract_addr (node : Code.node Range.located) =
  match node.value with
  | Group (Braces, [{value = Text addr; _}])
  | Group (Parens, [{value = Text addr; _}])
  | Group (Squares, [{value = Group (Squares, [{value = Text addr; _}]); _}])
  | Text addr
  | Import (_, addr) ->
    Some (Range.{value = addr; loc = node.loc})
  | Subtree (addr, _) ->
    Option.map (fun s -> Range.{value = s; loc = node.loc}) addr
  | _ -> None

let rec analyse (node : Code.node Range.located) =
  begin
    let@ {value; loc} = Option.iter @~ extract_addr node in
    S.yield ({value = Item.addr value; loc});
  end;
  begin
    let@ paths, loc = Option.iter @~ paths node in
    let@ path = List.iter @~ paths in
    S.yield ({value = Item.path path; loc});
  end;
  let children = Code.children node in
  List.iter analyse children

let analyse_syntax nodes =
  let@ () = S.run in
  List.iter analyse nodes

let contains = fun
    ~(position : Lsp.Types.Position.t)
    (loc : Range.t option)
  ->
  let L.Position.{line = cursor_line; character = cursor_character} = position in
  match loc with
  | Some loc ->
    begin
      match Range.view loc with
      | `Range (start, end_) ->
        let start_pos = Lsp_shims.Loc.lsp_pos_of_pos start in
        let end_pos = Lsp_shims.Loc.lsp_pos_of_pos end_ in
        let at_or_after_start =
          cursor_line < end_pos.line
          || (cursor_line = start_pos.line && start_pos.character <= cursor_character)
        in
        let before_or_at_end =
          end_pos.line > cursor_line
          || (cursor_line = end_pos.line && cursor_character <= end_pos.character)
        in
        at_or_after_start && before_or_at_end
      | _ -> false
    end
  | None -> false

let rec node_at
  : type a. position: L.Position.t -> children: (a Range.located -> a Range.located list) -> a Range.located list -> a Range.located option
= fun ~position ~children code ->
  match List.find_opt (fun Range.{loc; _} -> contains ~position loc) code with
  | None -> None
  | Some n ->
    match (node_at ~position ~children) (children n) with
    | Some inner -> Some inner
    | None -> Some n

let get_enclosing_code_group ~position nodes =
  let rec go ~position nodes =
    match List.find_opt (fun Range.{loc; _} -> contains ~position loc) nodes with
    | None -> None
    | Some n ->
      match n.value with
      | (Code.Group (delim, t)) ->
        begin
          match go ~position t with
          | None -> Some (delim, t)
          | Some t -> Some t
        end
      | _ ->
        (go ~position) (Code.children n)
  in
  go ~position nodes

let get_enclosing_syn_group ~position nodes =
  let rec go ~position nodes =
    match List.find_opt (fun Range.{loc; _} -> contains ~position loc) nodes with
    | None -> None
    | Some n ->
      match n.value with
      | (Syn.Group (delim, children)) ->
        begin
          match go ~position children with
          | None -> Some Asai.Range.{value = (delim, children); loc = n.loc}
          | Some t -> Some t
        end
      | _ ->
        go ~position (Syn.children n)
  in
  go ~position nodes

let find_with_prev ~position =
  let rec go prev = function
    | [] -> None
    | x :: xs -> if contains ~position Asai.Range.(x.loc) then Some (prev, x) else go (Some x) xs
  in
  go None

let parent_or_prev_at
  : type a. position: L.Position.t ->
  children: (a Range.located -> a Range.located list) ->
  a Range.located list ->
  [
    | `Prev of (a Range.located * a Range.located)
    | `Parent of a Range.located
    | `Node of a Range.located
  ] option
= fun ~position ~children code ->
  let go ~position ~children nodes =
    match find_with_prev ~position nodes with
    | None -> None
    | Some (None, node) ->
      begin
        match (node_at ~position ~children) (children node) with
        | Some inner ->
          (* go ~position ~children (children inner) *)
          Some (`Node inner)
        | None -> Some (`Node node)
      end
    | Some (Some prev, node) ->
      match (node_at ~position ~children) (children node) with
      | None -> Some (`Prev (prev, node))
      | Some inner ->
        Some (`Node inner)
  in
  go ~position ~children code

let parent_or_prev_at_code ~position = parent_or_prev_at ~position ~children: Code.children
let parent_or_prev_at_syn ~position = parent_or_prev_at ~position ~children: Syn.children

let node_at_code ~position = node_at ~position ~children: Code.children
let node_at_syn ~position = node_at ~position ~children: Syn.children

let get_visible ~position:_ _code =
  assert false
  (* let@ () = Sc.easy_run in
  Expand.Builtins.register_builtins Expand.builtins;
  let rec go ~(position : L.Position.t) (code : Code.t) =
    match code with
    | [] ->
      Sc.get_visible ()
    | ({loc; _} as node) :: rest ->
      let@ () = Expand.scope_effect node in
      if contains ~position loc then
        let children = Code.children node in
        match node_at_code ~position children with
        | None ->
          (* This case means "parent node is at position, but no child node is. I don't think this can happen"*)
          Resolver.Scope.get_visible ()
        | Some node' ->
          let@ () = Expand.scope_effect node' in
          Resolver.Scope.get_visible ()
      else
          go ~position rest
  in
  go ~position code *)

let addr_at ~(position : Lsp.Types.Position.t) (code : _ list) : _ Range.located option =
  Option.bind (node_at ~position ~children: Code.children code) extract_addr

exception Found of string

let word_at ~position (doc : Lsp.Text_document.t) =
  let L.Position.{line; character;} = position in
  let line = List.nth_opt (String.split_on_char '\n' (Lsp.Text_document.text doc)) line in
  match line with
  | None -> None
  | Some line ->
    let words = String.split_on_char ' ' line in
    try
      let acc = ref 0 in
      List.iter
        (fun word ->
          Logs.debug (fun m -> m "%s" word);
          let length = String.length word in
          if !acc + length + 1 > character then raise (Found word)
          else acc := !acc + length + 1
        )
        words;
      None
    with
      | Found str -> Some str
