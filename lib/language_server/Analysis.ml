(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module L = Lsp.Types

module Item = struct
  type t = [
    | `Path of Trie.path
    | `Addr of string
  ]
  let addr str = `Addr str
  let path p = `Path p
end

module S = Algaeff.Sequencer.Make(struct
  type t = Item.t Range.located
end)

let nodes_within (node : Code.node Range.located) =
  match node.value with
  | Code.Math (_, t)
  | Code.Group (_, t)
  | Code.Let (_, _, t)
  | Code.Scope t
  | Code.Put (_, t)
  | Code.Fun (_, t)
  | Code.Default (_, t)
  | Code.Def (_, _, t)
  | Code.Namespace (_, t)
  | Code.Dx_const_uri t
  | Code.Dx_const_content t
  | Code.Call (t, _)
  | Code.Subtree (_, t) ->
    t
  | Code.Dx_prop (_, t)
  | Code.Dx_query (_, _, t)
  | Code.Dx_sequent (_, t) ->
    (List.concat t)
  | Code.Object {methods; _} ->
    (methods |> List.map snd |> List.concat)
  | Code.Patch {obj; methods; _} ->
    let methods = (methods |> List.map snd |> List.concat) in
    (List.append obj methods)
  | Code.Text _
  | Code.Verbatim _
  | Code.Ident _
  | Code.Hash_ident _
  | Code.Xml_ident (_, _)
  | Code.Open _
  | Code.Get _
  | Code.Import (_, _)
  | Code.Decl_xmlns (_, _)
  | Code.Alloc _
  | Code.Dx_var _
  | Code.Comment _
  | Code.Error _ ->
    []

let flatten (tree : Code.t) : Code.t =
  List.concat_map nodes_within tree

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
  let children = nodes_within node in
  List.iter analyse children

let contains = fun
    ~(position : Lsp.Types.Position.t)
    (located : _ Range.located)
  ->
  let L.Position.{line = cursor_line; character = cursor_character} = position in
  match located.loc with
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

let rec node_at ~(position : Lsp.Types.Position.t) (code : _ list) : Code.node Range.located option =
  match List.find_opt (contains ~position) code with
  | None -> None
  | Some n ->
    match (node_at ~position) (nodes_within n) with
    | Some inner -> Some inner
    | None -> Some n

let addr_at ~(position : Lsp.Types.Position.t) (code : _ list) : _ Range.located option =
  Option.bind (node_at ~position code) extract_addr

let analyse_syntax nodes =
  let@ () = S.run in
  List.iter analyse nodes

exception Found of string

let word_at ~position (doc : Lsp.Text_document.t) =
  let L.Position.{line; character;} = position in
  let line = List.nth_opt (String.split_on_char '\n' (Lsp.Text_document.text doc)) line in
  match line with
  | None -> None
  | Some line ->
    let words = String.split_on_char ' ' line in
    Logs.debug (fun m -> m "line has %d words" (List.length words));
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
