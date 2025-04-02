(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_compiler
open Forester_core

open struct
  module R = Resolver
  module Sc = R.Scope
end

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

let code_children (node : Code.node Range.located) =
  match node.value with
  | Math (_, t)
  | Group (_, t)
  | Let (_, _, t)
  | Scope t
  | Put (_, t)
  | Fun (_, t)
  | Default (_, t)
  | Def (_, _, t)
  | Namespace (_, t)
  | Dx_const_uri t
  | Dx_const_content t
  | Call (t, _)
  | Subtree (_, t) ->
    t
  | Dx_prop (_, t)
  | Dx_query (_, _, t)
  | Dx_sequent (_, t) ->
    (List.concat t)
  | Object {methods; _} ->
    (methods |> List.map snd |> List.concat)
  | Patch {obj; methods; _} ->
    let methods = (methods |> List.map snd |> List.concat) in
    (List.append obj methods)
  | Text _
  | Verbatim _
  | Ident _
  | Hash_ident _
  | Xml_ident (_, _)
  | Open _
  | Get _
  | Import (_, _)
  | Decl_xmlns (_, _)
  | Alloc _
  | Dx_var _
  | Comment _
  | Error _ ->
    []

let syn_children (node : Syn.node Range.located) =
  match node.value with
  | Group (_, t) -> t
  | Math (_, t) -> t
  | Subtree (_, t) -> t
  | Link {dest; title} -> Option.fold ~some: (fun t -> t @ dest) ~none: dest title
  | Fun (_, t) -> t
  | Put (r, s, t) -> r @ s @ t
  | Default (r, s, t) -> r @ s @ t
  | Get t -> t
  | Xml_tag (_, qs, t) -> List.concat_map snd qs @ t
  | Call (t, _) -> t
  | Object {methods; _} ->
    List.concat_map snd methods
  | Patch {obj; methods; _} ->
    List.concat_map snd methods @
      obj
  | Dx_sequent (t, ts) -> t @ List.concat ts
  | Dx_query (_, ps, ns) -> List.concat ps @ List.concat ns
  | Dx_const (_, n) -> n
  | Dx_prop (t, ts) -> t @ List.concat ts
  | Text _
  | Verbatim _
  | Var _
  | Sym _
  | TeX_cs _
  | Prim _
  | Results_of_query
  | Transclude
  | Embed_tex
  | Ref
  | Title
  | Parent
  | Taxon
  | Meta
  | Attribution (_, _)
  | Tag _
  | Date
  | Number
  | Dx_var _
  | Dx_execute
  | Route_asset
  | Syndicate_current_tree_as_atom_feed
  | Syndicate_query_as_json_blob
  | Current_tree ->
    []

let flatten (tree : Code.t) : Code.t =
  List.concat_map code_children tree

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
  let children = code_children node in
  List.iter analyse children

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

let code_node_at ~position = node_at ~position ~children: code_children
let syn_node_at ~position = node_at ~position ~children: syn_children

let get_visible
= fun ~position code ->
  let@ () = Sc.easy_run in
  Expand.Builtins.register_builtins Expand.builtins;
  let rec go ~(position : L.Position.t) (code : Code.t) =
    match code with
    | [] ->
      Sc.get_visible ()
    | ({loc; _} as node) :: rest ->
      let@ () = Expand.scope_effect node in
      if contains ~position loc then
        match (node_at ~position ~children: code_children) (code_children node) with
        | None ->
          Sc.get_visible ()
        | Some node' ->
          let@ () = Expand.scope_effect node' in
          Resolver.Scope.get_visible ()
      else
        begin
          go ~position rest
        end
  in
  go ~position code

let addr_at ~(position : Lsp.Types.Position.t) (code : _ list) : _ Range.located option =
  Option.bind (node_at ~position ~children: code_children code) extract_addr

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
