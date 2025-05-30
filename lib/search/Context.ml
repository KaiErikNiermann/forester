(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open struct module T = Types end

(* The idea is to render a search result with surrounding context, say 5 words
   on each side. *)

type path = int list

let show_leaf_node
  : T.content T.content_node -> string
= fun node ->
  match node with
  | T.Text s -> s
  | T.CDATA s -> s
  | T.Uri i
  | T.Route_of_uri i ->
    Format.asprintf "%a" URI.pp i
  | T.Xml_elt _
  | T.Transclude _
  | T.Contextual_number _
  | T.Section _
  | T.KaTeX (_, _)
  | T.Link _
  | T.Artefact _
  | T.Datalog_script _
  | T.Results_of_datalog_query _ ->
    raise @@
      Invalid_argument (Format.asprintf "%a is not a leaf node" T.(pp_content_node pp_content) node)

let get_nth_word i string =
  Str.(split @@ regexp "[^a-zA-Z0-9]+") string
  |> List.filter_map
      (fun s ->
        let lower = String.lowercase_ascii s in
        if not @@ Tokenizer.(Set.mem lower common_words) then
          Some lower
        else
          None
      )
  |> (fun l -> List.nth l i)

let render_context_list
  : (path -> 'a -> string) -> path -> 'a list -> string
= fun f path l ->
  match path with
  | [] -> String.concat "" @@ List.map (f []) l
  | i :: path' ->
    let n = List.nth l i in
    f path' n

let rec render_context_frontmatter
  : path -> T.content T.frontmatter -> string
= fun path frontmatter ->
  match path with
  | [] -> raise (Invalid_argument "stopped on non-leaf node")
  | 0 :: _path ->
    Format.(asprintf "%a" (pp_print_option URI.pp) frontmatter.uri)
  | 1 :: path' ->
    begin
      match path' with
      | [] ->
        Option.value ~default: "" @@
          Option.map T.show_content frontmatter.title
      | path ->
        Option.value ~default: "" @@
          Option.map (render_context_content path) frontmatter.title
    end
  | 2 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.dates*)
  | 3 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.attributions*)
  | 4 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | path ->
        Option.value ~default: "" @@
          Option.map (render_context_content path) frontmatter.taxon
    end (*frontmatter.taxon*)
  | 5 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.number*)
  | 6 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.designated_parent*)
  | 7 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.source_path*)
  | 8 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.tags*)
  | 9 :: path' ->
    begin
      match path' with
      | [] -> assert false
      | _path -> assert false
    end (*frontmatter.metas*)
  | _ -> raise (Invalid_argument "out of bound index")

and render_context_node
  : path -> T.content T.content_node -> string
= fun path node ->
  match path with
  | [] -> show_leaf_node node
  | i :: path' ->
    match node with
    | T.Text s -> get_nth_word i s
    | T.CDATA _ -> raise @@ Invalid_argument "can't descend into CDATA node"
    | T.Xml_elt elt -> render_context_content path elt.content
    | T.Transclude _ -> assert false
    | T.Contextual_number _ -> assert false
    | T.Section _ -> assert false
    | T.KaTeX (_, _) -> assert false
    | T.Link link -> render_context_content path' link.content
    | T.Artefact _ -> assert false
    | T.Uri _ -> assert false
    | T.Route_of_uri _ -> assert false
    | T.Datalog_script _ -> assert false
    | T.Results_of_datalog_query _ -> assert false

and render_context_content
  : path -> T.content -> string
= fun path content ->
  let T.Content c = content in
  match path with
  | [] -> T.show_content content
  | i :: path' ->
    let node = List.nth c i in
    (* render_context_node in *)
    render_context_node path' node

and render_context_article
  : path -> T.content T.article -> string
= fun path article ->
  match path with
  | [] -> ""
  | 0 :: path' -> render_context_frontmatter path' article.frontmatter
  | 1 :: path' -> render_context_content path' article.mainmatter
  | _ -> raise (Invalid_argument "out of bound index")

and debug_context_article : path -> string = function
  | [] -> "empty path"
  | 0 :: _path' -> "frontmatter ->"
  | 1 :: _path' -> "mainmatter ->"
  | _ -> raise (Invalid_argument "out of bound index")
