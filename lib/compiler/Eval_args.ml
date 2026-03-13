(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module T = Types

type located = Value.t Range.located
type resolved_vertex = [`Content | `Uri]

let extract_content (node : located) =
  match node.value with
  | Value.Content content -> content
  | v ->
    Reporter.fatal
      ?loc: node.loc
      (Type_error {expected = [Content]; got = Some v})

let extract_text_loc (node : located) : string Range.located =
  let content = extract_content node in
  let rec loop acc = function
    | [] -> Option.some @@ String.concat "" @@ Bwd.prepend acc []
    | (T.Text txt | T.CDATA txt) :: content -> loop (Bwd.snoc acc txt) content
    | T.Uri uri :: content -> loop (Bwd.snoc acc (URI.to_string uri)) content
    | _ -> None
  in
  match loop Emp (T.extract_content content) with
  | Some txt -> {value = String.trim txt; loc = node.loc}
  | None ->
    Reporter.fatal
      ?loc: node.loc
      (Type_error {expected = [Text]; got = None})

let extract_text (node : located) : string = (extract_text_loc node).value

let extract_obj_ptr (x : located) =
  match x.value with
  | Obj sym -> sym
  | other ->
    Reporter.fatal
      ?loc: x.loc
      (Type_error {expected = [Obj]; got = Some other})

let extract_sym (x : located) =
  match x.value with
  | Sym sym -> sym
  | other ->
    Reporter.fatal
      ?loc: x.loc
      (Type_error {expected = [Sym]; got = Some other})

let extract_bool (x : located) =
  match x.value with
  | Content (T.Content [Text "true"]) -> true
  | Content (T.Content [Text "false"]) -> false
  | other ->
    Reporter.fatal
      ?loc: x.loc
      (Type_error {expected = [Bool]; got = Some other})

let default_backmatter ~(uri : URI.t) : T.content =
  let vtx = T.Uri_vertex uri in
  let make_section title query =
    let section =
      let frontmatter =
        T.default_frontmatter ~title: (T.Content [T.Text title]) ()
      in
      let mainmatter = T.Content [T.Results_of_datalog_query query] in
      let flags =
        {T.default_section_flags with hidden_when_empty = Some true}
      in
      T.{frontmatter; mainmatter; flags}
    in
    T.Section section
  in
  T.Content
    [
      make_section "References" @@ Builtin_queries.references_datalog vtx;
      make_section "Context" @@ Builtin_queries.context_datalog vtx;
      make_section "Backlinks" @@ Builtin_queries.backlinks_datalog vtx;
      make_section "Related" @@ Builtin_queries.related_datalog vtx;
      make_section "Contributions" @@ Builtin_queries.contributions_datalog vtx;
    ]

let resolve_uri ~(base : URI.t) str =
  match URI.of_string_exn str with
  | uri ->
    (
      match (URI.scheme uri, URI.host uri, URI.path_components uri) with
      | None, None, ([] | [_]) ->
        let uri = URI_scheme.named_uri ~base str in
        Result.ok uri
      | _ -> Ok uri
    )
  | exception _ -> Error "Invalid URI"

let extract_uri ~(base : URI.t) (node : located) =
  let text = extract_text node in
  resolve_uri ~base text

let extract_vertex ~(base : URI.t) ~type_ (node : located) =
  match type_ with
  | `Content -> Ok (T.Content_vertex (extract_content node))
  | `Uri ->
    let@ uri = Result.map @~ extract_uri ~base node in
    T.Uri_vertex uri

let pp_tex_cs fmt = function
  | TeX_cs.Symbol x -> Format.fprintf fmt "\\%c" x
  | TeX_cs.Word x -> Format.fprintf fmt "\\%s " x
