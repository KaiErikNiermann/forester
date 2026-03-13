(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open struct module T = Forester_core.Types end

module Set = Set.Make(String)

type loc =
  | In_frontmatter
  | In_mainmatter

let int_of_field_frontmatter
= function
  | `uri -> 0
  | `title -> 1
  | `dates -> 2
  | `attributions -> 3
  | `taxon -> 4
  | `number -> 5
  | `designated_parent -> 6
  | `source_path -> 7
  | `tags -> 8
  | `metas -> 9

let int_of_field_article
= function
  | `frontmatter -> 0
  | `mainmatter -> 1
  | `backmatter -> 2

type token = {v: string; loc: loc}

let common_words =
  Set.of_list
    [
      "a";
      "and";
      "be";
      "have";
      "i";
      "in";
      "of";
      "that";
      "the";
      "to";
    ]

let tokenize string =
  Str.(split @@ regexp "[^a-zA-Z0-9]+") string
  |> List.filter_map
      (fun s ->
        let lower = String.lowercase_ascii s in
        if not @@ Set.mem lower common_words then
          Some (Stemming.stem lower)
        else
          None
      )

let rec tokenize_content
  : int list -> loc -> T.content -> (int list * string) list
= fun path loc node ->
  match node with
  | T.Content nodes ->
    List.concat @@
      List.mapi
        (fun i node ->
          match node with
          | T.Text s ->
            List.mapi
              (fun j token -> j :: i :: path, token)
              (tokenize s)
          (* i :: path, token *)
          | T.CDATA s ->
            List.mapi
              (fun j token -> j :: i :: path, token)
              (tokenize s)
          | T.Xml_elt {content; _} ->
            (* TODO: Consider tokenizing xml_qname *)
            tokenize_content
              (i :: path)
              loc
              content
          | T.Section {frontmatter; mainmatter; _} ->
            tokenize_frontmatter
              (int_of_field_article `frontmatter :: path)
              frontmatter @
              tokenize_content path loc mainmatter
          | T.Link {content; _} -> tokenize_content (i :: path) loc content
          | T.KaTeX (_, _) ->
            (* NOTE:
               In order to properly search math, we need to revamp the
               architecture and add more features...*)
            []
          | T.Transclude _
          | T.Contextual_number _
          | T.Artefact _
          | T.Uri _
          | T.Route_of_uri _
          | T.Datalog_script _
          | T.Results_of_datalog_query _ ->
            []
        )
        nodes

and tokenize_vertex
  : int list ->
  loc ->
  T.content T.vertex ->
  (int list * string) list
= fun path loc v ->
  match v with
  | T.Uri_vertex _ -> []
  | T.Content_vertex c ->
    tokenize_content path loc c

and tokenize_attribution
  : int list ->
  loc ->
  T.content T.attribution ->
  (int list * string) list
= fun path loc v ->
  match v with
  | T.{vertex; _} ->
    tokenize_vertex path loc vertex

and tokenize_frontmatter
  : int list ->
  T.content T.frontmatter ->
  (int list * string) list
= fun path fm ->
  match fm with
  | {
    title;
    attributions;
    taxon;
    tags;
    metas;
    _;
  } ->
    List.concat
      [
        Option.value
          ~default: []
          (
            Option.map
              (
                tokenize_content
                  (int_of_field_frontmatter `title :: path)
                  In_frontmatter
              )
              title
          );
        Option.value
          ~default: []
          (
            Option.map
              (
                tokenize_content
                  (int_of_field_frontmatter `taxon :: path)
                  In_frontmatter
              )
              taxon
          );
        List.concat_map (tokenize_attribution path In_frontmatter) attributions;
        List.concat_map (tokenize_vertex path In_frontmatter) tags;
        List.concat @@
          List.mapi
            (fun _i (s, c) ->
              (List.mapi (fun i t -> i :: path, t) @@ tokenize s) @
                tokenize_content path In_frontmatter c
            )
            metas;
      ]

let tokenize_article : T.content T.article -> (int list * string) list = function
  | {frontmatter; mainmatter; _} ->
    tokenize_frontmatter [0] frontmatter @
      tokenize_content [1] In_mainmatter mainmatter
    |> List.map (fun (x, y) -> List.rev x, y)
