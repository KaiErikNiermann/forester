(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Types

let rec pp_content ~forest ~router fmt = function
  | T.Content c -> c |> List.iter @@ pp_content_node ~forest ~router fmt

and pp_content_node ~forest ~(router : URI.t -> URI.t) fmt : 'a T.content_node -> unit = function
  | Text txt | CDATA txt -> Format.pp_print_string fmt txt
  | Uri uri -> URI.pp fmt uri
  | Route_of_uri uri -> Format.fprintf fmt "%a" URI.pp (router uri)
  | KaTeX (_, content) -> pp_content ~forest ~router fmt content
  | Xml_elt elt -> pp_content ~forest ~router fmt elt.content
  | Transclude trn -> pp_transclusion ~forest ~router fmt trn
  | Contextual_number addr -> Format.fprintf fmt "[%a]" URI.pp addr
  | Section section -> pp_section ~forest ~router fmt section
  | Link link -> pp_link ~forest ~router fmt link
  | Results_of_datalog_query _ | Artefact _ | Datalog_script _ -> ()

and pp_transclusion ~forest ~(router : URI.t -> URI.t) fmt (transclusion : T.transclusion) =
  match State.get_content_of_transclusion transclusion forest with
  | None -> Format.fprintf fmt "<could not resolve transclusion of %a>" URI.pp transclusion.href
  | Some content -> pp_content ~forest ~router fmt content

and pp_link ~forest ~(router : URI.t -> URI.t) fmt (link : T.content T.link) =
  pp_content ~forest ~router fmt link.content

and pp_section ~forest ~(router : URI.t -> URI.t) fmt (section : T.content T.section) =
  match section.frontmatter.title with
  | None -> Format.fprintf fmt "<omitted content>"
  | Some title -> Format.fprintf fmt "<omitted content: %a>" (pp_content ~forest ~router) title

let string_of_content ~forest ?(router : URI.t -> URI.t = Fun.id) =
  Format.asprintf "%a" (pp_content ~forest ~router)
