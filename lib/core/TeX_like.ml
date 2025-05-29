(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Types

let rec pp_content fmt = function
  | Content nodes ->
    List.iter (pp_content_node fmt) nodes

and pp_content_node fmt = function
  | Text str -> Format.fprintf fmt "%s" str
  | CDATA str -> Format.fprintf fmt "%s" str
  | KaTeX (_, xs) -> pp_content fmt xs
  | Xml_elt _ | Transclude _ | Contextual_number _ | Section _ | Link _ | Artefact _ | Uri _ | Route_of_uri _ | Datalog_script _ | Results_of_datalog_query _ ->
    Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "Cannot render this kind of content node as TeX-like string"]

let string_of_content =
  Format.asprintf "%a" pp_content
