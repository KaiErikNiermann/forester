(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

(** Forester uses an IRI scheme that incorporates a host that demarcates the current forest. A typical tree has have an address like this: [forest://host/xxx-NNNN]. Resources such as PDFs or images will be referred to by their hash.*)

val scheme : string

val base_iri :
  host: string ->
  URI.t

val user_iri :
  host: string ->
  string ->
  URI.t

val hash_iri :
  host: string ->
  string ->
  URI.t

val is_named_iri : URI.t -> bool

val lsp_uri_to_iri :
  host: string ->
  Lsp.Uri.t ->
  URI.t

val split_addr :
  URI.t ->
  (string option * int) option

val path_to_iri :
  host: string ->
  string ->
  URI.t

val last_segment : string -> string
val name : URI.t -> string
