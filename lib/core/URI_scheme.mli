(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

val named_uri :
  base: URI.t ->
  string ->
  URI.t

val lsp_uri_to_uri :
  base: URI.t ->
  Lsp.Uri.t ->
  URI.t

val split_addr :
  URI.t ->
  (string option * int) option

val path_to_uri :
  base: URI.t ->
  string ->
  URI.t

val last_segment : string -> string
val name : URI.t -> string
