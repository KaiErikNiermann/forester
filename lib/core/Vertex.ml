(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Types

type t = content vertex
[@@deriving show]

let clean = function
  | Content_vertex x -> Content_vertex x
  | Iri_vertex uri -> Iri_vertex uri

let hash x = Hashtbl.hash (clean x)

let compare x y = compare (clean x) (clean y)
let equal x y = clean x = clean y

let uri_of_vertex : _ Types.vertex -> URI.t option = function
  | Iri_vertex uri -> Some uri
  | Content_vertex _ -> None
