(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {
  path: string;
  route_locally: bool
}
[@@deriving show]

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
  prefixes: string list; (* TODO: remove this as we no longer advocate using prefixes at all *)
}
[@@deriving show]

val default_url : URI.t
val default : ?url: URI.t -> unit -> t
val home_uri : t -> URI.t
