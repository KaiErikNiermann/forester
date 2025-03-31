(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t = {
  trees: string list;
  assets: string list;
  foreign: string list;
  theme: string;
  url: URI.t;
  home: URI.t option;
  prefixes: string list; (* TODO: remove this as we no longer advocate using prefixes at all *)
}
[@@deriving show]

val default : t

val home_uri : t -> URI.t
