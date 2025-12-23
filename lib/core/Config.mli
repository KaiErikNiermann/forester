(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {
  path: string;
  route_locally: bool;
  include_in_manifest: bool
}
[@@deriving show]

type latex_settings = {
  document_class: string;
  document_class_options: string list;
  compile_command: string list;
  dvisvgm_command: string list;
}
[@@deriving show]

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
  latex: latex_settings;
}
[@@deriving show]

val default_url : URI.t
val default_latex : latex_settings
val default : ?url: URI.t -> ?latex: latex_settings -> unit -> t
val home_uri : t -> URI.t
