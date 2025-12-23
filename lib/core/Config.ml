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
[@@deriving show, repr]

type latex_settings = {
  document_class: string;
  document_class_options: string list;
  compile_command: string list;
  dvisvgm_command: string list;
}
[@@deriving show, repr]

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
  latex: latex_settings;
}
[@@deriving show, repr]

let default_url = URI.of_string_exn "http://forest.local/"

let default_latex : latex_settings = {
  document_class = "standalone";
  document_class_options = ["preview"; "border=2pt"; "10pt"];
  compile_command = ["latex"; "-halt-on-error"; "-interaction=nonstopmode"];
  dvisvgm_command = [
    "dvisvgm";
    "--exact";
    "--clipjoin";
    "--font-format=woff";
    "--bbox=papersize";
    "--zoom=1.5";
    "--stdin";
    "--stdout"
  ];
}

let default ?(url = default_url) ?(latex = default_latex) () : t = {
  trees = ["trees"];
  assets = [];
  foreign = [];
  url;
  home = URI_scheme.named_uri ~base: url "index";
  latex;
}

let home_uri config = config.home
