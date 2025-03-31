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
  prefixes: string list;
}
[@@deriving show, repr]

let default : t = {
  trees = ["trees"];
  assets = [];
  foreign = [];
  theme = "theme";
  url = URI.of_string_exn "http://localhost/";
  home = None;
  prefixes = [];
}

(* TODO: validate beforehand *)
let base_uri {url; _} = url

let home_uri config =
  match config.home with
  | Some uri -> uri
  | None -> URI_scheme.named_uri ~base: config.url "index"
