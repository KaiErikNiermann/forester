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
  home: URI.t;
  prefixes: string list;
}
[@@deriving show, repr]

let default ?(url = URI.of_string_exn "http://localhost/") () : t = {
  trees = ["trees"];
  assets = [];
  foreign = [];
  theme = "theme";
  url;
  home = URI_scheme.named_uri ~base: url "index";
  prefixes = [];
}

(* TODO: validate beforehand *)
let base_uri {url; _} = url

let home_uri config = config.home
