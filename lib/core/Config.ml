(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type foreign = {
  path: string;
  route_locally: bool
}
[@@deriving show, repr]

type t = {
  trees: string list;
  assets: string list;
  foreign: foreign list;
  url: URI.t;
  home: URI.t;
}
[@@deriving show, repr]

let default_url = URI.of_string_exn "http://forest.local/"

let default ?(url = default_url) () : t = {
  trees = ["trees"];
  assets = [];
  foreign = [];
  url;
  home = URI_scheme.named_uri ~base: url "index";
}

(* TODO: validate beforehand *)
let base_uri {url; _} = url

let home_uri config = config.home
