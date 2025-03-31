(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

let parse lexbuf filename =
  match Toml.Parser.parse lexbuf filename with
  | `Error (desc, {source; _}) ->
    let@ () = Reporter.tracef "when parsing configuration file" in
    let loc = Asai.Range.of_lexbuf ~source: (`File source) lexbuf in
    Reporter.fatal ~loc Configuration_error ~extra_remarks: [Asai.Diagnostic.loctextf "%s" desc]
  | `Ok tbl ->
    let open Toml.Lenses in
    let forest = key "forest" |-- table in
    let renderer = key "renderer" |-- table in
    let url =
      match get tbl (forest |-- key "url" |-- string) with
      | Some url ->
        begin
          try
            URI.of_string_exn url
          with
            | _ -> Reporter.fatal Configuration_error ~extra_remarks: [Asai.Diagnostic.loctext "Invalid URL specified in `url` key."]
        end
      | None -> Reporter.fatal Configuration_error ~extra_remarks: [Asai.Diagnostic.loctext "You need to set the `url' key in your configuration file; this should be a URL like `https://www.my-great-forest.org/` or `http://localhost/`. Even if you do not plan to publish your forest, please choose a URL."]
    in
    let trees =
      Option.value ~default: Config.default.trees @@
        get tbl (forest |-- key "trees" |-- array |-- strings)
    in
    let foreign =
      Option.value ~default: Config.default.foreign @@
        get tbl (forest |-- key "foreign" |-- array |-- strings)
    in
    let assets =
      Option.value ~default: Config.default.assets @@
        get tbl (forest |-- key "assets" |-- array |-- strings)
    in
    let theme =
      Option.value ~default: Config.default.theme @@
        get tbl (renderer |-- key "theme" |-- string)
    in
    let home =
      Option.map (URI_scheme.named_uri ~base: url) @@
        get tbl (renderer |-- key "home" |-- string)
    in
    let prefixes =
      Option.value ~default: Config.default.prefixes @@
        get tbl (forest |-- key "prefixes" |-- array |-- strings)
    in
    Config.{url; assets; trees; foreign; theme; home; prefixes}

let parse_forest_config_string str =
  let lexbuf = Lexing.from_string str in
  parse lexbuf "<anonymous>"

let parse_forest_config_file filename =
  try
    let ch = open_in filename in
    let@ () = Fun.protect ~finally: (fun _ -> close_in ch) in
    let lexbuf = Lexing.from_channel ch in
    let result = parse lexbuf filename in
    Sys.chdir @@ Filename.dirname filename;
    result
  with
    | exn -> Reporter.fatal Configuration_error ~extra_remarks: [Asai.Diagnostic.loctextf "%a" Eio.Exn.pp exn]
