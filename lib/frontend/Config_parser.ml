(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

(* type keys = (Toml.Types.Table.key [@printer fun fmt key -> fprintf fmt "%s" (Toml.Types.Table.Key.to_string key)]) list list [@@deriving show] *)

module Key_set = struct
  include Set.Make(struct
    type t = Toml.Types.Table.key list
    let compare = compare
  end)

  let remove : string list -> t -> t = fun strs set ->
    let key = List.map Toml.Types.Table.Key.of_string strs in
    remove key set
end

let keys (tbl : Toml.Types.value Toml.Types.Table.t) =
  let rec go current keys tbl =
    List.fold_left
      (fun acc (key, value) ->
        match value with
        | Toml.Types.TBool _
        | TInt _
        | TFloat _
        | TString _
        | TDate _
        | TArray _ ->
          (key :: current) :: acc
        | TTable tbl ->
          go (key :: current) acc tbl
      )
      keys
      (Toml.Types.Table.to_list tbl)
  in
  Key_set.of_list @@ List.map List.rev @@ go [] [] tbl

let parse lexbuf filename =
  let@ () = Reporter.easy_run in
  match Toml.Parser.parse lexbuf filename with
  | `Error (desc, {source; _}) ->
    let@ () = Reporter.tracef "when parsing configuration file" in
    let loc = Asai.Range.of_lexbuf ~source: (`File source) lexbuf in
    Reporter.fatal ~loc Configuration_error ~extra_remarks: [Asai.Diagnostic.loctextf "%s" desc]
  | `Ok tbl ->
    let open Toml.Lenses in
    let keys = ref (keys tbl) in
    let with_default ~value k lens =
      let open Toml.Lenses in
      match get tbl lens with
      | None ->
        Reporter.emit (Using_default_option k);
        value
      | Some v ->
        keys := Key_set.remove k !keys;
        v
    in
    let forest = key "forest" |-- table in
    let url =
      let k = ["forest"; "url"] in
      match get tbl (forest |-- key "url" |-- string) with
      | Some url ->
        keys := Key_set.remove k !keys;
        begin
          try
            URI.of_string_exn url
          with
            | _ -> Reporter.fatal Configuration_error ~extra_remarks: [Asai.Diagnostic.loctext "Invalid URL specified in `url` key."]
        end
      | None ->
        Reporter.emit (Using_default_option k);
        Config.default_url
    in
    let default = Config.default ~url () in
    let trees =
      let k = ["forest"; "trees"] in
      with_default ~value: default.trees k (forest |-- key "trees" |-- array |-- strings)
    in
    let foreign =
      let k = ["forest"; "foreign"] in
      match get tbl (forest |-- key "foreign" |-- array |-- tables) with
      | None -> default.foreign
      | Some foreign_tbls ->
        keys := Key_set.remove k !keys;
        let@ foreign_tbl = List.map @~ foreign_tbls in
        let path =
          match get foreign_tbl (key "path" |-- string) with
          | None -> Reporter.fatal (Required_config_option "path")
          | Some path -> path
        in
        let route_locally =
          match get foreign_tbl @@ (key "route_locally" |-- bool) with
          | None -> true
          | Some b -> b
        in
        Config.{path; route_locally}
    in
    let assets =
      with_default
        ~value: default.assets
        ["forest"; "assets"]
        (forest |-- key "assets" |-- array |-- strings)
    in
    let home =
      let k = ["forest"; "home"] in
      URI_scheme.named_uri ~base: url @@
        with_default ~value: "index" k (forest |-- key "home" |-- string)
    in
    let prefixes =
      with_default
        ~value: default.prefixes
        ["forest"; "prefixes"]
        (forest |-- key "prefixes" |-- array |-- strings)
    in
    begin
      if not (Key_set.is_empty !keys) then
        let keys =
          !keys
          |> Key_set.to_list
          |> List.map (List.map (Toml.Types.Table.Key.to_string))
        in
        Reporter.emit (Uninterpreted_config_options keys);
    end;
    Config.{url; assets; trees; foreign; home; prefixes}

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
