(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude

let scheme = "forest"

let base_iri ~host =
  URI.make ~scheme ~host ()

let user_iri ~host str =
  URI.make
    ~host
    ~scheme
    ~path: [str]
    ()

let hash_iri ~host hash_str =
  URI.make
    ~host
    ~scheme
    ~path: ["hash"; hash_str]
    ()

let is_named_iri iri =
  match URI.scheme iri, URI.path_components iri with
  | sch, "hash" :: _ when sch = scheme -> false
  | _ -> true

let last_segment str =
  str
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
(* |> Filename.chop_extension *)

let name (iri : URI.t) : string =
  iri
  |> URI.path_string
  |> last_segment (* this is dodgy!*)

let split_addr (iri : URI.t) : (string option * int) option =
  let name = last_segment @@ URI.path_string iri in
  (* primitively check for address of form YYYY-MM-DD *)
  let date_regex = Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
  if Str.string_match date_regex name 0 then
    None
  else
    match String.rindex_opt name '-' with
    | Some i ->
      let prefix = String.sub name 0 i
      and suffix = String.sub name (i + 1) (String.length name - i - 1)
      in
      begin
        match BaseN.Base36.int_of_string suffix with
        | Some key -> Some (Some prefix, key)
        | None -> None
      end
    | _ ->
      let@ key = Option.map @~ BaseN.Base36.int_of_string name in
      None, key

let lsp_uri_to_iri
  : host: string -> Lsp.Uri.t -> URI.t
= fun ~host uri ->
  let iri =
    uri
    |> Lsp.Uri.to_path
    |> Filename.chop_extension
    |> last_segment
    |> user_iri ~host
  in
  assert ((Filename.extension @@ URI.path_string iri) = "");
  iri

let path_to_iri ~host str =
  str
  |> last_segment
  |> Filename.chop_extension
  |> user_iri ~host

let source_path_to_addr p = Filename.(chop_extension @@ basename p)
