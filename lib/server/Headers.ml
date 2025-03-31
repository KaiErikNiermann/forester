(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Types

let parse_flag field header =
  match Http.Header.get header field with
  | Some "true" -> Some true
  | Some "false" -> Some false
  | Some _ -> None
  | None -> None

let parse_title_flags
    (header : Http.Header.t)
    : T.title_flags option
  =
  match parse_flag "Empty-When-Untitled" header with
  | Some b -> Some T.{empty_when_untitled = b;}
  | None -> None

let parse_section_flags (header : Http.Header.t) : T.section_flags option =
  let hidden_when_empty = parse_flag "Hidden-When-Empty" header in
  let included_in_toc = parse_flag "Included-In-Toc" header in
  let header_shown = parse_flag "Header-Shown" header in
  let metadata_shown = parse_flag "Metadata-Shown" header in
  let numbered = parse_flag "Numbered" header in
  let expanded = parse_flag "Expanded" header in
  Some
    {
      hidden_when_empty;
      included_in_toc;
      header_shown;
      metadata_shown;
      numbered;
      expanded
    }

let parse_content_target (header : Http.Header.t) : T.content_target option =
  let open Http in
  match Header.get header "Taxon" with
  | Some _ -> Some T.Taxon
  | None ->
    match Header.get header "Mainmatter" with
    | Some _ -> Some T.Mainmatter
    | None ->
      match Header.get header "Full" with
      | Some _ ->
        begin
          match parse_section_flags header with
          | Some flags -> Some (T.Full flags)
          | None -> None
        end
      | None ->
        match Header.get header "Title" with
        | None -> None
        | Some _ ->
          match parse_title_flags header with
          | Some flags -> Some (T.Title flags)
          | None -> None
