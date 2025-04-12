(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module T = Types

let parse_flag field header =
  match Http.Header.get header field with
  | Some "true" -> Some true
  | Some "false" -> Some false
  | Some _ -> None
  | None -> None

let parse_title_flags (header : Http.Header.t) : T.title_flags option =
  let@ b = Option.map @~ parse_flag "Empty-When-Untitled" header in
  T.{empty_when_untitled = b}

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
        let@ flags = Option.map @~ parse_section_flags header in
        T.Full flags
      | None ->
        let@ _ = Option.bind @@ Header.get header "Title" in
        let@ flags = Option.map @~ parse_title_flags header in
        T.Title flags
