(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_frontend

module L = Lsp.Types
(* module F = Analysis.F *)
(* module PT = Analysis.PT *)
module T = Types

let compute
    ({position;
      textDocument;
      _
    }:
      L.HoverParams.t
    )
    : L.Hover.t option
  =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render =
    Plain_text_client.string_of_content
      ~forest
      ~router: (Legacy_xml_client.route forest)
  in
  let config = forest.config in
  let host = config.host in
  let content =
    match State.get_code
      forest
      (URI_scheme.lsp_uri_to_uri ~host: forest.config.host textDocument.uri) with
    | None -> "code of current tree is not stored. this is a bug"
    | Some tree ->
      (* TODO: use node_at and provide hover for things other than links.*)
      match Analysis.addr_at ~position tree.nodes with
      | None -> Format.asprintf "character: %i, line: %i." position.character position.line;
      | Some Range.{value = addr_at_cursor; _} ->
        let uri_under_cursor = URI_scheme.user_uri ~host addr_at_cursor in
        match State.get_article uri_under_cursor forest with
        | None ->
          Format.asprintf "Could not get article %a." URI.pp uri_under_cursor
        | Some {mainmatter; frontmatter; _} ->
          let main = render mainmatter in
          if main = "" then (Format.asprintf "%a" T.(pp_frontmatter pp_content) frontmatter)
          else main
  in
  Some
    (
      L.Hover.create
        ~contents: (
          `MarkupContent
            {
              kind = L.MarkupKind.Markdown;
              value = content
            }
        )
        ()
    )
