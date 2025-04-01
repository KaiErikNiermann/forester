(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_frontend
open Forester_search
open State.Syntax

module L = Lsp.Types
module T = Types

let (let*) = Option.bind

let compute ({position; textDocument; _}: L.HoverParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render =
    Plain_text_client.string_of_content
      ~forest
      ~router: Fun.id
  in
  let uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url textDocument.uri in
  let* content =
    match forest.={uri} with
    | None -> Reporter.fatal Internal_error ~extra_remarks: [Asai.Diagnostic.loctextf "%a is not in the index" URI.pp uri]
    | Some tree ->
      match Tree.to_code tree with
      | None -> None
      | Some {nodes; _} ->
        match Analysis.node_at ~position nodes with
        | None -> None
        | Some node ->
          let tree_under_cursor =
            let* {value = addr; _} = Analysis.extract_addr node in
            let uri_under_cursor = URI_scheme.named_uri ~base: forest.config.url addr in
            State.get_article uri_under_cursor forest
          in
          match tree_under_cursor with
          | Some article -> Some (render article.mainmatter)
          | None ->
            let* doc = Tree.to_doc tree in
            let* search_term = Analysis.word_at ~position doc in
            let results = List.map snd @@ Index.search forest.search_index search_term in
            Some Format.(asprintf "Relevant results:@.%a@." (pp_print_list ~pp_sep: (fun out () -> fprintf out "@.") URI.pp) results)
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
