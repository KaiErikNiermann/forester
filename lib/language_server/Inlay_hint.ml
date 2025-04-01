(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_frontend
open Forester_compiler
open State.Syntax

module L = Lsp.Types

let (let*) = Option.bind

let compute (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  match params with
  | {textDocument;
    _;
  } ->
    let Lsp_state.{forest; _} = Lsp_state.get () in
    let config = forest.config in
    let uri = URI_scheme.lsp_uri_to_uri ~base: config.url textDocument.uri in
    (* match Forest.find_opt forest.parsed  with *)
    match Option.bind forest.={uri} Tree.to_code with
    | None ->
      None
    | Some {nodes; _} ->
      nodes
      |> Analysis.flatten
      |> List.filter_map
          (fun
              (Range.{loc; _} as node)
            ->
            match Option.map Range.view loc with
            | None -> None
            | Some (`End_of_file _) -> None
            | Some (`Range (_, pos)) ->
              match Analysis.extract_addr node with
              | None -> None
              | Some {value = str; _} ->
                let uri = URI_scheme.named_uri ~base: config.url str in
                match State.get_article uri forest with
                | None ->
                  None
                | Some {frontmatter; _} ->
                  match frontmatter.title with
                  | None -> None
                  | Some title ->
                    let content =
                      " " ^ Plain_text_client.string_of_content ~forest title
                    in
                    Some
                      (
                        L.InlayHint.create
                          ~position: (Lsp_shims.Loc.lsp_pos_of_pos pos)
                          ~label: (`String content)
                          ()
                      )
          )
      |> Option.some
