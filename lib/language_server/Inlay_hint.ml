(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_frontend
open Forester_compiler

module L = Lsp.Types

let (let*) = Option.bind

let compute (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  match params with
  | {textDocument;
    _;
  } ->
    let Lsp_state.{forest; _} = Lsp_state.get () in
    (* let render = Render.render forest STRING in *)
    let config = State.config forest in
    let host = config.host in
    match Forest.find_opt (State.parsed forest) (Iri_scheme.uri_to_iri ~host textDocument.uri) with
    | None ->
      None
    | Some {code; _} ->
      forest.resources
      |> Forest.get_all_resources
      |> List.iter
          (fun resource ->
            match resource with
            | Types.Article {frontmatter; _} ->
              Eio.traceln "%a" (Types.(pp_frontmatter pp_content)) frontmatter
            | Types.Asset _ -> ()
          );
      code
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
              | Some str ->
                let iri = Iri_scheme.user_iri ~host str in
                match Forest.get_article iri forest.resources with
                | None ->
                  None
                | Some {frontmatter; _} ->
                  match frontmatter.title with
                  | None -> None
                  | Some title ->
                    let content =
                      " " ^
                        Plain_text_client.string_of_content
                          ~forest: forest.resources
                          ~router: (Legacy_xml_client.route forest)
                          title
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
