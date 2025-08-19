(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_frontend
open Forester_compiler
open State.Syntax

open struct
  module L = Lsp.Types
end

let list_of_option = function
  | Some x -> [x]
  | None -> []

let consume_addr_for_inlay ~(config : Config.t) ~(forest : State.t) (node : Syn.node Range.located) : string option * Syn.t =
  match node.value with
  | Link {title = None; dest = [{value = Text addr; _}]} -> Some addr, []
  | Subtree (addr, nodes) -> addr, nodes
  | _ -> None, Syn.children node

let inlay_hint_for_addr ~(config : Config.t) ~(forest : State.t) ~(pos : Range.position) (addr : string) : L.InlayHint.t option =
  let uri = URI_scheme.named_uri ~base: config.url addr in
  let@ {frontmatter; _} = Option.bind @@ State.get_article uri forest in
  let@ title = Option.bind frontmatter.title in
  let content = " " ^ Plain_text_client.string_of_content ~forest title in
  Option.some @@ L.InlayHint.create ~position: (Lsp_shims.Loc.lsp_pos_of_pos pos) ~label: (`String content) ()

let pos_of_node (node : 'a Range.located) : Range.position option =
  let@ loc = Option.bind node.loc in
  match Range.view loc with
  | `End_of_file _ -> None
  | `Range (_, pos) -> Some pos

let rec extract_inlayable_hints ~(config : Config.t) ~(forest : State.t) (nodes : Syn.t) : L.InlayHint.t list =
  let@ node = List.concat_map @~ nodes in
  let addr_opt, rest = consume_addr_for_inlay ~config ~forest node in
  let hint_opt =
    let@ addr = Option.bind addr_opt in
    let@ pos = Option.bind @@ pos_of_node node in
    inlay_hint_for_addr ~config ~forest ~pos addr
  in
  let hints = extract_inlayable_hints ~config ~forest rest in
  list_of_option hint_opt @ hints


let compute (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let uri = URI_scheme.lsp_uri_to_uri ~base: config.url params.textDocument.uri in
  let@ {nodes; _} = Option.map @~ Option.bind forest.={uri} Tree.to_syn in
  extract_inlayable_hints ~config ~forest nodes
