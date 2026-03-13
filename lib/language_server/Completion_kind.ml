(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend
open State.Syntax

open struct
  module L = Lsp.Types
end

let kind_of_syn_node : Syn.node -> L.CompletionItemKind.t option = function
  | Fun (_, _) -> Some Function
  | Text _ | Verbatim _ -> Some Text
  | Meta -> Some Field
  | Route_asset -> Some File
  | Var _ -> Some Variable
  | Prim _
  | Transclude
  | Embed_tex
  | Title
  | Parent
  | Taxon
  | Attribution (_, _)
  | Tag _
  | Date
  | Number ->
    Some Keyword
  | Ref -> Some Reference
  | Group (_, _)
  | Math (_, _)
  | Link _
  | Subtree (_, _)
  | Sym _
  | Put (_, _, _)
  | Default (_, _, _)
  | Get _
  | Xml_tag (_, _, _)
  | TeX_cs _
  | Unresolved_ident _
  | Object _
  | Patch _
  | Call (_, _)
  | Results_of_query
  | Dx_sequent (_, _)
  | Dx_query (_, _, _)
  | Dx_prop (_, _)
  | Dx_var _
  | Dx_const (_, _)
  | Dx_execute
  | Syndicate_current_tree_as_atom_feed
  | Syndicate_query_as_json_blob
  | Current_tree ->
    None

let insert_text path = String.concat "/" path

let completion_item_of_visible_entry
    ((path, (data, _)):
      Yuujinchou.Trie.path * (Resolver.P.data * Asai.Range.t option)
    )
  =
  match data with
  | Term [] -> None
  | Term (node :: _) ->
    let kind = kind_of_syn_node node.value in
    let insert_text = insert_text path in
    Some
      (
        L.CompletionItem.create ?kind ~insertText: insert_text ~label: (String.concat "/" path) ()
      )
  | Xmlns _ -> assert false
