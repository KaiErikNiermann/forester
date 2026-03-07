(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** Pretty-printer for Forester markup language. Converts parsed AST back to
    formatted source code. *)

open Forester_prelude
open Forester_core
module L = Lsp.Types
module Render = Document_format_render

type config = Document_format_render.config = {
  indent_width : int;
  max_line_width : int;
  break_after_frontmatter : bool;
}
(** Configuration for the formatter *)

let default_config = Render.default_config

let rec pp_nodes ~config ~indent fmt (nodes : Code.t) =
  let frontmatter, body =
    let rec split_frontmatter acc = function
      | [] -> (List.rev acc, [])
      | ({ Range.value; _ } as node) :: rest ->
          if Render.is_frontmatter_node value then
            split_frontmatter (node :: acc) rest
          else begin
            match value with
            | Code.Comment _ -> split_frontmatter (node :: acc) rest
            | _ -> (List.rev acc, node :: rest)
          end
    in
    if indent = 0 then split_frontmatter [] nodes else ([], nodes)
  in
  List.iter
    (fun node ->
      pp_node ~config ~indent fmt node;
      Format.pp_print_newline fmt ())
    frontmatter;
  if config.break_after_frontmatter && frontmatter <> [] && body <> [] then
    Format.pp_print_newline fmt ();
  let rec print_body prev_was_block = function
    | [] -> ()
    | ({ Range.value; _ } as node) :: rest ->
        let is_block = Render.is_block_node value in
        if prev_was_block && is_block then Format.pp_print_newline fmt ();
        pp_node ~config ~indent fmt node;
        if is_block then Format.pp_print_newline fmt ();
        print_body is_block rest
  in
  print_body false body

and pp_node ~config ~indent fmt node =
  let render =
    Render.
      {
        pp_nodes =
          (fun ~indent nested_fmt nodes ->
            pp_nodes ~config ~indent nested_fmt nodes);
      }
  in
  Render.pp_node ~render ~config ~indent fmt node

(** Format code to a string *)
let format_code ?(config = default_config) (code : Code.t) : string =
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt config.max_line_width;
  pp_nodes ~config ~indent:0 fmt code;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Compute formatting edits for a document *)
let compute (params : L.DocumentFormattingParams.t) =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let uri = params.textDocument.uri in
  let forester_uri = URI_scheme.lsp_uri_to_uri ~base:forest.config.url uri in
  match Forester_compiler.State.get_code forest forester_uri with
  | None ->
      Logs.debug (fun m ->
          m "Format: Could not find code for %a" URI.pp forester_uri);
      None
  | Some { nodes; _ } -> begin
      let doc_opt =
        match Forester_compiler.State.find_opt forest forester_uri with
        | Some tree -> Forester_core.Tree.to_doc tree
        | None -> None
      in
      match doc_opt with
      | None ->
          Logs.debug (fun m ->
              m "Format: Could not find document for %a" URI.pp forester_uri);
          None
      | Some doc ->
          let text = Lsp.Text_document.text doc in
          let line_count =
            let count = ref 0 in
            String.iter (fun c -> if c = '\n' then incr count) text;
            !count + 1
          in
          let last_line_length =
            match String.rindex_opt text '\n' with
            | None -> String.length text
            | Some i -> String.length text - i - 1
          in
          let config =
            { default_config with indent_width = params.options.tabSize }
          in
          let formatted = format_code ~config nodes in
          let range =
            L.Range.create
              ~start:(L.Position.create ~line:0 ~character:0)
              ~end_:
                (L.Position.create ~line:line_count ~character:last_line_length)
          in
          let edit = L.TextEdit.create ~range ~newText:formatted in
          Some [ edit ]
    end

(** Compute range formatting edits *)
let compute_range (params : L.DocumentRangeFormattingParams.t) =
  let full_params =
    L.DocumentFormattingParams.create ~textDocument:params.textDocument
      ~options:params.options ()
  in
  compute full_params
