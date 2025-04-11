(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend

open struct
  module T = Types
  module L = Lsp.Types
end

open State.Syntax

let (let*) = Option.bind

let kind
  : Syn.node -> L.CompletionItemKind.t option
= function
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

(* Depending on the context, we may want to provide different kinds of completions*)
let completion_types ~position (tree : Tree.t) =
  match Tree.to_syn tree with
  | None -> []
  | Some syn ->
    let enclosing_group = Analysis.get_enclosing_syn_group ~position syn.nodes in
    match enclosing_group with
    | None -> []
    | Some {loc; value = _} ->
      let start =
        Option.map Range.view loc
        |> Option.map (function
            | `Range (start, _) -> start
            | `End_of_file pos -> pos
          )
      in
      let context =
        Option.bind
          start
          (fun pos ->
            let position = Lsp_shims.Loc.lsp_pos_of_pos pos in
            Analysis.parent_or_prev_at_syn ~position syn.nodes
          )
      in
      match context with
      | None -> []
      | Some (`Node {value = Route_asset; _}) -> [`Assets]
      | Some (`Parent _)
      | Some (`Node _) ->
        [`Visible]
      | Some (`Prev ({value; _}, _)) ->
        match value with
        | Group (Braces, _) -> []
        | Group (Parens, _) -> []
        | Group (Squares, _) -> [`Addrs]
        | Route_asset -> [`Assets]
        | Transclude -> [`Addrs]
        | Text _
        | Verbatim _
        | Math (_, _)
        | Link _
        | Subtree (_, _)
        | Fun (_, _)
        | Var _
        | Sym _
        | Put (_, _, _)
        | Default (_, _, _)
        | Get _
        | Xml_tag (_, _, _)
        | TeX_cs _
        | Prim _
        | Object _
        | Patch _
        | Call (_, _)
        | Results_of_query
        | Embed_tex
        | Ref
        | Title
        | Parent
        | Taxon
        | Meta
        | Attribution (_, _)
        | Tag _
        | Date
        | Number
        | Dx_sequent (_, _)
        | Dx_query (_, _, _)
        | Dx_prop (_, _)
        | Dx_var _
        | Dx_const (_, _)
        | Dx_execute
        | Syndicate_query_as_json_blob
        | Syndicate_current_tree_as_atom_feed
        | Current_tree ->
          []

let insert_text path = String.concat "/" path

let asset_completions ~(config : Config.t) () =
  let asset_dirs = config.assets in
  let paths = List.of_seq @@ Hashtbl.to_seq_keys Asset_router.router in
  let@ asset_path = List.filter_map @~ paths in
  let@ insertText =
    Option.map @~
      List.find_map
        (fun dir ->
          let dir_path = Unix.realpath dir in
          Logs.debug (fun m -> m "dir: %s" dir);
          Logs.debug (fun m -> m "dir_path: %s" dir_path);
          Logs.debug (fun m -> m "asset_path: %s" asset_path);
          if String.starts_with ~prefix: dir_path asset_path then
            Some
              (
                String.sub
                  asset_path
                  (String.length dir_path - String.length dir)
                  (String.length asset_path - String.length dir_path + String.length dir)
              )
          else None
        )
        asset_dirs
  in
  L.CompletionItem.create
    ~label: insertText
    ~kind: File
    ~insertText
    ()

let make (path, (data, _): Yuujinchou.Trie.path * (Resolver.P.data * Asai.Range.t option)) =
  match data with
  | Resolver.P.Term syn ->
    (* NOTE: Eventually we want to analyse the syntax so that, for example,
       you can tab through the snippet for a function of arity n*)
    let kind = kind (List.hd syn).value in
    let insertText = insert_text path in
    Some
      (
        L.CompletionItem.create
          ?kind
          ~insertText
          ~label: (String.concat "/" path)
          ()
      )
  | Resolver.P.Xmlns _ ->
    None

let compute ({context; position; textDocument = {uri}; _;}: L.CompletionParams.t) =
  let triggerCharacter =
    match context with
    | Some {triggerCharacter; _} ->
      triggerCharacter
    | None -> None
  in
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = forest.config in
  let base = config.url in
  let uri = URI_scheme.lsp_uri_to_uri ~base uri in
  let tree = forest.={uri} in
  match tree with
  | None ->
    Reporter.fatal
      Internal_error
      ~backtrace: (Bwd.of_list [Asai.Diagnostic.loctextf "when computing completions for %a" URI.pp uri])
      ~extra_remarks: [Asai.Diagnostic.loctextf "%a was not found in the index" URI.pp uri]
  | Some tree ->
    let code = Tree.to_code tree in
    let syn = Tree.to_syn tree in
    assert (Option.is_some syn);
    let completion_types = completion_types ~position tree in
    let items =
      List.concat_map
        (function
          | `Addrs ->
            forest.index
            |> URI.Tbl.to_seq
            |> Seq.filter_map (fun (uri, tree) ->
                let frontmatter = Tree.get_frontmatter tree in
                let documentation =
                  let render = Plain_text_client.string_of_content ~forest in
                  let title = Option.map (fun fm -> State.get_expanded_title fm forest) frontmatter in
                  let taxon = Option.bind frontmatter (fun fm -> T.(fm.taxon)) in
                  let content =
                    Format.asprintf
                      {|%s\n %s\n |}
                      (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "# %s" (render s)) title)
                      (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "taxon: %s" (render s)) taxon)
                  in
                  Some (`String content)
                in
                let insertText =
                  (* TODO if host = current_host insert shortform else insert fully qualified uri*)
                  match triggerCharacter with
                  | Some "{" -> URI_scheme.name uri ^ "}"
                  | Some "(" -> URI_scheme.name uri ^ ")"
                  | Some "[" -> URI_scheme.name uri ^ "]"
                  | _ -> ""
                in
                Some (L.CompletionItem.create ?documentation ~label: (URI_scheme.name uri) ~insertText ())
              )
            |> List.of_seq
          | `Assets -> asset_completions ~config ()
          | `Visible ->
            List.concat @@
            Option.to_list @@
            Option.map
              (fun ({nodes; _}: Tree.code) ->
                Analysis.get_visible ~forest ~position nodes
                |> Trie.to_seq
                |> List.of_seq
                |> List.filter_map make
              )
              code
        )
        completion_types
    in
    Logs.debug (fun m -> m "items: %d" (List.length items));
    Some
      (
        `CompletionList
          (L.CompletionList.create ~isIncomplete: false ~items ())
      )
