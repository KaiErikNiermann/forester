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
open State.Syntax

open struct
  module T = Types
  module L = Lsp.Types
end

type completion =
  | Addrs
  | New_addr
  | Assets
  | Visible
  | Date
[@@deriving show]

module S = Set.Make(struct
  type t = completion
  let compare = compare
end)

type completion_kind = {
  text: string -> completion option;
  code: Code.node Asai.Range.located Analysis.Context.t -> completion option;
  syn: Syn.node Asai.Range.located Analysis.Context.t -> completion option;
}

let subtree_completion : completion_kind =
  let text word_before =
    if Str.(string_match (regexp {|.*subtree.*|}) word_before 0) then
      Some New_addr
    else None
  in
  let code (context : _ Analysis.Context.t) =
    match context with
    | Prev (Asai.Range.{value = Code.Subtree (_, _); _}, _)
    | Prev (_, Asai.Range.{value = Code.Subtree (_, _); _})
    | Parent {value = Code.Subtree (_, _); _} ->
      Some New_addr
    | Parent _
    | Prev (_, _)
    | Top _ ->
      None
  in
  let syn (context : Syn.node Range.located Analysis.Context.t) =
    match context with
    | (Top {value = Subtree _; _}) -> Some New_addr
    | (Prev (_, {value = Subtree _; _;})) -> Some New_addr
    | _ -> None
  in
  {text; code; syn}

let asset_completion : completion_kind =
  let text word_before =
    if Str.(string_match (regexp {|.*route-asset.*|}) word_before 0) then
      Some Assets
    else None
  in
  let code (context : _ Analysis.Context.t) =
    match context with
    | Prev (Asai.Range.{value = Code.Ident ["route-asset"]; _}, _)
    | Prev (_, Asai.Range.{value = Code.Ident ["route-asset"]; _}) ->
      Some Assets
    | Prev _ | Parent _ | Top _ -> None
  in
  let syn (context : Syn.node Range.located Analysis.Context.t) =
    match context with
    | Top {value = Route_asset; _} -> Some Assets
    | Prev (_, {value = Route_asset; _;}) -> Some Assets
    | _ -> None
  in
  {text; code; syn}

let date_completion : completion_kind =
  let text word_before =
    if Str.(string_match (regexp {|.*date.*|}) word_before 0) then
      Some Date
    else None
  in
  let code (context : _ Analysis.Context.t) =
    match context with
    | Prev (Asai.Range.{value = Code.Ident ["date"]; _}, _)
    | Prev (_, Asai.Range.{value = Code.Ident ["date"]; _}) ->
      Some Date
    | Prev _ | Parent _ | Top _ -> None
  in
  let syn (_context : Syn.node Range.located Analysis.Context.t) = None in
  {text; code; syn}

let uri_completion : completion_kind =
  let text word_before =
    if Str.(string_match (regexp {|.*]($|}) word_before 0)
      || Str.(string_match (regexp {|.*\[\[$|}) word_before 0)
      || Str.(string_match (regexp {|.*transclude.*|}) word_before 0) then
      Some Addrs
    else None
  in
  let code (_context : _ Analysis.Context.t) = None in
  let syn (_context : Syn.node Range.located Analysis.Context.t) = None in
  {text; code; syn}

let new_uri_completion : completion_kind =
  let text context =
    if Str.(string_match (regexp {|.*subtree\[.*|}) context 0) then Some New_addr
    else None
  in
  let code context = Option.map (Fun.const New_addr) (uri_completion.code context) in
  let syn context = Option.map (Fun.const New_addr) (uri_completion.syn context) in
  {text; code; syn}

let completion_types (t : Tree.t) ~position =
  let completions : completion_kind list = [
    uri_completion;
    asset_completion;
    subtree_completion;
    new_uri_completion;
    date_completion;
  ]
  in
  let code_opt = Tree.to_code t in
  let syn_opt = Tree.to_syn t in
  let doc_opt = Tree.to_doc t in
  let text_context = Option.bind doc_opt @@ Analysis.word_before ~position in
  Logs.debug (fun m -> m "Text_context: %a" Format.(pp_print_option pp_print_string) text_context);
  let code_context =
    let enclosing_group = Analysis.get_enclosing_code_group in
    let@ position = Option.bind @@ Analysis.enclosing_group_start ~enclosing_group ~position t in
    let@ code = Option.bind code_opt in
    Analysis.parent_or_prev_at_code ~position code.nodes
  in
  let syn_context =
    let enclosing_group = Analysis.get_enclosing_syn_group in
    let@ position = Option.bind @@ Analysis.enclosing_group_start ~enclosing_group ~position t in
    let@ syn = Option.bind syn_opt in
    Analysis.parent_or_prev_at_syn ~position syn.nodes
  in
  completions
  |> List.fold_left
      begin
        fun acc completion_kind ->
          (* let module Kind = (val completion_kind : CompletionKind) in *)
          let compl =
            List.fold_left
              begin
                fun acc a ->
                  match a with
                  | None -> acc
                  | Some compl -> S.add compl acc
              end
              S.empty
              [
                Option.bind text_context completion_kind.text;
                Option.bind code_context completion_kind.code;
                Option.bind syn_context completion_kind.syn;
              ]
          in
          S.union compl acc
      end
      S.empty

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

let asset_completions ~(config : Config.t) =
  let asset_dirs = config.assets in
  let paths = List.of_seq @@ Hashtbl.to_seq_keys Asset_router.router in
  let@ asset_path = List.filter_map @~ paths in
  let@ insertText =
    Option.map @~
      List.find_map
        (fun dir ->
          let dir_path = Unix.realpath dir in
          if String.starts_with ~prefix: dir_path asset_path then
            try
              Some
                (
                  String.sub
                    asset_path
                    (String.length dir_path - String.length dir)
                    (String.length asset_path - String.length dir_path + String.length dir)
                )
            with
              | _ -> None
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
  | Term syn ->
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
  | Xmlns _ ->
    assert false

(* These are useful completion items that are handled during parsing, not during expansion and are thus not "builtins"*)

module Syntax = struct
  let verb =
    let insertText =
      Lsp.Snippet.(
        let open O in
        to_string @@
        "\\startverb<<|\n" @+ tabstop 1 +@ "\n<<|"
      )
    in
    L.CompletionItem.create
      ~insertTextFormat: Snippet
      ~insertText
      ~label: "startverb"
      ~documentation: (`String "Create a verbatim block")
      ()
end

(* Incomplete. The idea here is to create clever snippets for specific syntax items. *)
let syntax_completions =
  [
    ("startverb", "startverb");
    ("scope", "scope");
    ("put", "put");
    ("put?", "put?");
    ("get", "get");
    ("import", "import");
    ("export", "export");
    ("namespace", "namespace");
    ("open", "open");
    ("def", "def");
    ("alloc", "alloc");
    ("let", "let");
    ("fun", "fun");
    ("subtree", "subtree");
    ("object", "object");
    ("patch", "patch");
    ("call", "call");
    ("datalog", "datalog");
    ("xmlns", "xmlns");
  ]
  |> List.map @@ fun (insertText, label) ->
    L.CompletionItem.create
      ~insertText
      ~label
      ()

let addr_completions ~(forest : State.t) : L.CompletionItem.t list =
  List.of_seq @@
    let@ uri, tree = Seq.map @~ URI.Tbl.to_seq forest.index in
    let frontmatter = Tree.get_frontmatter tree in
    let title = Option.map (State.get_expanded_title @~ forest) frontmatter in
    let render = Plain_text_client.string_of_content ~forest in
    let documentation =
      let taxon = Option.bind frontmatter (fun fm -> fm.taxon) in
      let content =
        Format.asprintf
          {|%s\n %s\n |}
          (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "# %s" (render s)) title)
          (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "taxon: %s" (render s)) taxon)
      in
      Some (`String content)
    in
    let insertText = URI_scheme.name uri in
    let title_text = Option.map render title in
    let filterText = Option.fold ~none: insertText ~some: (fun s -> insertText ^ " " ^ s) title_text in
    L.CompletionItem.create
      ?documentation
      ~label: (Format.(asprintf "%a (%s)" (pp_print_option pp_print_string) title_text (URI_scheme.name uri)))
      ~insertText
      ~filterText
      ()

let new_addr_completions ~(forest : State.t) : L.CompletionItem.t list =
  let next mode = URI_util.next_uri ~prefix: None ~mode ~forest in
  [
    L.CompletionItem.create ~label: "random" ~insertText: (next `Random) ();
    L.CompletionItem.create ~label: "sequential" ~insertText: (next `Sequential) ()
  ]

let visible_completions ~(forest : State.t) ~(position : L.Position.t) : Tree.code option -> L.CompletionItem.t list = function
  | None ->
    List.append syntax_completions @@
      let@ path, _ = List.map @~ List.of_seq @@ Trie.to_seq Expand.initial_visible_trie in
      L.CompletionItem.create
        ~insertText: "todo"
        ~label: (String.concat "/" path)
        ()
  | Some {nodes; _} ->
    Analysis.get_visible ~position ~forest nodes
    |> Trie.to_seq
    |> List.of_seq
    |> List.filter_map make
    |> List.append syntax_completions

let date_completions () : L.CompletionItem.t list =
  let now = Human_datetime.now () in
  let now_string = Format.asprintf "%a" Human_datetime.pp now in
  [
    L.CompletionItem.create ~label: now_string ~insertText: now_string ()
  ]

let compute ({context; position; textDocument = {uri}; _;}: L.CompletionParams.t) =
  Logs.debug (fun m -> m "when computing completions for %s" (Lsp.Uri.to_string uri));
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
    Logs.debug (fun m -> m "Received completion request at %s" (Yojson.Safe.to_string (L.Position.yojson_of_t position)));
    Logs.debug (fun m -> m "phase is %s" (Tree.show_phase tree));
    let completion_types = completion_types ~position tree in
    Logs.debug (fun m -> m "computed completion types: %a" (Format.pp_print_list pp_completion) (S.to_list completion_types));
    let items =
      let@ completion = List.concat_map @~ S.to_list completion_types in
      match completion with
      | Addrs -> addr_completions ~forest
      | New_addr -> new_addr_completions ~forest
      | Assets -> asset_completions ~config
      | Visible -> visible_completions ~forest ~position code
      | Date -> date_completions ()
    in
    Logs.debug (fun m -> m "items: %d" (List.length items));
    Option.some @@ `CompletionList (L.CompletionList.create ~isIncomplete: false ~items ())
