(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
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

module Kind = Completion_kind

type completion = Addrs | New_addr | Assets | Visible | Date [@@deriving show]

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
    | Parent _ | Prev (_, _) | Top _ -> None
  in
  let syn (context : Syn.node Range.located Analysis.Context.t) =
    match context with
    | Top {value = Subtree _; _} -> Some New_addr
    | Prev (_, {value = Subtree _; _}) -> Some New_addr
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
    | Prev (_, {value = Route_asset; _}) -> Some Assets
    | _ -> None
  in
    {text; code; syn}

let date_completion : completion_kind =
  let text word_before =
    if Str.(string_match (regexp {|.*date.*|}) word_before 0) then Some Date
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
      || Str.(string_match (regexp {|.*transclude.*|}) word_before 0) then Some Addrs
    else None
  in
  let code (_context : _ Analysis.Context.t) = None in
  let syn (_context : Syn.node Range.located Analysis.Context.t) = None in
    {text; code; syn}

let new_uri_completion : completion_kind =
  let text context =
    if Str.(string_match (regexp {|.*subtree\[.*|}) context 0) then
      Some New_addr
    else None
  in
  let code context =
    Option.map (Fun.const New_addr) (uri_completion.code context)
  in
  let syn context =
    Option.map (Fun.const New_addr) (uri_completion.syn context)
  in
    {text; code; syn}

let completion_kinds = [
  uri_completion;
  asset_completion;
  subtree_completion;
  new_uri_completion;
  date_completion;
]

let completion_types (tree : Tree.t) ~position : completion list =
  let code_opt = Tree.to_code tree in
  let syn_opt = Tree.to_syn tree in
  let doc_opt = Tree.to_doc tree in
  let text_context = Option.bind doc_opt @@ Analysis.word_before ~position in
  Logs.debug (fun m ->
    m "Text_context: %a" Format.(pp_print_option pp_print_string) text_context
  );
  let code_context =
    let enclosing_group = Analysis.get_enclosing_code_group in
    let@ position =
      Option.bind @@
        Analysis.enclosing_group_start ~enclosing_group ~position tree
    in
    let@ code = Option.bind code_opt in
    Analysis.parent_or_prev_at_code ~position code.nodes
  in
  let syn_context =
    let enclosing_group = Analysis.get_enclosing_syn_group in
    let@ position =
      Option.bind @@
        Analysis.enclosing_group_start ~enclosing_group ~position tree
    in
    let@ syn = Option.bind syn_opt in
    Analysis.parent_or_prev_at_syn ~position syn.nodes
  in
  completion_kinds
  |> List.fold_left
      (fun acc completion_kind ->
        let detected =
          List.fold_left
            (fun detected candidate ->
              match candidate with
              | None -> detected
              | Some completion -> S.add completion detected
            )
            S.empty
            [
              Option.bind text_context completion_kind.text;
              Option.bind code_context completion_kind.code;
              Option.bind syn_context completion_kind.syn;
            ]
        in
        S.union detected acc
      )
      S.empty
  |> S.to_list

module Syntax = struct
  let verb =
    let insert_text =
      Lsp.Snippet.(
        let open O in
        to_string @@ "\\startverb<<|\n" @+ (tabstop 1 +@ "\n<<|")
      )
    in
    L.CompletionItem.create ~insertTextFormat: Snippet ~insertText: insert_text ~label: "startverb" ~documentation: (`String "Create a verbatim block") ()
end

let syntax_completions =
  [("startverb", "startverb");
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
  |> List.map @@ fun (insert_text, label) ->
    L.CompletionItem.create ~insertText: insert_text ~label ()

let asset_completions ~(config : Config.t) =
  let asset_dirs = config.assets in
  let paths = List.of_seq @@ Hashtbl.to_seq_keys Asset_router.router in
  let@ asset_path = List.filter_map @~ paths in
  let@ insert_text =
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
                    (
                      String.length asset_path - String.length dir_path +
                        String.length dir
                    )
                )
            with
              | _ -> None
          else None
        )
        asset_dirs
  in
  L.CompletionItem.create
    ~label: insert_text
    ~kind: File
    ~insertText: insert_text
    ()

let addr_completions ~(forest : State.t) : L.CompletionItem.t list =
  let articles = List.of_seq @@ State.get_all_articles forest in
  let@ article = List.filter_map @~ articles in
  let frontmatter = article.frontmatter in
  let@ _ = Option.bind frontmatter.title in
  let title = State.get_expanded_title frontmatter forest in
  let render = Plain_text_client.string_of_content ~forest in
  let documentation =
    let taxon = frontmatter.taxon in
    let content =
      Format.asprintf
        "# %s\n %s\n "
        (render title)
        (
          Option.fold
            ~none: ""
            ~some: (fun s -> Format.asprintf "taxon: %s" (render s))
            taxon
        )
    in
    Some (`String content)
  in
  let@ uri = Option.bind @@ frontmatter.uri in
  let@ uri_name = Option.bind @@ URI_scheme.name uri in
  let title_text = render title in
  Option.some @@
    L.CompletionItem.create
      ?documentation
      ~label: Format.(asprintf "%a (%s)" pp_print_string title_text uri_name)
      ~insertText: uri_name
      ~filterText: (uri_name ^ " " ^ title_text)
      ()

let new_addr_completions ~(forest : State.t) : L.CompletionItem.t list =
  let next mode = URI_util.next_uri ~prefix: None ~mode ~forest in
  [
    L.CompletionItem.create ~label: "random" ~insertText: (next `Random) ();
    L.CompletionItem.create
      ~label: "sequential"
      ~insertText: (next `Sequential)
      ();
  ]

let visible_completions
  ~(forest : State.t)
  ~(position : L.Position.t)
  : Tree.code option -> L.CompletionItem.t list
= function
  | None ->
    List.append syntax_completions @@
      let@ path, _ =
        List.map @~ List.of_seq @@ Trie.to_seq Expand.initial_visible_trie
      in
      L.CompletionItem.create
        ~insertText: "todo"
        ~label: (String.concat "/" path)
        ()
  | Some {nodes; _} ->
    Analysis.get_visible ~position ~forest nodes
    |> Trie.to_seq
    |> List.of_seq
    |> List.filter_map Kind.completion_item_of_visible_entry
    |> List.append syntax_completions

let date_completions () : L.CompletionItem.t list =
  let now = Human_datetime.now () in
  let now_string = Format.asprintf "%a" Human_datetime.pp now in
    [L.CompletionItem.create ~label: now_string ~insertText: now_string ()]

let items_for_completion
    ~(forest : State.t)
    ~(position : L.Position.t)
    ~(completion : completion)
    ~(code : Tree.code option)
  =
  let config = forest.config in
  match completion with
  | Addrs -> addr_completions ~forest
  | New_addr -> new_addr_completions ~forest
  | Assets -> asset_completions ~config
  | Visible -> visible_completions ~forest ~position code
  | Date -> date_completions ()
