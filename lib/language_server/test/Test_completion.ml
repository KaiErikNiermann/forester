(*
 subtree_completion_text* SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend
open Forester_lsp
open Forester_test
open Testables

open struct module L = Lsp.Types end

let test_find_with_prev () =
  let code =
    Result.get_ok @@
      parse_string
        {|
\li{I am on line 1}
\li{I am on line 2}
\li{I am on line 3}
\li{I am on line 4}
|}
  in
  Logs.debug (fun m -> m "%a" Code.pp code);
  let result = Analysis.find_with_prev ~position: {line = 4; character = 5;} code in
  begin
    let@ {loc; _} = List.iter @~ code in
    Logs.debug (fun m -> m "%s" (Yojson.Safe.to_string (L.Range.yojson_of_t (Lsp_shims.Loc.lsp_range_of_range loc))));
  end;
  match result with
  | None -> Alcotest.fail "no result"
  | Some (prev, node) ->
    let open DSL.Code in
    Alcotest.(check code_node)
      "node"
      (
        Group
          (
            Braces,
            [
              text "I";
              text " ";
              text "am";
              text " ";
              text "on";
              text " ";
              text "line";
              text " ";
              text "4"
            ]
          )
      )
      (Code.map strip_code node.value);
    match prev with
    | None -> Alcotest.fail "no prev"
    | Some prev ->
      Alcotest.(check code_node) "Pred" (Ident ["li"]) prev.value

let test_node_at_pos_with_prev_or_parent () =
  let code =
    Result.get_ok @@
      parse_string
        {|
\def\foo[arg]{Hello, \arg!}
\p{\ul{\li{item}}}
|}
  in
  let prev = Analysis.find_with_prev ~position: {character = 5; line = 2;} code in
  begin
    match prev with
    | None -> Alcotest.fail "node not found"
    | Some (prev, node) ->
      match prev with
      | None -> Alcotest.fail "no pred"
      | Some prev ->
        Alcotest.(check code_node) "check pred" (Ident ["p"]) prev.value;
        Alcotest.(check code_node)
          "check node"
          (DSL.Code.(braces [ul; braces [li; braces [text "item"]]]).value)
          (Code.map strip_code node.value)
  end;
  let result = Analysis.parent_or_prev_at_code ~position: {character = 5; line = 2;} code in
  match result with
  | None -> Alcotest.fail "no result"
  | Some (Prev (_pred, _node)) -> Alcotest.fail "pred"
  | Some (Top node) -> Alcotest.(check code_node) "node" (Ident ["ul"]) node.value
  | Some (Parent _) -> Alcotest.fail "parent"

let test_word_before () =
  let@ () = Reporter.easy_run in
  let text =
    (*          1         2  *)
    (*01234567890123456789012*)
    {|asdfasdf \route-asset{assets/|}
  in
  let doc =
    Lsp.Text_document.make
      ~position_encoding: `UTF8
      {
        textDocument =
        L.TextDocumentItem.create
          ~languageId: "forester"
          ~uri: (Lsp.Uri.of_string "")
          ~version: 1
          ~text
      }
  in
  let result =
    Option.get @@
      let position = L.Position.create ~character: 22 ~line: 0 in
      Analysis.word_before ~position doc
  in
  Alcotest.(check string) "" "\\route-asset{" result

let test_uri_completion () =
  let result_1 = Completion.uri_completion.text "complete](" in
  let result_2 = Completion.uri_completion.text "\\transclude{" in
  Alcotest.(check @@ option completion_type) "link" (Some Addrs) result_1;
  Alcotest.(check @@ option completion_type) "transclusion" (Some Addrs) result_2

let test_completion_types ~env () =
  let@ () = Reporter.easy_run in
  let forest = State.make ~env ~config: (Config.default ()) ~dev: false () in
  let result_1 =
    (*                                         0123456789012*)
    let code = Result.get_ok @@ parse_string {|\route-asset{}|} in
    let uri = URI.of_string_exn "http://localhost/tree" in
    let expanded =
      let@ () = Resolver.Scope.easy_run in
      Expand.Builtins.register_builtins Expand.builtins;
      Expand.expand ~forest code
    in
    let tree = mk_tree ~uri ~code ~expanded in
    Logs.debug (fun m -> m "%a" Tree.pp tree);
    Completion.(S.to_list (completion_types ~position: {line = 0; character = 12} tree))
  in
  let result_2 =
    (*           0123456789012*)
    let text = {|\p{[link](}|} in
    let tree =
      let uri = Lsp.Uri.of_path "/foo" in
      Tree.Document
        (
          Lsp.Text_document.make
            ~position_encoding: `UTF8
            {
              textDocument = {
                languageId = "forester";
                text;
                uri;
                version = 1
              }
            }
        )
    in
    Logs.debug (fun m -> m "%a" Tree.pp tree);
    Completion.(S.to_list (completion_types ~position: {line = 0; character = 10} tree))
  in
  Alcotest.(check @@ list completion_type) "" [Assets] result_1;
  Alcotest.(check @@ list completion_type) "" [Addrs] result_2

let test_contains () =
  let position = L.Position.create ~character: 12 ~line: 0 in
  let src = `File "foo" in
  let start_pos = Asai.Range.{source = src; offset = 1; start_of_line = 0; line_num = 1} in
  let end_pos = Asai.Range.{source = src; offset = 13; start_of_line = 0; line_num = 1} in
  let loc = Option.some @@ Asai.Range.make (start_pos, end_pos) in
  let result = Analysis.contains ~position loc in
  Alcotest.(check bool) "" true result

let test_node_at () =
  let case_1 =
    let position = L.Position.create ~character: 12 ~line: 0 in
    (*                                         0123456789012*)
    let code = Result.get_ok @@ parse_string {|\import{asdf}|} in
    let result = Option.get @@ Analysis.node_at_code ~position code in
    Alcotest.(check code_node)
      ""
      (Import (Private, "asdf"))
      (Asai.Range.(result.value))
  in
  let case_2 =
    let code =
      Result.get_ok @@
        parse_string
          (*          1         2         3         4         5          *)
          (*0123456789012345678901234567890123456789012345678901234567890123*)
          {|\def\Mor[arg1][arg2][arg3]{#{{\arg2}\xrightarrow{\arg1}{\arg3}}}
      |}
    in
    let position_1 = L.Position.create ~character: 40 ~line: 0 in
    let position_2 = L.Position.create ~character: 50 ~line: 0 in
    let result1 = Option.get @@ Analysis.node_at_code ~position: position_1 code in
    let result2 = Option.get @@ Analysis.node_at_code ~position: position_2 code in
    Alcotest.(check code_node) "" (Ident ["xrightarrow"]) (Asai.Range.(result1.value));
    Alcotest.(check code_node) "" (Ident ["arg1"]) (Asai.Range.(result2.value))
  in
  let case_3 =
    let code =
      Result.get_ok @@
        parse_string
          {|
%123456789012345678901234567890123456789012345678901234567890
\p{
  \foo{
    \ul{
      \li{asdf}
    }
  }
}
        |}
    in
    let position = L.Position.create ~character: 12 ~line: 5 in
    let result = Option.get @@ Analysis.node_at_code ~position code in
    Alcotest.(check code_node) "" (Text "asdf") (Asai.Range.(result.value))
  in
  case_1;
  case_2;
  case_3

let test_addr_at () =
  let code = Result.get_ok @@ parse_string {|\transclude{tfmt-0005}|} in
  let position = L.Position.create ~character: 13 ~line: 0 in
  let result = Option.get @@ Analysis.addr_at ~position code in
  Alcotest.(check string) "" "tfmt-0005" (Asai.Range.(result.value))

let test_word_at () =
  let text =
    {|Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum
    |}
  in
  let doc =
    Lsp.Text_document.make
      ~position_encoding: `UTF8
      {
        textDocument =
        L.TextDocumentItem.create
          ~languageId: "forester"
          ~uri: (Lsp.Uri.of_string "")
          ~version: 1
          ~text
      }
  in
  let lorem =
    Option.get @@
      let position = L.Position.create ~character: 0 ~line: 0 in
      Analysis.word_at ~position doc
  in
  let dolor =
    Option.get @@
      let position = L.Position.create ~character: 13 ~line: 0 in
      Analysis.word_at ~position doc
  in
  let irure =
    Option.get @@
      let position = L.Position.create ~character: 25 ~line: 3 in
      Analysis.word_at ~position doc
  in
  Alcotest.(check string) "" "Lorem" lorem;
  Alcotest.(check string) "" "dolor" dolor;
  Alcotest.(check string) "" "irure" irure

let test_enclosing_group ~env () =
  let forest = State.make ~env ~config: (Config.default ()) ~dev: false () in
  (*                                         012345678901234*)
  let code = Result.get_ok @@ parse_string {|\foo{\bar{baz}}|} in
  let expanded =
    let@ () = Resolver.Scope.easy_run in
    Expand.expand ~forest code
  in
  let uri = URI.of_string_exn "localhost:foo" in
  let tree = mk_tree ~uri ~code ~expanded in
  let case_1 =
    let position = L.Position.{line = 0; character = 6} in
    match Analysis.get_enclosing_syn_group ~position tree with
    | None -> Alcotest.fail "no enclosing group"
    | Some {value = (d, nodes); _} ->
      let open DSL.Syn in
      Alcotest.(check delim) "" Braces d;
      Alcotest.(check syn)
        ""
        [tex_cs "bar"; braces [text "baz"]]
        (strip_syn nodes)
  in
  let case_2 =
    let position = L.Position.{line = 0; character = 10} in
    match Analysis.get_enclosing_syn_group ~position tree with
    | None -> Alcotest.fail "no enclosing group"
    | Some {value = (d, nodes); _} ->
      let open DSL.Syn in
      Alcotest.(check delim) "" Braces d;
      Alcotest.(check syn) "" [text "baz"] (strip_syn nodes)
  in
  List.iter Fun.id [case_1; case_2]

let test_subtree_completion_text () =
  (*           012345678*)
  let text = {|\subtree[|} in
  let position : L.Position.t = {line = 0; character = 9;} in
  let doc =
    Lsp.Text_document.make
      ~position_encoding: `UTF8
      {
        textDocument =
        L.TextDocumentItem.create
          ~languageId: "forester"
          ~uri: (Lsp.Uri.of_string "")
          ~version: 1
          ~text
      }
  in
  let result = Completion.(S.to_list @@ completion_types ~position (Document doc)) in
  Alcotest.(check bool)
    ""
    (Str.(string_match (regexp {|.*route-asset.*|}) {|\subtree[|} 0))
    false;
  Alcotest.(check @@ list completion_type)
    ""
    [New_addr]
    result

let test_subtree_completion_code () =
  Alcotest.(check string)
    ""
    ""
    ""

let test_asset_completion_text () =
  (*           0123456789012*)
  let text = {|\route-asset{|} in
  let position : L.Position.t = {line = 0; character = 13;} in
  let doc =
    Lsp.Text_document.make
      ~position_encoding: `UTF8
      {
        textDocument =
        L.TextDocumentItem.create
          ~languageId: "forester"
          ~uri: (Lsp.Uri.of_string "")
          ~version: 1
          ~text
      }
  in
  let result = Completion.(S.to_list @@ completion_types ~position (Document doc)) in
  Alcotest.(check @@ list completion_type)
    ""
    [Assets]
    result

let test_asset_completion_syn () =
  let@ () = Reporter.easy_run in
  let textDocument = find_doc (Test_env.get ()) "sterling-2024-cl-forester" in
  let position : L.Position.t = {line = 4; character = 27} in
  let params = L.CompletionParams.create ~position ~textDocument () in
  let completions =
    Completion.compute params
    |> Option.map (fun (`CompletionList L.CompletionList.{items; _}) ->
        String.concat "" @@
          List.map
            (fun i -> Format.asprintf "%s" (Yojson.Safe.to_string @@ L.CompletionItem.yojson_of_t i))
            items
      )
  in
  Alcotest.(check @@ option string)
    ""
    (Some {|{\"insertText\":\"assets/autocomplete.png\",\"kind\":17,\"label\":\"assets/autocomplete.png\"}{\"insertText\":\"assets/asset-completions.png\",\"kind\":17,\"label\":\"assets/asset-completions.png\"}{\"insertText\":\"assets/sterling-2024-cl-forester.pdf\",\"kind\":17,\"label\":\"assets/sterling-2024-cl-forester.pdf\"}{\"insertText\":\"assets/workspace-symbols.png\",\"kind\":17,\"label\":\"assets/workspace-symbols.png\"}{\"insertText\":\"assets/diagnostics.png\",\"kind\":17,\"label\":\"assets/diagnostics.png\"}{\"insertText\":\"assets/virtual-text.png\",\"kind\":17,\"label\":\"assets/virtual-text.png\"}{\"insertText\":\"assets/code-action.png\",\"kind\":17,\"label\":\"assets/code-action.png\"}{\"insertText\":\"assets/document-symbols.png\",\"kind\":17,\"label\":\"assets/document-symbols.png\"}{\"insertText\":\"assets/inlay-hints.png\",\"kind\":17,\"label\":\"assets/inlay-hints.png\"}|})
    completions

(* let test_uri () = *)
(*   Alcotest.(check string) *)
(*     "" *)
(*     "" *)
(*     "" *)

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = Config_parser.parse_forest_config_file "forest.toml" in
  let forest = Driver.batch_run ~env ~config ~dev: true in
  let lsp_io = LspEio.init env in
  let init = Lsp_state.{forest; lsp_io; should_shutdown = false;} in
  let dirs = Eio_util.paths_of_dirs ~env config.trees in
  let position = L.Position.create ~line: 17 ~character: 1 in
  let test_env = {
    dirs;
    config;
    position;
  }
  in
  let@ () = Test_env.run ~init: test_env in
  let@ () = Lsp_state.run ~init in
  Alcotest.run
    "Test_completion"
    [
      "Context",
      [
        "find_with_pred", `Quick, test_find_with_prev;
        "node_at_pos_with_predecessor_or_parent", `Quick, test_node_at_pos_with_prev_or_parent;
        "word_before", `Quick, test_word_before;
      ];
      "Analysis",
      [
        "contains", `Quick, test_contains;
        "node_at", `Quick, test_node_at;
        "addr_at", `Quick, test_addr_at;
        "word_at", `Quick, test_word_at;
        "get_enclosing_group", `Quick, (test_enclosing_group ~env);
      ];
      "Completion",
      [
        "completion_type", `Quick, (test_completion_types ~env);
      ];
      "subtree_completion",
      [
        "text", `Quick, test_subtree_completion_text;
        "code", `Quick, test_subtree_completion_code;
      ];
      "asset_completion",
      [
        "text", `Quick, test_asset_completion_text;
        "syn", `Quick, test_asset_completion_syn;
      ];
      "uri",
      [
        "uri_completion", `Quick, test_uri_completion;
      ];
    ]
