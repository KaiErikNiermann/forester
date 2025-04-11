(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
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

module Handlers = Server.Handlers

module L = Lsp.Types

type test_env = {
  dirs: Eio.Fs.dir_ty Eio.Path.t list;
  config: Config.t;
  position: L.Position.t;
}

let find_doc (env : test_env) addr =
  let path =
    Eio.Path.native_exn @@
    Option.get @@
    Dir_scanner.find_tree env.dirs (URI_scheme.named_uri ~base: env.config.url addr)
  in
  ({uri = Lsp.Uri.of_path path}: L.TextDocumentIdentifier.t)

module Test_env = Algaeff.State.Make(struct type t = test_env end)

let find_tree addr =
  let env = Test_env.get () in
  let dirs = env.dirs in
  Eio.Path.native_exn @@
  Option.get @@
  Dir_scanner.find_tree dirs @@
  URI_scheme.named_uri ~base: env.config.url addr

let test_code_actions () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "tfmt-0005" in
  let params =
    L.CodeActionParams.create
      ~context: (L.CodeActionContext.create ~diagnostics: [] ())
      ~textDocument
      ~range: (L.Range.create ~start: env.position ~end_: env.position)
      ()
  in
  let result =
    Handlers.Code_action.compute params |> function
      | None -> assert false
      | Some actions -> List.map (function `CodeAction a -> a | _ -> assert false) actions
  in
  Alcotest.(check int)
    ""
    2
    (List.length result)

let test_call_hierarchy () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "tfmt-0005" in
  let params =
    L.CallHierarchyPrepareParams.create
      ~textDocument
      ~position: env.position
      ()
  in
  let result = Handlers.Call_hierarchy.compute params in
  Alcotest.(check @@ option @@ list unit)
    ""
    None
    result

let test_change_configuration () =
  let@ () = Reporter.easy_run in
  (* Eio.Path.save  ~create: *)
  let settings = `Assoc ["configuration_file", `String "other.toml"] in
  let params = L.DidChangeConfigurationParams.create ~settings in
  let result = Handlers.Change_configuration.compute params in
  Alcotest.(check unit)
    ""
    result
    ()

let location =
(module struct
  type t = L.Location.t
  let pp fmt t = Yojson.Safe.pp fmt (L.Location.yojson_of_t t)
  let equal = (=)
end: Alcotest.TESTABLE with type t = L.Location.t)

let test_definitions () =
  let@ () = Reporter.easy_run in
  let path = find_tree "tfmt-0005" in
  let textDocument : L.TextDocumentIdentifier.t = {uri = Lsp.Uri.of_path path} in
  let position = L.Position.create ~line: 16 ~character: 13 in
  let params = L.DefinitionParams.create ~position ~textDocument () in
  let result =
    Handlers.Definitions.compute params |> function
      | Some (`Location locations) -> locations
      | Some (`LocationLink _location_links) ->
        assert false
      (* location_links *)
      | None -> assert false
  in
  Alcotest.(check int) "" 1 (List.length result);
  let start = L.Position.create ~character: 1 ~line: 0 in
  let end_ = L.Position.create ~character: 1 ~line: 0 in
  let range = L.Range.create ~start ~end_ in
  let uri = Lsp.Uri.of_path @@ find_tree "tfmt-0006" in
  Alcotest.(check location)
    ""
    (L.Location.create ~range ~uri)
    (List.hd result)

let test_code_lens () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "tfmt-0005" in
  let params = L.CodeLensParams.create ~textDocument () in
  let result = List.length @@ Handlers.Code_lens.compute params in
  Alcotest.(check int) "" 0 result

let test_completion () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let position = env.position in
  let textDocument = find_doc env "tfmt-0005" in
  let params = L.CompletionParams.create ~position ~textDocument () in
  let result =
    Handlers.Completion.compute params |> function
      | Some (`CompletionList completions) -> List.length completions.items
      | None -> -1
  in
  Alcotest.(check int)
    ""
    0
    result

let test_did_change () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "tfmt-0005" in
  let params =
    L.DidChangeTextDocumentParams.create
      ~contentChanges: []
      ~textDocument: ({uri = textDocument.uri; version = 2})
  in
  let result = Handlers.Did_change.compute params in
  Alcotest.(check unit)
    ""
    ()
    result

let test_did_open () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "tfmt-0005" in
  let params =
    L.DidOpenTextDocumentParams.create
      ~textDocument: {
        uri = textDocument.uri;
        languageId = "forester";
        text = "\\title{hello}";
        version = 1;
      }
  in
  let result = Handlers.Did_open.compute params in
  Alcotest.(check unit)
    ""
    ()
    result

let test_document_link () =
  let _document_link =
  (module struct
    type t = L.DocumentLink.t
    let pp fmt t = Yojson.Safe.pp fmt (L.DocumentLink.yojson_of_t t)
    let equal = (=)
  end: Alcotest.TESTABLE with type t = L.DocumentLink.t)
  in
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let textDocument = find_doc env "jms-0052" in
  let params = L.DocumentLinkParams.create ~textDocument () in
  let Lsp_state.{forest; _} = Lsp_state.get () in
  (* URI.Tbl.iter *)
  (*   (fun uri tree -> Logs.debug (fun m -> m "%a ~> %s" URI.pp uri (Tree.pp_stage tree))) *)
  (*   forest.index; *)
  URI.Tbl.iter
    (fun _ diag -> List.iter Reporter.Tty.display diag)
    forest.diagnostics;
  let result = Handlers.Document_link.compute params in
  Alcotest.(check int)
    ""
    10
    (List.length @@ Option.get result)

let test_document_symbols () =
  let@ () = Reporter.easy_run in
  let _document_symbol =
  (module struct
    type t = L.DocumentSymbol.t
    let pp fmt t = Yojson.Safe.pp fmt (L.DocumentSymbol.yojson_of_t t)
    let equal = (=)
  end: Alcotest.TESTABLE with type t = L.DocumentSymbol.t)
  in
  let env = Test_env.get () in
  let textDocument = find_doc env "jms-0052" in
  let params = L.DocumentSymbolParams.create ~textDocument () in
  let `DocumentSymbol result = Option.get @@ Handlers.Document_symbols.compute params in
  Alcotest.(check int)
    ""
    29
    (List.length result)

let test_highlight () =
  let@ () = Reporter.easy_run in
  let _document_highlight =
  (module struct
    type t = L.DocumentHighlight.t
    let pp fmt t = Yojson.Safe.pp fmt (L.DocumentHighlight.yojson_of_t t)
    let equal = (=)
  end: Alcotest.TESTABLE with type t = L.DocumentHighlight.t)
  in
  let env = Test_env.get () in
  let textDocument = find_doc env "jms-0052" in
  let params = L.DocumentHighlightParams.create ~position: env.position ~textDocument () in
  let result = Handlers.Highlight.compute params in
  Alcotest.(check int)
    ""
    48
    (List.length @@ Option.get result)

let test_hover () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let hover =
  (module struct
    type t = L.Hover.t
    let pp fmt t = Yojson.Safe.pp fmt (L.Hover.yojson_of_t t)
    let equal = (=)
  end: Alcotest.TESTABLE with type t = L.Hover.t)
  in
  let textDocument = find_doc env "jms-0052" in
  let params = L.HoverParams.create ~position: {character = 12; line = 32;} ~textDocument () in
  let result = Handlers.Hover.compute params in
  let expected =
    Some
      (
        L.Hover.create
          ~contents: (
            `MarkupContent
              {
                kind = L.MarkupKind.Markdown;
                value = "In this section, we will walk through the installation of the Forester software.<omitted content: System requirements of Forester><omitted content: Installing the Forester software>"
              }
          )
          ()
      )
  in
  Alcotest.(check @@ option hover)
    ""
    expected
    result

let test_inlay_hint () =
  let@ () = Reporter.easy_run in
  let env = Test_env.get () in
  let _inlay_hint =
  (module struct
    type t = L.InlayHint.t
    let pp fmt t = Yojson.Safe.pp fmt (L.InlayHint.yojson_of_t t)
    let equal = (=)
  end: Alcotest.TESTABLE with type t = L.InlayHint.t)
  in
  let textDocument = find_doc env "jms-0052" in
  let params = L.InlayHintParams.create ~range: (L.Range.create ~start: env.position ~end_: env.position) ~textDocument () in
  let result = Handlers.Inlay_hint.compute params in
  Alcotest.(check int)
    ""
    25
    (List.length @@ Option.get result)

let test_workspace_symbols () =
  let@ () = Reporter.easy_run in
  let params = L.WorkspaceSymbolParams.create ~query: "foo" () in
  let result = Option.get @@ Handlers.Workspace_symbols.compute params in
  Alcotest.(check int)
    ""
    151
    (List.length result)

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
          (*0123456789012345678901234567890123456789012345678901234567890*)
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
  let doc
    =
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
  | Some (`Prev (_pred, _node)) -> Alcotest.fail "pred"
  | Some (`Node node) -> Alcotest.(check code_node) "node" (Ident ["ul"]) node.value
  | Some (`Parent _) -> Alcotest.fail "parent"

let test_enclosing_group ~forest () =
  (*                                         012345678901234*)
  let code = Result.get_ok @@ parse_string {|\foo{\bar{baz}}|} in
  let expanded =
    let@ () = Resolver.Scope.easy_run in
    Expand.expand ~forest code
  in
  let case_1 =
    let position = L.Position.{line = 0; character = 6} in
    match Analysis.get_enclosing_syn_group ~position expanded with
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
    match Analysis.get_enclosing_syn_group ~position expanded with
    | None -> Alcotest.fail "no enclosing group"
    | Some {value = (d, nodes); _} ->
      let open DSL.Syn in
      Alcotest.(check delim) "" Braces d;
      Alcotest.(check syn) "" [text "baz"] (strip_syn nodes)
  in
  List.iter Fun.id [case_1; case_2]

let test_asset_completion () =
  let@ () = Reporter.easy_run in
  let textDocument = find_doc (Test_env.get ()) "sterling-2024-cl-forester" in
  let position : L.Position.t = {line = 4; character = 27} in
  let params = L.CompletionParams.create ~position ~textDocument () in
  let completions =
    Handlers.Completion.compute params
    |> Option.map (fun (`CompletionList L.CompletionList.{items; _}) ->
        String.concat "" @@
          List.map
            (fun i -> Format.asprintf "%s" (Yojson.Safe.to_string @@ L.CompletionItem.yojson_of_t i))
            items
      )
  in
  Alcotest.(check @@ option string)
    ""
    (Some {|{"insertText":"assets/sterling-2024-cl-forester.pdf","kind":17,"label":"assets/sterling-2024-cl-forester.pdf"}|})
    completions

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  let config = {(Config_parser.parse_forest_config_file "forest.toml") with prefixes = ["jms"]} in
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
    "Test_lsp"
    [
      "Analysis",
      [
        "contains", `Quick, test_contains;
        "node_at", `Quick, test_node_at;
        "addr_at", `Quick, test_addr_at;
        "word_at", `Quick, test_word_at;
        "get_enclosing_group", `Quick, (test_enclosing_group ~forest);
      ];
      "Completion",
      [
        "find_with_pred", `Quick, test_find_with_prev;
        "node_at_pos_with_predecessor_or_parent", `Quick, test_node_at_pos_with_prev_or_parent;
        "asset_completion", `Quick, test_asset_completion;
      ];
      "Handlers",
      [
        "call hierarchy", `Quick, test_call_hierarchy;
        "change configuration", `Quick, test_change_configuration;
        "code actions", `Quick, test_code_actions;
        "code lens", `Quick, test_code_lens;
        "completion", `Quick, test_completion;
        "definitions", `Quick, test_definitions;
        "did change", `Quick, test_did_change;
        "did open", `Quick, test_did_open;
        "document link", `Quick, test_document_link;
        "document symbols", `Quick, test_document_symbols;
        "highlight", `Quick, test_highlight;
        "hover", `Quick, test_hover;
        "inlay hint", `Quick, test_inlay_hint;
        "workspace symbols", `Quick, test_workspace_symbols;
      ]
    ]
