(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* At the moment these tests are mainly stubs.*)

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
  config: Forester_compiler.Config.t;
  position: L.Position.t;
}

let find_doc (env : test_env) addr =
  let path =
    Eio.Path.native_exn @@
    Option.get @@
    Dir_scanner.find_tree env.dirs (URI_scheme.user_uri ~host: env.config.host addr)
  in
  ({uri = Lsp.Uri.of_path path}: L.TextDocumentIdentifier.t)

module Test_env = Algaeff.State.Make(struct type t = test_env end)

let find_tree addr =
  let env = Test_env.get () in
  let dirs = env.dirs in
  let host = env.config.host in
  Eio.Path.native_exn @@ Option.get @@ Dir_scanner.find_tree dirs (URI_scheme.user_uri ~host addr)

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
  let params = L.HoverParams.create ~position: env.position ~textDocument () in
  let result = Handlers.Hover.compute params in
  let expected =
    Some (L.Hover.create ~contents: (`MarkupContent {kind = L.MarkupKind.Markdown; value = "character: 1, line: 17."}) ())
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
  let located = Range.{value = (); loc} in
  let result = Analysis.contains ~position located in
  Alcotest.(check bool) "" true result

let test_node_at () =
  let position = L.Position.create ~character: 12 ~line: 0 in
  let code = Result.get_ok @@ parse_string_loc {|\import{asdf}|} in
  let result = Option.get @@ Analysis.node_at ~position code in
  Alcotest.(check code_node)
    ""
    (Import (Private, "asdf"))
    (Asai.Range.(result.value))

let test_addr_at () =
  let code = Result.get_ok @@ parse_string_loc {|\transclude{tfmt-0005}|} in
  let position = L.Position.create ~character: 13 ~line: 0 in
  let result = Option.get @@ Analysis.addr_at ~position code in
  Alcotest.(check string) "" "tfmt-0005" (Asai.Range.(result.value))

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
