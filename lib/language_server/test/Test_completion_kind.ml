(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
open Forester_core
module Completion_kind = Forester_lsp.Completion_kind

let check_kind label expected node =
  let actual = Completion_kind.kind_of_syn_node node in
  Alcotest.(check bool) label true (actual = expected)

let test_kind_of_syn_node_classifies_core_variants () =
  check_kind
    "fun -> function"
    (Some Forester_lsp.L.CompletionItemKind.Function)
    (Syn.Fun ([], []));
  check_kind
    "text -> text"
    (Some Forester_lsp.L.CompletionItemKind.Text)
    (Syn.Text "hello");
  check_kind
    "route asset -> file"
    (Some Forester_lsp.L.CompletionItemKind.File)
    Syn.Route_asset;
  check_kind
    "ref -> reference"
    (Some Forester_lsp.L.CompletionItemKind.Reference)
    Syn.Ref;
  check_kind "current tree -> none" None Syn.Current_tree

let test_insert_text_joins_path_segments () =
  Alcotest.(check string)
    "path joined with slash"
    "alpha/beta/gamma"
    (Completion_kind.insert_text ["alpha"; "beta"; "gamma"])

let () =
  Alcotest.run
    "Completion kind"
    [
      (
        "kind",
        [Alcotest.test_case
          "classifies syn nodes"
          `Quick
          test_kind_of_syn_node_classifies_core_variants;
        Alcotest.test_case
          "joins path segments"
          `Quick
          test_insert_text_joins_path_segments;
        ]
      );
    ]
