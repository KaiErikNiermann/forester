(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_test

let native_paths seq =
  seq |> List.of_seq |> List.map Eio.Path.native_exn |> List.sort String.compare

let absolute_dir env path =
  Eio.Path.(env#fs / Unix.realpath (Eio.Path.native_exn path))

let test_scan_directories_includes_markdown_sources ~env () =
  with_open_tmp_dir ~env @@ fun tmp ->
  let open Eio.Path in
  let tree_dir = tmp / "trees" in
  mkdir ~perm: 0o755 tree_dir;
  save ~create: (`Exclusive 0o644) (tree_dir / "index.tree") "";
  save ~create: (`Exclusive 0o644) (tree_dir / "guide.md") "# guide\n";
  save
    ~create: (`Exclusive 0o644)
    (tree_dir / "appendix.MARKDOWN")
    "# appendix\n";
  save ~create: (`Exclusive 0o644) (tree_dir / ".hidden.md") "# hidden\n";
  save ~create: (`Exclusive 0o644) (tree_dir / "notes.txt") "ignore\n";
  let tree_dir = absolute_dir env tree_dir in
  let scanned = Dir_scanner.scan_directories [tree_dir] |> native_paths in
  let expected =
    [native_exn (tree_dir / "appendix.MARKDOWN");
    native_exn (tree_dir / "guide.md");
    native_exn (tree_dir / "index.tree");
    ]
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "markdown and tree files are scanned"
    expected
    scanned

let test_find_tree_resolves_markdown_basenames ~env () =
  with_open_tmp_dir ~env @@ fun tmp ->
  let open Eio.Path in
  let tree_dir = tmp / "trees" in
  mkdir ~perm: 0o755 tree_dir;
  save ~create: (`Exclusive 0o644) (tree_dir / "chapter.md") "# chapter\n";
  save
    ~create: (`Exclusive 0o644)
    (tree_dir / "appendix.markdown")
    "# appendix\n";
  let tree_dir = absolute_dir env tree_dir in
  let config = Config.default () in
  let chapter_uri = URI_scheme.named_uri ~base: config.url "chapter" in
  let appendix_uri = URI_scheme.named_uri ~base: config.url "appendix" in
  let chapter_path =
    Dir_scanner.find_tree [tree_dir] chapter_uri
    |> Option.map Eio.Path.native_exn
  in
  let appendix_path =
    Dir_scanner.find_tree [tree_dir] appendix_uri
    |> Option.map Eio.Path.native_exn
  in
  Alcotest.(check (option string))
    "chapter md resolved"
    (Some (native_exn (tree_dir / "chapter.md")))
    chapter_path;
  Alcotest.(check (option string))
    "appendix markdown resolved"
    (Some (native_exn (tree_dir / "appendix.markdown")))
    appendix_path

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  run
    "Dir_scanner"
    [
      (
        "scan",
        [test_case
          "scan_directories includes markdown sources"
          `Quick
          (test_scan_directories_includes_markdown_sources ~env);
        test_case
          "find_tree resolves markdown basenames"
          `Quick
          (test_find_tree_resolves_markdown_basenames ~env);
        ]
      );
    ]
