(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_prelude
open Forester_frontend

open struct module T = Types end

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  run
    "Test_incremental_compilation"
    [
      "",
      []
      (* [test_case "separate" `Quick test_separate] *)
    ]
