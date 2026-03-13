(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

let test_split_addr_1 () =
  let uri = URI.of_string_exn "forest://test/foo-bar" in
  Alcotest.(check @@ option @@ pair (option string) int)
    ""
    (URI_scheme.split_addr uri)
    None

let test_split_addr_2 () =
  let uri = URI.of_string_exn "forest://test/foo-1234" in
  Alcotest.(check @@ option @@ pair (option string) int)
    ""
    (URI_scheme.split_addr uri)
    (Some (Some "foo", 49360))

let test_split_addr_3 () =
  let uri = URI.of_string_exn "forest://test/1976-02-11" in
  Alcotest.(check @@ option @@ pair (option string) int)
    ""
    (URI_scheme.split_addr uri)
    None

let test_split_addr_4 () =
  let uri = URI.of_string_exn "forest://test/asdf" in
  Alcotest.(check @@ option @@ pair (option string) int)
    ""
    (URI_scheme.split_addr uri)
    None

let test_split_addr_5 () =
  let uri = URI.of_string_exn "forest://test/ASDF" in
  Alcotest.(check @@ option @@ pair (option string) int)
    ""
    (URI_scheme.split_addr uri)
    (Some (None, 503331))

let () =
  let open Alcotest in
  run
    "Test_uri_util"
    [
      "split_addr",
      [test_case "split_addr" `Quick test_split_addr_1;
      test_case "split_addr" `Quick test_split_addr_2;
      test_case "split_addr" `Quick test_split_addr_3;
      test_case "split_addr" `Quick test_split_addr_4;
      test_case "split_addr" `Quick test_split_addr_5;
      ]
    ]
