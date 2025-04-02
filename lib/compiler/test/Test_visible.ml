(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_test
open Forester_lsp

let test_visible () =
  let code =
    Result.get_ok @@
      parse_string_loc
        {|
\def\greet[name]{Hello, \name!}
\p{\greet{Jon}}
    |}
  in
  let result =
    Trie.to_seq @@
      Analysis.get_visible ~position: {line = 2; character = 5;} code
  in
  Logs.debug (fun m -> m "result:");
  Seq.iter
    (fun (path, (data, tag)) ->
      Logs.debug (fun m ->
        m
          "%a: (%a,%a)"
          Trie.pp_path
          path
          Resolver.P.pp_data
          data
          (Format.pp_print_option Asai.Range.dump)
          tag
      )
    )
    result
(* Alcotest.(check @@ (seq (pair path (pair data tag)))) *)
(*   "" *)
(*   (List.to_seq []) *)
(*   result *)

let () =
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  (* let@ env = Eio_main.run in *)
  test_visible ()
