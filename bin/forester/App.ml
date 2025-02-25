(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Brr

let htmx = Jv.get Jv.global "htmx"

let ajax o = Jv.call o "ajax"
let on_load o = Jv.call o "onLoad"

let katex = Jv.get Jv.global "katex"

type _katex_config = {
  displayMode: Jv.t;
  output: Jstr.t;
  leqno: Jv.t;
  fleqn: Jv.t;
  throwOnError: Jv.t;
  errorColor: Jstr.t;
  minRuleThickness: Jv.t;
  colorIsTextColor: Jv.t;
  maxSize: Jv.t;
  maxExpand: Jv.t;
  strict: Jv.t;
  trust: Jv.t; (* Boolean or function*)
  globalGroup: Jv.t
}

let trust_config =
  Jv.obj
    [|
      "trust",
      Jv.of_bool true
    |]

let render o = Jv.call o "render"

let () =
  ignore @@
    on_load
      htmx
      [|
        let f = fun root ->
          El.fold_find_by_selector
            ~root
            (fun elt _ ->
              ignore @@
                render
                  katex
                  [|
                    Jv.get (El.to_jv elt) "textContent";
                    El.to_jv elt;
                    trust_config;
                  |]
            )
            (Jstr.v ".math")
            ()
        in
        Jv.repr f
      |]

(* let () = ignore @@ Jv.call htmx "logAll" [||] *)

let () =
  ignore @@
    Ev.listen
      Ev.keydown
      (fun e ->
        let ev = Ev.as_type e in
        Ev.(
          if Keyboard.ctrl_key ev && Keyboard.key ev = Jstr.v "k" then
            begin
              Console.log ["hello"];
              prevent_default e;
              ignore @@
                ajax
                  htmx
                  [|
                    Jv.of_string "GET";
                    Jv.of_string "/searchmenu";
                    Jv.of_string "#modal-container";
                  |]
            end
        )
      )
      (Document.as_target G.document)
