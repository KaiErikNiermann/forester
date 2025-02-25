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
                  |]
            )
            (Jstr.v ".math")
            ()
        in
        Jv.repr f
      |]

let () =
  ignore @@
    Ev.listen
      Ev.keydown
      (fun e ->
        let ev = Ev.as_type e in
        Ev.(
          if Keyboard.ctrl_key ev && Keyboard.key ev = Jstr.v "k" then
            begin
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
