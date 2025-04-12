(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open struct module L = Lsp.Types end

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
