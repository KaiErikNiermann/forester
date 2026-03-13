(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type env = Eio_unix.Stdenv.base

val latex_to_svg :
  env: env ->
  settings: Config.latex_settings ->
  ?loc: Asai.Range.t ->
  string ->
  string
