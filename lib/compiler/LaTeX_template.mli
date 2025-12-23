(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

val pp :
	Format.formatter ->
	document_class: string ->
	document_class_options: string list ->
	preamble: string ->
	body: string ->
	unit

val to_string :
	document_class: string ->
	document_class_options: string list ->
	preamble: string ->
	body: string ->
	string
