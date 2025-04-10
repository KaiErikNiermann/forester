(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module Semantic_tokens = Semantic_tokens


val recv : unit -> Jsonrpc.Packet.t option
val send : Jsonrpc.Packet.t -> unit

val should_shutdown : unit -> bool
val initiate_shutdown : unit -> unit

val run : init: Lsp_state.state -> (unit -> 'a) -> 'a

module Request : sig
  type packed = Lsp.Client_request.packed
  type 'resp t = 'resp Lsp.Client_request.t

  val handle : Jsonrpc.Request.t -> Jsonrpc.Response.t
  val recv : unit -> (Jsonrpc.Id.t * packed) option
  val respond : Jsonrpc.Id.t -> 'resp t -> 'resp -> unit
end

module Notification : sig
  type t = Lsp.Client_notification.t

  val handle : Jsonrpc.Notification.t -> unit
  val recv : unit -> t option
end
