(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Make () = struct
  open Algaeff.Reader.Make(struct type t = URI.Set.t end)
  let add_seen_uri uri =
    scope @@ URI.Set.add uri

  let add_seen_uri_opt uri_opt kont =
    match uri_opt with
    | None -> kont ()
    | Some uri -> add_seen_uri uri kont

  let have_seen_uri uri =
    URI.Set.mem uri @@ read ()

  let have_seen_uri_opt uri_opt =
    match uri_opt with
    | None -> false
    | Some uri -> have_seen_uri uri

  let run k = run ~env: URI.Set.empty k
end
