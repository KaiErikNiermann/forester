type t

val host : t -> string option
val scheme : t -> string
val path_string : t -> string
val path_components : t -> string list
val with_path_components : string list -> t -> t

val canonicalise : t -> t
val relativise : host: string -> t -> t
val resolve : base: t -> t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

(* TODO: get rid of *)
val clean : t -> t

val make :
  ?scheme: string ->
  ?user: string ->
  ?host: string ->
  ?port: int ->
  ?path: string list ->
  unit ->
  t

val t : t Repr.t
val pp : Format.formatter -> t -> unit
val to_string : t -> string
val of_string_exn : string -> t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
module Tbl : Hashtbl.S with type key = t