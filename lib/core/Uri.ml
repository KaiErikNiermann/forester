module Basics = struct
  type t = Iri.t

  let host = Iri.host
  let scheme = Iri.scheme
  let port = Iri.port
  let path_string iri = Iri.path_string iri

  let equal x y = Iri.equal ~normalize: true x y
  let compare x y = Iri.compare ~normalize: true x y
  let resolve = Iri.resolve ~normalize: true

  let clean iri = Iri.with_query iri (Iri.query iri)
  let hash (iri : t) = Hashtbl.hash (clean iri)


  let path_components x =
    match Iri.path x with
    | Absolute xs -> xs
    | Relative xs -> xs

  let with_path_components xs iri =
    Iri.normalize @@
    match Iri.path iri with
    | Absolute _ -> Iri.with_path iri (Absolute xs)
    | Relative _ -> Iri.with_path iri (Relative xs)

  let t = Repr.map Repr.string Iri.of_string Iri.to_string

  let pp (fmt : Format.formatter) (iri : t) =
    Format.fprintf fmt "%s" @@
      Iri.to_string ~pctencode: false iri

  let to_string = Iri.to_uri

  let of_string_exn : string -> t =
    Iri.of_string ~normalize: true

  let make ?scheme ?user ?host ?port ?path () =
    let path = Option.map (fun xs -> Iri.Absolute xs) path in
    Iri.normalize @@ Iri.iri ?scheme ?user ?host ?port ?path ()

  let relativise ~(host : string) iri =
    if scheme iri = "forest" && Iri.host iri = Some host then
      let (Iri.Absolute components | Iri.Relative components) = Iri.path iri in
      Iri.iri ~path: (Iri.Relative components) ()
    else
      iri

  let canonicalise iri = Iri.normalize iri
end

module Set = Set.Make(Basics)
module Map = Map.Make(Basics)
module Tbl = Hashtbl.Make(Basics)

include Basics
