module Basics = struct
  type t = {
    scheme: string option;
    userinfo: string option;
    host: string option;
    port: int option;
    path: string;
    fragment: string option;
  }

  let hydrate {scheme; userinfo; host; port; path; fragment} =
    Uri.make ?scheme ?userinfo ?host ?port ~path ?fragment ()

  let dehydrate x = {
    scheme = Uri.scheme x;
    userinfo = Uri.userinfo x;
    host = Uri.host x;
    port = Uri.port x;
    path = Uri.path x;
    fragment = Uri.fragment x
  }

  let host x = x.host
  let scheme x = x.scheme
  let port x = x.port

  let path_components x =
    List.filter (function "" -> false | _ -> true) @@
    String.split_on_char '/' @@ x.path

  let path_string x =
    String.concat "/" @@ path_components x

  let equal = (=)
  let compare = compare

  let resolve ~base x =
    dehydrate @@ Uri.resolve "" (hydrate base) (hydrate x)

  let canonicalise iri = dehydrate @@ Uri.canonicalize @@ hydrate iri
  let hash (iri : t) = Hashtbl.hash iri

  let with_path_components xs iri =
    dehydrate @@
    Uri.canonicalize @@
    Uri.with_path (hydrate iri) @@ String.concat "/" xs

  let t = Repr.map Repr.string (Fun.compose dehydrate Uri.of_string) (Fun.compose Uri.to_string hydrate)

  let pp (fmt : Format.formatter) (iri : t) =
    Format.fprintf fmt "%s" @@
    Uri.to_string @@ hydrate iri (* wanted it not pct-encoded, but we'll see*)

  let to_string x = Uri.to_string @@ hydrate x

  let of_string_exn str =
    dehydrate @@ Uri.canonicalize @@ Uri.of_string str

  let make ?scheme ?user ?host ?port ?path () =
    let path = Option.map (String.concat "/") path in
    dehydrate @@ Uri.canonicalize @@ Uri.make ?scheme ?userinfo: user ?host ?port ?path ()

  let relativise ~(host : string) iri =
    if scheme iri = Some "forest" && iri.host = Some host then
      dehydrate @@ Uri.make ?scheme: (scheme iri) ~path: iri.path ()
    else
      iri
end

module Set = Set.Make(Basics)
module Map = Map.Make(Basics)
module Tbl = Hashtbl.Make(Basics)

include Basics
