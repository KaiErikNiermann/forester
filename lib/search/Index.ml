open Forester_core
open Forester_compiler
module T = Forester_core.Types
module Search_index = State.Search_index

module Set = Set.Make(String)
(* module Index = Map.Make(String) *)

let common_words =
  Set.of_list
    [
      "a";
      "and";
      "be";
      "have";
      "i";
      "in";
      "of";
      "that";
      "the";
      "to";
    ]

let tokenize string =
  Str.(split @@ regexp "[^a-zA-Z0-9]+") string
  |> List.filter_map
      (fun s ->
        let lower = String.lowercase_ascii s in
        if not @@ Set.mem lower common_words then
          Some (Stemming.stem lower)
        else
          None
      )

let rec tokenize_content : T.content -> _ = function
  | Content nodes ->
    List.concat_map
      (function
        | T.Text s
        | T.CDATA s ->
          tokenize s
        | T.Xml_elt {content; _} ->
          (* TODO: Consider tokenizing xml_qname *)
          tokenize_content content
        | T.Section {frontmatter; mainmatter; _} -> tokenize_frontmatter frontmatter @ tokenize_content mainmatter
        | T.Prim (_, content) -> tokenize_content content
        | T.Link {content; _} -> tokenize_content content
        | T.KaTeX (_, _) ->
          (* NOTE:
             In order to properly search math, we need to revamp the
             architecture and add more features...*)
          []
        | T.Transclude _
        | T.Contextual_number _
        | T.Results_of_query _
        | T.TeX_cs _
        | T.Img _
        | T.Artefact _
        | T.Iri _
        | T.Route_of_iri _
        | T.Datalog_script _
        | T.Results_of_datalog_query _ ->
          []
      )
      nodes

and tokenize_vertex = function
  | T.Iri_vertex _ -> []
  | T.Content_vertex c ->
    tokenize_content c

and tokenize_attribution
  : T.content T.attribution -> _
= function
  | {vertex; _} ->
    tokenize_vertex vertex

and tokenize_frontmatter
  : T.content T.frontmatter -> _
= function
  | {title;
    attributions;
    taxon;
    tags;
    metas;
    _;
  } ->
    List.concat
      [
        Option.value ~default: [] (Option.map tokenize_content title);
        Option.value ~default: [] (Option.map tokenize_content taxon);
        List.concat_map tokenize_attribution attributions;
        List.concat_map tokenize_vertex tags;
        List.concat_map (fun (s, c) -> tokenize s @ tokenize_content c) metas;
      ]

let tokenize_article : T.content T.article -> _ = function
  | {frontmatter; mainmatter; _} ->
    tokenize_frontmatter frontmatter @ tokenize_content mainmatter

let add
  : T.content T.article list ->
  iri list Search_index.t ->
  iri list Search_index.t
= fun articles index ->
  List.fold_left
    (fun acc article ->
      if Option.is_none T.(article.frontmatter.iri) then acc
      else
        let tokens = tokenize_article article in
        let iri = Option.get T.(article.frontmatter.iri) in
        List.fold_left
          (fun acc token ->
            match Search_index.find_opt token acc with
            | Some ids ->
              if List.nth ids (List.length ids - 1) = iri then acc
              else
                Search_index.add token (ids @ [iri]) acc
            | None -> Search_index.add token [iri] acc
          )
          acc
          tokens
    )
    index
    articles

let search
  : iri list Search_index.t -> string -> iri list list
= fun index term ->
  tokenize term
  |> List.concat_map
      (fun str ->
        match Search_index.find_opt str index with
        | Some ids ->
          [ids]
        | None -> []
      )

let index
  : T.content T.resource Forest.t -> iri list Search_index.t
= function
  | resources ->
    Forest.get_all_articles resources
    |> (fun articles -> add articles Search_index.empty)
