(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open State.Syntax

module Unit_map = URI.Map

open struct
  module R = Resolver
  module Sc = R.Scope
end

(* TODO: remove this in favor of https://github.com/ocaml/ocaml/pull/13760 *)
let edit_distance ~cutoff x y =
  let len_x, len_y = String.length x, String.length y in
  let grid = Array.make_matrix (len_x + 1) (len_y + 1) 0 in
  for i = 1 to len_x do
    grid.(i).(0) <- i;
  done;
  for j = 1 to len_y do
    grid.(0).(j) <- j;
  done;
  for j = 1 to len_y do
    for i = 1 to len_x do
      let cost = if x.[i - 1] = y.[j - 1] then 0 else 1 in
      let k = Int.min (grid.(i - 1).(j) + 1) (grid.(i).(j - 1) + 1) in
      grid.(i).(j) <- Int.min k (grid.(i - 1).(j - 1) + cost)
    done;
  done;
  let result = grid.(len_x).(len_y) in
  if result > cutoff then None
  else
    Some result

let suggestions
    ?prefix
    ~(cutoff : int)
    (p : Trie.bwd_path)
    : ('data, 'tag) Trie.t -> ('data, int) Trie.t
  =
  let compare p d =
    edit_distance ~cutoff (String.concat "" (Bwd.to_list p)) (String.concat "" (Bwd.to_list d))
  in
  Trie.filter_map
    ?prefix
    (fun q (data, _) ->
      match compare p q with
      | Some i ->
        if i > cutoff then
          None
        else
          (Some (data, i))
      | None -> None
    )

let suggestions path visible =
  suggestions ~cutoff: 2 (Bwd.of_list path) visible
  |> Trie.to_seq
  |> Seq.map (fun (path, (data, distance)) -> (path, data, distance))
  |> List.of_seq
  |> List.sort (fun (_, _, a) (_, _, b) -> Int.compare a b)

let create_suggestions path =
  let visible = Sc.get_visible () in
  let suggestions = suggestions path visible in
  let extra_remarks =
    if List.length suggestions > 0 then
      let (path, data, _) = List.hd suggestions in
      let location_hint =
        match data with
        | R.P.Term ({loc = Some loc; _} :: _) ->
          begin
            match Range.view loc with
            | `End_of_file {source; _}
            | `Range ({source; _}, _) ->
              match Range.title source with
              | Some string ->
                [Asai.Diagnostic.loctextf "defined in %s" string]
              | _ -> []
          end
        | _ -> []
      in
      [Asai.Diagnostic.loctextf "Did you mean %a?" Sc.pp_path path] @ location_hint
    else []
  in
  extra_remarks

module Builtins = struct

  let create_sym path =
    let sym = Symbol.named path in
    sym,
    fun () ->
      Sc.include_singleton path @@
        (Term [Range.locate_opt None (Syn.Sym sym)], None)

  let register_builtins builtins =
    Sc.include_subtree [] @@
    Yuujinchou.Trie.of_seq @@
    let@ path, node = Seq.map @~ List.to_seq builtins in
    path, (R.P.Term [Range.locate_opt None node], None)

  module Transclude = struct
    let expanded_sym, alloc_expanded = create_sym ["transclude"; "expanded"]
    let show_heading_sym, alloc_show_heading = create_sym ["transclude"; "heading"]
    let toc_sym, alloc_toc = create_sym ["transclude"; "toc"]
    let numbered_sym, alloc_numbered = create_sym ["transclude"; "numbered"]
    let show_metadata_sym, alloc_show_metadata = create_sym ["transclude"; "metadata"]
  end
end

let rec expand_method_calls (base : Syn.t) : Code.t -> Syn.t * Code.t = function
  | {value = Hash_ident x; loc} :: rest ->
    let base = [Range.{value = Syn.Call (base, x); loc}] in
    expand_method_calls base rest
  | rest -> base, rest

type 'a Effect.t += Entered_range : Range.t option -> unit Effect.t

let entered_range (loc : Range.t option) : unit =
  Effect.perform @@ Entered_range loc

let rec expand_eff ~(forest : State.t) : Code.t -> Syn.t = function
  | [] -> []
  | node :: rest ->
    entered_range node.loc;
    match node.value with
    | Hash_ident x | Text x ->
      {node with value = Text x} :: expand_eff ~forest rest
    | Verbatim x ->
      {node with value = Verbatim x} :: expand_eff ~forest rest
    | Namespace (path, body) ->
      let result =
        let@ () = Sc.section path in
        expand_eff ~forest body
      in
      result @ expand_eff ~forest rest
    | Open path ->
      Sc.modify_visible @@
        R.Lang.union
          [
            R.Lang.all;
            R.Lang.renaming path []
          ];
      expand_eff ~forest rest
    | Group (Squares, x) ->
      begin
        match x with
        | [{value = Group (Squares, y); loc = yloc}] ->
          entered_range yloc;
          let y = expand_eff ~forest y in
          {node with value = Link {dest = y; title = None}} :: expand_eff ~forest rest
        | _ ->
          let x = expand_eff ~forest x in
          begin
            match rest with
            | {value = Group (Parens, y); loc = yloc} :: rest ->
              entered_range yloc;
              let y = expand_eff ~forest y in
              (* TODO: merge the ranges *)
              {node with value = Link {dest = y; title = Some x}} :: expand_eff ~forest rest
            | _ -> {node with value = Group (Squares, x)} :: expand_eff ~forest rest
          end
      end
    | Group (d, x) ->
      let x = expand_eff ~forest x in
      {node with value = Group (d, x)} :: expand_eff ~forest rest
    | Subtree (addr, nodes) ->
      let identity =
        match addr with
        | Some addr -> Tree.URI (URI_scheme.named_uri ~base: forest.config.url addr)
        | None -> Tree.Anonymous
      in
      let nodes =
        let@ () = Sc.section [] in
        expand_eff ~forest nodes
      in
      {node with value = Syn.Subtree (addr, nodes)} :: expand_eff ~forest rest
    | Math (m, x) ->
      let x = expand_eff ~forest x in
      {node with value = Math (m, x)} :: expand_eff ~forest rest
    | Ident path ->
      let out, rest = expand_method_calls (expand_ident node.loc path) rest in
      out @ expand_eff ~forest rest
    | Xml_ident (prefix, uname) ->
      let qname = expand_xml_ident node.loc (prefix, uname) in
      let attrs, rest = get_xml_attrs ~forest [] rest in
      let arg_opt, rest = get_arg_opt ~forest rest in
      {node with value = Xml_tag (qname, attrs, Option.value ~default: [] arg_opt)} :: expand_eff ~forest rest
    | Scope body ->
      let body =
        let@ () = Sc.section [] in
        expand_eff ~forest body
      in
      body @ expand_eff ~forest rest
    | Alloc x ->
      let symbol = Symbol.named x in
      Sc.include_singleton x @@ (Term [Range.locate_opt node.loc (Syn.Sym symbol)], node.loc);
      expand_eff ~forest rest
    | Put (k, v) ->
      let k = expand_ident node.loc k in
      let v = expand_eff ~forest v in
      (* TODO: merge locations! the resulting location is narrowed to the 'put' node, and therefore breaks the nesting of locations. That could lead to trouble in the future. *)
      [{node with value = Put (k, v, expand_eff ~forest rest)}]
    | Default (k, v) ->
      let k = expand_ident node.loc k in
      let v = expand_eff ~forest v in
      (* TODO: merge locations! the resulting location is narrowed to the 'put' node, and therefore breaks the nesting of locations. That could lead to trouble in the future. *)
      [{node with value = Default (k, v, expand_eff ~forest rest)}]
    | Get k ->
      let k = expand_ident node.loc k in
      {node with value = Get k} :: expand_eff ~forest rest
    | Dx_var name ->
      {node with value = Dx_var name} :: expand_eff ~forest rest
    | Dx_const_content x ->
      let x = expand_eff ~forest x in
      {node with value = Dx_const (`Content, x)} :: expand_eff ~forest rest
    | Dx_const_uri x ->
      let x = expand_eff ~forest x in
      {node with value = Dx_const (`Uri, x)} :: expand_eff ~forest rest
    | Dx_prop (rel, args) ->
      let rel = expand_eff ~forest rel in
      let args = List.map (expand_eff ~forest) args in
      {node with value = Dx_prop (rel, args)} :: expand_eff ~forest rest
    | Dx_query (var, pos, neg) ->
      let pos = List.map (expand_eff ~forest) pos in
      let neg = List.map (expand_eff ~forest) neg in
      {node with value = Dx_query (var, pos, neg)} :: expand_eff ~forest rest
    | Dx_sequent (concl, prems) ->
      let concl = expand_eff ~forest concl in
      let prems = List.map (expand_eff ~forest) prems in
      {node with value = Dx_sequent (concl, prems)} :: expand_eff ~forest rest
    | Fun (xs, body) ->
      let lam = expand_lambda ~forest node.loc (xs, body) in
      lam :: expand_eff ~forest rest
    | Let (x, ys, def) ->
      let lam = expand_lambda ~forest node.loc (ys, def) in
      let@ () = Sc.section [] in
      Sc.import_singleton x @@ (Term [lam], node.loc);
      expand_eff ~forest rest
    | Def (x, ys, def) ->
      let lam = expand_lambda ~forest node.loc (ys, def) in
      Sc.include_singleton x @@ (Term [lam], node.loc);
      expand_eff ~forest rest
    | Decl_xmlns (prefix, xmlns) ->
      let path = ["xmlns"; prefix] in
      Sc.include_singleton path @@ (Xmlns {prefix; xmlns}, node.loc);
      expand_eff ~forest rest
    | Object {self; methods} ->
      let self, methods =
        let@ () = Sc.section [] in
        let sym = Symbol.fresh () in
        let var = Range.{value = Syn.Var sym; loc = node.loc} in (* TODO: correct the location *)
        begin
          let@ self = Option.iter @~ self in
          Sc.import_singleton self @@ (R.P.Term [var], node.loc) (* TODO: correct the location*)
        end;
        sym, List.map (expand_method ~forest) methods
      in
      {node with value = Object {self; methods}} :: expand_eff ~forest rest
    | Patch {obj; self; methods} ->
      let obj = expand_eff ~forest obj in
      let self, super, methods =
        let@ () = Sc.section [] in
        let self_sym = Symbol.fresh () in
        let super_sym = Symbol.fresh () in
        let self_var = Range.locate_opt None @@ Syn.Var self_sym in
        let super_var = Range.locate_opt None @@ Syn.Var super_sym in
        begin
          let@ self = Option.iter @~ self in
          Sc.import_singleton self @@ (Term [self_var], node.loc);
          (* TODO: correct location*)
          Sc.import_singleton (self @ ["super"]) @@ (Term [super_var], node.loc)
        end;
        self_sym, super_sym, List.map (expand_method ~forest) methods
      in
      let patched = Syn.Patch {obj; self; super; methods} in
      {node with value = patched} :: expand_eff ~forest rest
    | Call (obj, meth) ->
      let obj = expand_eff ~forest obj in
      {node with value = Call (obj, meth)} :: expand_eff ~forest rest
    | Import (vis, dep) ->
      let dep_uri = URI_scheme.named_uri ~base: forest.config.url dep in
      begin
        match forest./{dep_uri} with
        | None ->
          Reporter.emit ?loc: node.loc (Import_not_found dep_uri)
        | Some tree ->
          begin
            match vis with
            | Public -> Sc.include_subtree [] tree
            | Private -> Sc.import_subtree [] tree
          end
      end;
      expand_eff ~forest rest
    | Comment _ | Error _ ->
      ignore @@ assert false;
      expand_eff ~forest rest

and get_xml_attrs ~forest acc = function
  | {value = Group (Squares, [{value = Text key; loc = loc1}]); _} :: {value = Group (Braces, value); loc = loc2} :: rest ->
    entered_range loc1;
    entered_range loc2;
    let qname = expand_xml_ident loc1 @@ Forester_xml_names.split_xml_qname key in
    let value = expand_eff ~forest value in
    get_xml_attrs ~forest (acc @ [qname, value]) rest
  | rest -> acc, rest

and get_arg_opt ~forest : Code.t -> _ = function
  | {value = Group (Braces, arg); loc} :: rest ->
    entered_range loc;
    Some (expand_eff ~forest arg), rest
  | rest -> None, rest

and expand_ident loc path =
  match Sc.resolve path, path with
  | None, [name] ->
    begin
      match TeX_cs.parse name with
      | None ->
        let extra_remarks = create_suggestions path in
        let visible = Sc.get_visible () in
        Reporter.fatal
          ?loc
          ~extra_remarks
          (Expansion_error (`Resolution_error (visible, path)))
      | Some (cs, rest) ->
        let rest = match rest with "" -> [] | _ -> [Range.{value = Syn.Text rest; loc}] in
        Range.{value = Syn.TeX_cs cs; loc} :: rest
    end
  | None, _ ->
    let extra_remarks = create_suggestions path in
    let visible = Sc.get_visible () in
    Reporter.fatal ?loc ~extra_remarks (Expansion_error (`Resolution_error (visible, path)))
  | Some (Term x, _), _ ->
    let relocate Range.{value; _} = Range.{value; loc} in
    List.map relocate x
  | Some (Xmlns {xmlns; prefix}, _), _ ->
    let visible = Sc.get_visible () in
    Reporter.fatal
      ?loc
      ~extra_remarks: [
        Asai.Diagnostic.loctextf
          "path %a resolved to xmlns:%s=\"%s\" instead of term"
          Sc.pp_path
          path
          xmlns
          prefix
      ]
      (Expansion_error (`Resolution_error (visible, path)))

and expand_xml_ident loc (prefix, uname) : Types.xml_qname =
  match prefix with
  | None -> {xmlns = None; prefix = ""; uname}
  | Some prefix ->
    match Sc.resolve ["xmlns"; prefix] with
    | Some (Xmlns {xmlns; prefix}, _) ->
      {xmlns = Some xmlns; prefix = prefix; uname}
    | _ ->
      Reporter.fatal
        ?loc
        (Expansion_error `Xmlns_error)
        ~extra_remarks: [
          Asai.Diagnostic.loctextf
            "expected path `%s` to resolve to xmlns"
            prefix
        ]

and expand_method ~forest (key, body) =
  key, expand_eff ~forest body

and expand_lambda ~forest loc (xs, body) =
  let@ () = Sc.section [] in
  let syms =
    let@ strategy, x = List.map @~ xs in
    let sym = Symbol.named x in
    let var = Range.locate_opt None @@ Syn.Var sym in
    Sc.import_singleton x @@ (Term [var], loc);
    strategy, sym
  in
  Range.{value = Syn.Fun (syms, expand_eff ~forest body); loc}

let ignore_entered_range f x =
  let open Effect.Deep in
  try_with
    f
    x
    {
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Entered_range _ ->
          Option.some @@ fun (k : (a, _) continuation) ->
          continue k ()
        | _ -> None
    }

let expand ~forest (xs : Code.t) : Syn.t =
  ignore_entered_range (expand_eff ~forest) xs

let builtins = [
  ["p"], Syn.Prim `P;
  ["em"], Syn.Prim `Em;
  ["strong"], Syn.Prim `Strong;
  ["li"], Syn.Prim `Li;
  ["ol"], Syn.Prim `Ol;
  ["ul"], Syn.Prim `Ul;
  ["code"], Syn.Prim `Code;
  ["blockquote"], Syn.Prim `Blockquote;
  ["pre"], Syn.Prim `Pre;
  ["figure"], Syn.Prim `Figure;
  ["figcaption"], Syn.Prim `Figcaption;
  ["transclude"], Syn.Transclude;
  ["tex"], Syn.Embed_tex;
  ["ref"], Syn.Ref;
  ["title"], Syn.Title;
  ["taxon"], Syn.Taxon;
  ["date"], Syn.Date;
  ["meta"], Syn.Meta;
  ["author"], Syn.Attribution (Author, `Uri);
  ["author"; "literal"], Syn.Attribution (Author, `Content);
  ["contributor"], Syn.Attribution (Contributor, `Uri);
  ["contributor"; "literal"], Syn.Attribution (Contributor, `Content);
  ["parent"], Syn.Parent;
  ["number"], Syn.Number;
  ["tag"], Syn.Tag `Content;
  ["query"], Syn.Results_of_query;
  ["rel"; "has-tag"], Syn.Text Builtin_relation.has_tag;
  ["rel"; "has-taxon"], Syn.Text Builtin_relation.has_taxon;
  ["rel"; "has-author"], Syn.Text Builtin_relation.has_author;
  ["rel"; "has-direct-contributor"], Syn.Text Builtin_relation.has_direct_contributor;
  ["rel"; "transcludes"], Syn.Text Builtin_relation.transcludes;
  ["rel"; "transcludes"; "transitive-closure"], Syn.Text Builtin_relation.transcludes_tc;
  ["rel"; "transcludes"; "reflexive-transitive-closure"], Syn.Text Builtin_relation.transcludes_rtc;
  ["rel"; "links-to"], Syn.Text Builtin_relation.links_to;
  ["rel"; "is-reference"], Syn.Text Builtin_relation.is_reference;
  ["rel"; "is-person"], Syn.Text Builtin_relation.is_person;
  ["rel"; "is-node"], Syn.Text Builtin_relation.is_node;
  ["rel"; "in-host"], Syn.Text Builtin_relation.in_host;
  ["execute"], Syn.Dx_execute;
  ["route-asset"], Syn.Route_asset;
  ["syndicate-query-as-json-blob"], Syn.Syndicate_query_as_json_blob;
  ["syndicate-current-tree-as-atom-feed"], Syn.Syndicate_current_tree_as_atom_feed;
  ["current-tree"], Syn.Current_tree;
]

let expand_tree_inner ~forest (code : Tree.code) : Tree.syn =
  let trace k =
    match Tree.identity_to_uri code.identity with
    | None -> k ()
    | Some uri ->
      let@ () = Reporter.tracef "when expanding tree %s" (URI.to_string uri) in
      k ()
  in
  let@ () = trace in
  let@ () = Sc.section [] in
  let nodes = expand_eff ~forest code.nodes in
  let exports = Sc.get_export () in
  Tree.{nodes; identity = code.identity; code; units = exports}

let expand_tree ~(forest : State.t) (code : Tree.code) : Tree.syn * Reporter.Message.t Asai.Diagnostic.t list =
  let diagnostics = ref [] in
  let emit d = diagnostics := d :: !diagnostics in
  let fatal d =
    emit d;
    Tree.{
      nodes = [];
      identity = code.identity;
      code = code;
      units = Trie.empty;
    },
    !diagnostics
  in
  Reporter.run ~emit ~fatal @@ fun () ->
  let@ () = Sc.easy_run in
  Builtins.register_builtins builtins;
  Builtins.Transclude.alloc_expanded ();
  Builtins.Transclude.alloc_show_heading ();
  Builtins.Transclude.alloc_toc ();
  Builtins.Transclude.alloc_numbered ();
  Builtins.Transclude.alloc_show_metadata ();
  let expanded_tree = ignore_entered_range (expand_tree_inner ~forest) code in
  expanded_tree, !diagnostics
