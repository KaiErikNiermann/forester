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

module Builtins = struct
  module Transclude = struct
    let expanded_sym = Symbol.named ["transclude"; "expanded"]
    let show_heading_sym = Symbol.named ["transclude"; "heading"]
    let toc_sym = Symbol.named ["transclude"; "toc"]
    let numbered_sym = Symbol.named ["transclude"; "numbered"]
    let show_metadata_sym = Symbol.named ["transclude"; "metadata"]
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
      let methods =
        let@ () = Sc.section [] in
        begin
          let@ self = Option.iter @~ self in
          let var = Range.{value = Syn.Var self; loc = node.loc} in (* TODO: correct the location *)
          Sc.import_singleton [self] @@ (Term [var], node.loc) (* TODO: correct the location*)
        end;
        List.map (expand_method ~forest) methods
      in
      {node with value = Object {self; methods}} :: expand_eff ~forest rest
    | Patch {obj; self; super; methods} ->
      let obj = expand_eff ~forest obj in
      let methods =
        let@ () = Sc.section [] in
        begin
          let@ self = Option.iter @~ self in
          let self_var = Range.locate_opt None @@ Syn.Var self in
          Sc.import_singleton [self] @@ (Term [self_var], node.loc);
          let@ super = Option.iter @~ super in
          let super_var = Range.locate_opt None @@ Syn.Var super in
          Sc.import_singleton [super] @@ (Term [super_var], node.loc)
        end;
        List.map (expand_method ~forest) methods
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
  match Sc.resolve path with
  | None ->
    let visible = Sc.get_visible () in
    [Range.{value = Syn.Unresolved_ident (visible, path); loc}]
  | Some (Term x, _) ->
    let relocate Range.{value; _} = Range.{value; loc} in
    List.map relocate x
  | Some (Xmlns {xmlns; prefix}, _) ->
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
      (Unresolved_identifier (visible, path)) (* TODO: This should be perhaps a different error *)

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
        (Unresolved_xmlns prefix)
        ~extra_remarks: [
          Asai.Diagnostic.loctextf
            "expected path `%s` to resolve to xmlns"
            prefix;
          Asai.Diagnostic.loctextf "You may fix this by defining an XML namespace:@.   \\xmlns:%s{...}" prefix;
        ]

and expand_method ~forest (key, body) =
  key, expand_eff ~forest body

and expand_lambda ~forest loc (xs, body) =
  let@ () = Sc.section [] in
  let xs =
    let@ strategy, x = List.map @~ xs in
    let var = Range.locate_opt None @@ Syn.Var x in
    Sc.import_singleton [x] @@ (Term [var], loc);
    strategy, x
  in
  Range.{value = Syn.Fun (xs, expand_eff ~forest body); loc}

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

(* Feel free to extend this *)
let tex_builtin_words =
  List.to_seq ["left"; "right"; "big"; "bigr"; "Big"; "Bigr"; "bigg"; "biggr"; "Bigg"; "Biggr"; "bigl"; "Bigl"; "biggl"; "Biggl"; "mathrlap"; "mathllap"; "mathclap"; "rlap"; "llap"; "ulap"; "dlap"; "infty"; "infinity"; "lbrace"; "rbrace"; "llbracket"; "rrbracket"; "lvert"; "lVert"; "rvert"; "rVert"; "vert"; "Vert"; "setminus"; "backslash"; "smallsetminus"; "sslash"; "lfloor"; "lceil"; "lmoustache"; "lang"; "langle"; "llangle"; "rfloor"; "rceil"; "rmoustache"; "rang"; "rangle"; "rrangle"; "uparrow"; "downarrow"; "updownarrow"; "prime"; "alpha"; "beta"; "gamma"; "delta"; "zeta"; "eta"; "theta"; "iota"; "kappa"; "lambda"; "mu"; "nu"; "xi"; "pi"; "rho"; "sigma"; "tau"; "upsilon"; "chi"; "psi"; "omega"; "backepsilon"; "varkappa"; "varpi"; "varrho"; "varsigma"; "vartheta"; "varepsilon"; "phi"; "varphi"; "arccos"; "arcsin"; "arctan"; "arg"; "cos"; "cosh"; "cot"; "coth"; "csc"; "deg"; "dim"; "exp"; "hom"; "ker"; "lg"; "ln"; "log"; "sec"; "sin"; "sinh"; "tan"; "tanh"; "det"; "gcd"; "inf"; "lim"; "liminf"; "limsup"; "max"; "min"; "Pr"; "sup"; "omicron"; "epsilon"; "cdot"; "Alpha"; "Beta"; "Delta"; "Gamma"; "digamma"; "Lambda"; "Pi"; "Phi"; "Psi"; "Sigma"; "Theta"; "Xi"; "Zeta"; "Eta"; "Iota"; "Kappa"; "Mu"; "Nu"; "Rho"; "Tau"; "mho"; "Omega"; "Upsilon"; "Upsi"; "iff"; "Longleftrightarrow"; "Leftrightarrow"; "impliedby"; "Leftarrow"; "implies"; "Rightarrow"; "hookleftarrow"; "embedsin"; "hookrightarrow"; "longleftarrow"; "longrightarrow"; "leftarrow"; "to"; "rightarrow"; "leftrightarrow"; "mapsto"; "map"; "nearrow"; "nearr"; "nwarrow"; "nwarr"; "searrow"; "searr"; "swarrow"; "swarr"; "neArrow"; "neArr"; "nwArrow"; "nwArr"; "seArrow"; "seArr"; "swArrow"; "swArr"; "darr"; "Downarrow"; "uparr"; "Uparrow"; "downuparrow"; "duparr"; "updarr"; "Updownarrow"; "leftsquigarrow"; "rightsquigarrow"; "dashleftarrow"; "dashrightarrow"; "curvearrowbotright"; "righttoleftarrow"; "lefttorightarrow"; "leftrightsquigarrow"; "upuparrows"; "rightleftarrows"; "rightrightarrows"; "curvearrowleft"; "curvearrowright"; "downdownarrows"; "leftarrowtail"; "rightarrowtail"; "leftleftarrows"; "leftrightarrows"; "Lleftarrow"; "Rrightarrow"; "looparrowleft"; "looparrowright"; "Lsh"; "Rsh"; "circlearrowleft"; "circlearrowright"; "twoheadleftarrow"; "twoheadrightarrow"; "nLeftarrow"; "nleftarrow"; "nLeftrightarrow"; "nleftrightarrow"; "nRightarrow"; "nrightarrow"; "rightharpoonup"; "rightharpoondown"; "leftharpoonup"; "leftharpoondown"; "downharpoonleft"; "downharpoonright"; "leftrightharpoons"; "rightleftharpoons"; "upharpoonleft"; "upharpoonright"; "xrightarrow"; "xleftarrow"; "xleftrightarrow"; "xLeftarrow"; "xRightarrow"; "xLeftrightarrow"; "xleftrightharpoons"; "xrightleftharpoons"; "xhookleftarrow"; "xhookrightarrow"; "xmapsto"; "dots"; "ldots"; "cdots"; "ddots"; "udots"; "vdots"; "colon"; "cup"; "union"; "bigcup"; "Union"; "&Union;"; "cap"; "intersection"; "bigcap"; "Intersection"; "in"; "coloneqq"; "Coloneqq"; "coloneq"; "Coloneq"; "eqqcolon"; "Eqqcolon"; "eqcolon"; "Eqcolon"; "colonapprox"; "Colonapprox"; "colonsim"; "Colonsim"; "dblcolon"; "ast"; "Cap"; "Cup"; "circledast"; "circledcirc"; "curlyvee"; "curlywedge"; "divideontimes"; "dotplus"; "leftthreetimes"; "rightthreetimes"; "veebar"; "gt"; "lt"; "approxeq"; "backsim"; "backsimeq"; "barwedge"; "doublebarwedge"; "subset"; "subseteq"; "subseteqq"; "subsetneq"; "subsetneqq"; "varsubsetneq"; "varsubsetneqq"; "prec"; "parallel"; "nparallel"; "shortparallel"; "nshortparallel"; "perp"; "eqslantgtr"; "eqslantless"; "gg"; "ggg"; "geq"; "geqq"; "geqslant"; "gneq"; "gneqq"; "gnapprox"; "gnsim"; "gtrapprox"; "ge"; "le"; "leq"; "leqq"; "leqslant"; "lessapprox"; "lessdot"; "lesseqgtr"; "lesseqqgtr"; "lessgtr"; "lneq"; "lneqq"; "lnsim"; "lvertneqq"; "gtrsim"; "gtrdot"; "gtreqless"; "gtreqqless"; "gtrless"; "gvertneqq"; "lesssim"; "lnapprox"; "nsubset"; "nsubseteq"; "nsubseteqq"; "notin"; "ni"; "notni"; "nmid"; "nshortmid"; "preceq"; "npreceq"; "ll"; "ngeq"; "ngeqq"; "ngeqslant"; "nleq"; "nleqq"; "nleqslant"; "nless"; "supset"; "supseteq"; "supseteqq"; "supsetneq"; "supsetneqq"; "varsupsetneq"; "varsupsetneqq"; "approx"; "asymp"; "bowtie"; "dashv"; "Vdash"; "vDash"; "VDash"; "vdash"; "Vvdash"; "models"; "sim"; "simeq"; "nsim"; "smile"; "triangle"; "triangledown"; "triangleleft"; "cong"; "succ"; "nsucc"; "ngtr"; "nsupset"; "nsupseteq"; "propto"; "equiv"; "nequiv"; "frown"; "triangleright"; "ncong"; "succeq"; "succapprox"; "succnapprox"; "succcurlyeq"; "succsim"; "succnsim"; "nsucceq"; "nvDash"; "nvdash"; "nVDash"; "amalg"; "pm"; "mp"; "bigcirc"; "wr"; "odot"; "uplus"; "clubsuit"; "spadesuit"; "Diamond"; "diamond"; "sqcup"; "sqcap"; "sqsubset"; "sqsubseteq"; "sqsupset"; "sqsupseteq"; "Subset"; "Supset"; "ltimes"; "div"; "rtimes"; "bot"; "therefore"; "thickapprox"; "thicksim"; "varpropto"; "varnothing"; "flat"; "vee"; "because"; "between"; "Bumpeq"; "bumpeq"; "circeq"; "curlyeqprec"; "curlyeqsucc"; "doteq"; "doteqdot"; "eqcirc"; "fallingdotseq"; "multimap"; "pitchfork"; "precapprox"; "precnapprox"; "preccurlyeq"; "precsim"; "precnsim"; "risingdotseq"; "sharp"; "bullet"; "nexists"; "dagger"; "ddagger"; "not"; "top"; "natural"; "angle"; "measuredangle"; "backprime"; "bigstar"; "blacklozenge"; "lozenge"; "blacksquare"; "blacktriangle"; "blacktriangleleft"; "blacktriangleright"; "blacktriangledown"; "ntriangleleft"; "ntriangleright"; "ntrianglelefteq"; "ntrianglerighteq"; "trianglelefteq"; "trianglerighteq"; "triangleq"; "vartriangleleft"; "vartriangleright"; "forall"; "bigtriangleup"; "bigtriangledown"; "nprec"; "aleph"; "beth"; "eth"; "ell"; "hbar"; "Im"; "imath"; "jmath"; "wp"; "Re"; "Perp"; "Vbar"; "boxdot"; "Box"; "square"; "emptyset"; "empty"; "exists"; "circ"; "rhd"; "lhd"; "lll"; "unrhd"; "unlhd"; "Del"; "nabla"; "sphericalangle"; "heartsuit"; "diamondsuit"; "partial"; "qed"; "mod"; "pmod"; "bottom"; "neg"; "neq"; "ne"; "shortmid"; "mid"; "int"; "integral"; "iint"; "doubleintegral"; "iiint"; "tripleintegral"; "iiiint"; "quadrupleintegral"; "oint"; "conint"; "contourintegral"; "times"; "star"; "circleddash"; "odash"; "intercal"; "smallfrown"; "smallsmile"; "boxminus"; "minusb"; "boxplus"; "plusb"; "boxtimes"; "timesb"; "sum"; "prod"; "product"; "coprod"; "coproduct"; "otimes"; "Otimes"; "bigotimes"; "ominus"; "oslash"; "oplus"; "Oplus"; "bigoplus"; "bigodot"; "bigsqcup"; "bigsqcap"; "biginterleave"; "biguplus"; "wedge"; "Wedge"; "bigwedge"; "Vee"; "bigvee"; "invamp"; "parr"; "frac"; "tfrac"; "binom"; "tbinom"; "tensor"; "multiscripts"; "overbrace"; "underbrace"; "underline"; "bar"; "overline"; "closure"; "widebar"; "vec"; "widevec"; "overrightarrow"; "overleftarrow"; "overleftrightarrow"; "underrightarrow"; "underleftarrow"; "underleftrightarrow"; "dot"; "ddot"; "dddot"; "ddddot"; "tilde"; "widetilde"; "check"; "widecheck"; "hat"; "widehat"; "underset"; "stackrel"; "overset"; "over"; "atop"; "underoverset"; "sqrt"; "root"; "space"; "text"; "statusline"; "tooltip"; "toggle"; "begintoggle"; "endtoggle"; "mathraisebox"; "fghilight"; "fghighlight"; "bghilight"; "bghighlight"; "color"; "bgcolor"; "displaystyle"; "textstyle"; "textsize"; "scriptsize"; "scriptscriptsize"; "mathit"; "mathsf"; "mathtt"; "boldsymbol"; "mathbf"; "mathrm"; "mathbb"; "mathfrak"; "mathfr"; "slash"; "boxed"; "mathcal"; "mathscr"; "begin"; "end"; "substack"; "array"; "arrayopts"; "colalign"; "collayout"; "rowalign"; "align"; "equalrows"; "equalcols"; "rowlines"; "collines"; "frame"; "padding"; "rowopts"; "cellopts"; "rowspan"; "colspan"; "thinspace"; "medspace"; "thickspace"; "quad"; "qquad"; "negspace"; "negthinspace"; "negmedspace"; "negthickspace"; "phantom"; "operatorname"; "mathop"; "mathbin"; "mathrel"; "includegraphics"; "lparen"; "rparen"; "land"; "lor"; "middle"; "mathpunct"; "mathord"]
  |> Seq.map @@ fun word ->
    let path = [word] in
    let node = Syn.TeX_cs (TeX_cs.Word word) in
    path, (Syn.Term [Range.locate_opt None node], None)

(* Feel free to extend this *)
let tex_builtin_symbols =
  List.to_seq ['_'; ','; ';']
  |> Seq.map @@ fun c ->
    let path = [String_util.implode [c]] in
    let node = Syn.TeX_cs (TeX_cs.Symbol c) in
    path, (Syn.Term [Range.locate_opt None node], None)

let builtin_xml_namespaces =
  List.to_seq
    [
      "html", "http://www.w3.org/1999/xhtml";
      "mml", "http://www.w3.org/1998/Math/MathML"
    ]
  |> Seq.map @@ fun (prefix, xmlns) ->
    ["xmlns"; prefix], (Syn.Xmlns {prefix; xmlns}, None)

let builtins =
  Seq.concat @@
    List.to_seq
      [
        builtin_xml_namespaces;
        tex_builtin_words;
        tex_builtin_symbols;
        begin
          let open Builtins.Transclude in
          List.to_seq [expanded_sym; show_heading_sym; toc_sym; numbered_sym; show_metadata_sym]
          |> Seq.map @@ fun sym ->
            Symbol.name sym, (Syn.Term [Range.locate_opt None (Syn.Sym sym)], None)
        end;
        begin
          List.to_seq
            [
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
              ["rel"; "is-article"], Syn.Text Builtin_relation.is_article;
              ["rel"; "is-asset"], Syn.Text Builtin_relation.is_asset;
              ["rel"; "in-host"], Syn.Text Builtin_relation.in_host;
              ["execute"], Syn.Dx_execute;
              ["route-asset"], Syn.Route_asset;
              ["syndicate-query-as-json-blob"], Syn.Syndicate_query_as_json_blob;
              ["syndicate-current-tree-as-atom-feed"], Syn.Syndicate_current_tree_as_atom_feed;
              ["current-tree"], Syn.Current_tree;
            ]
          |> Seq.map @@ fun (path, node) ->
            path, (Syn.Term [Range.locate_opt None node], None)
        end
      ]

let initial_visible_trie : (Syn.resolver_data, Range.t option) Trie.t =
  Yuujinchou.Trie.of_seq builtins

let expand_tree_inner ~forest (code : Tree.code) : Tree.syn =
  let trace k =
    match identity_to_uri code.identity with
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
  Sc.run ~init_visible: initial_visible_trie @@ fun () ->
  let expanded_tree = ignore_entered_range (expand_tree_inner ~forest) code in
  expanded_tree, !diagnostics
