(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

%{
  open Forester_prelude
  open Forester_core

  let trim = Stdlib.String.trim

  let contains_whitespace s =
    let rec loop i =
      if i >= Stdlib.String.length s then false
      else
        match Stdlib.String.get s i with
        | ' ' | '\t' | '\r' | '\n' -> true
        | _ -> loop (i + 1)
    in
    loop 0

  let invalid_parenthesized_header raw =
    raise @@ Syntax_error.Invalid_syntax raw

  let split_parenthesized_header_items ~allow_empty raw =
    let raw = trim raw in
    if raw = "" then
      if allow_empty then [] else invalid_parenthesized_header raw
    else
      raw
      |> Stdlib.String.split_on_char ','
      |> List.map trim
      |> List.map (fun item ->
        if item = "" || contains_whitespace item then
          invalid_parenthesized_header raw
        else
          item
      )

  let parenthesized_binding_list ~allow_empty raw =
    split_parenthesized_header_items ~allow_empty raw
    |> List.map (fun item ->
      match Stdlib.String.get item 0 with
      | '~' ->
        let name =
          Stdlib.String.sub item 1 (Stdlib.String.length item - 1)
        in
        if name = "" || contains_whitespace name then
          invalid_parenthesized_header raw
        else
          Lazy, name
      | _ -> Strict, item
    )

  let single_parenthesized_name raw =
    match split_parenthesized_header_items ~allow_empty:false raw with
    | [name] -> name
    | _ -> invalid_parenthesized_header raw

  let patch_parenthesized_names raw =
    match split_parenthesized_header_items ~allow_empty:false raw with
    | [self] -> Some self, None
    | [self; super] -> Some self, Some super
    | _ -> invalid_parenthesized_header raw
%}

%token <string option * string> XML_ELT_IDENT
%token <string> DECL_XMLNS
%token <string> TEXT VERBATIM
%token <string> COMMENT
%token <string> WHITESPACE
%token <string> IDENT

%token <string> HASH_IDENT
%token IMPORT EXPORT DEF NAMESPACE LET FUN OPEN
%token OBJECT PATCH CALL
%token SUBTREE SCOPE PUT GET DEFAULT ALLOC
%token SLASH LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE TICK AT_SIGN HASH
%token EOF

%token DATALOG

%token DX_ENTAILED
%token <string> DX_VAR

%start <Code.t> main

%%

let locate(p) ==
| x = p; { Asai.Range.locate_lex $loc x }

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)

let bvar :=
| x = TEXT; { x }

let bvar_with_strictness :=
| x = TEXT; {
  match String_util.explode x with
  | '~' :: chars -> Lazy, String_util.implode chars
  | _ -> Strict, x
 }

let binder == list(squares(bvar_with_strictness))
let parenthesized_binder == raw = parens(wstext); { parenthesized_binding_list ~allow_empty:true raw }
let parenthesized_object_self == raw = parens(wstext); { single_parenthesized_name raw }
let parenthesized_patch_bindings == raw = parens(wstext); { patch_parenthesized_names raw }

let ws_or(p) :=
| WHITESPACE; { [] }
| x = p; { [x] }

let ws_list(p) := flatten(list(ws_or(p)))
let separated_ws_list(sep,p) := flatten(separated_list(sep,ws_or(p)))

let textual_node :=
| ~ = TEXT; <Code.Text>
| ~ = WHITESPACE; <Code.Text>
| ~ = head_node1; <Fun.id>

let code_expr == ws_list(locate(head_node1))
let textual_expr == list(locate(textual_node))

let patch_bindings :=
| self = squares(bvar); super = squares(bvar); { Some self, Some super }
| self = squares(bvar); { Some self, None }
| ~ = parenthesized_patch_bindings; <>
| { None, None }

let head_node :=
| DEF; (~,~,~) = def_fun_spec; <Code.Def>
| ALLOC; ~ = ident; <Code.Alloc>
| EXPORT; ~ = txt_arg; <Code.import_public>
| NAMESPACE; ~ = ident; ~ = braces(code_expr); <Code.Namespace>
| SUBTREE; ~ = option(squares(wstext)); ~ = braces(ws_list(locate(head_node))); <Code.Subtree>
| FUN; ~ = fun_bindings; ~ = arg; <Code.Fun>
| LET; (~,~,~) = fun_spec; <Code.Let>
| ~ = ident; <Code.Ident>
| ~ = HASH_IDENT; <Code.Hash_ident>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = ident; ~ = arg; <Code.Put>
| DEFAULT; ~ = ident; ~ = arg; <Code.Default>
| GET; ~ = ident; <Code.Get>
| OPEN; ~ = ident; <Code.Open>
| (~,~) = XML_ELT_IDENT; <Code.Xml_ident>
| ~ = DECL_XMLNS; ~ = txt_arg; <Code.Decl_xmlns>
| OBJECT; self = object_self_binding; methods = braces(ws_list(method_decl)); { Code.Object {self;  methods } }
| PATCH; obj = braces(code_expr); (self, super) = patch_bindings; methods = braces(ws_list(method_decl)); { Code.Patch {obj; self; super; methods} }
| CALL; ~ = braces(code_expr); ~ = txt_arg; <Code.Call>
| DATALOG; LBRACE; list(WHITESPACE); ~ = dx_sequent_node; RBRACE; <>
| ~ = VERBATIM; <Code.Verbatim>
| ~ = delimited(HASH_LBRACE, textual_expr, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, textual_expr, RBRACE); <Code.display_math>
| ~ = braces(textual_expr); <Code.braces>
| ~ = squares(textual_expr); <Code.squares>
| ~ = parens(textual_expr); <Code.parens>
| ~ = COMMENT; <Code.Comment>

let head_node1 :=
| ~ = head_node; <>
| DX_ENTAILED; { Code.Text "-:" }
| x = DX_VAR; { Code.Text ("?" ^ x) }
| TICK; { Code.Text "'" }
| AT_SIGN; { Code.Text "@" }
| HASH; { Code.Text "#" }

let method_decl :=
| k = squares(TEXT); list(WHITESPACE); v = arg; { k, v }

let object_self_binding :=
| x = squares(bvar); { Some x }
| x = parenthesized_object_self; { Some x }
| { None }

let ident :=
| ~ = separated_nonempty_list(SLASH, IDENT); <>

let ws_or_text :=
| x = TEXT; { x }
| x = WHITESPACE; { x }

let wstext :=
| xs = list(ws_or_text); { String.concat "" xs }

let arg :=
| braces(textual_expr)
| located_str = locate(VERBATIM);
  { [{located_str with value = Code.Verbatim located_str.value}] }

let txt_arg == braces(wstext)
let fun_spec == ~ = ident; ~ = binder; ~ = arg; <>
let fun_bindings :=
| ~ = binder; <>
| ~ = parenthesized_binder; <>

let def_fun_spec :=
| ~ = ident; ~ = binder; ~ = arg; <>
| ~ = ident; ~ = parenthesized_binder; ~ = arg; <>

let head_node_or_import :=
| head_node
| IMPORT; ~ = txt_arg; <Code.import_private>

let main :=
| ~ = ws_list(locate(head_node_or_import)); EOF; <>

let dx_rel :=
| x = locate(ident); { [Range.{x with value = Code.Ident x.value}] }
| TICK; ~ = arg; <>

let dx_term_node :=
| ~ = DX_VAR; <Code.Dx_var>
| TICK; ~ = arg; <Code.Dx_const_content>
| AT_SIGN; ~ = arg; <Code.Dx_const_uri>

let dx_term :=
| x = locate(dx_term_node); { [x] }

let dx_prop_node :=
| ~ = dx_rel; ~ = ws_list(dx_term); <Code.Dx_prop>

let dx_prop :=
| p = locate(dx_prop_node); { [p] }

let dx_sequent_node :=
| ~ = dx_prop; DX_ENTAILED; ~ = ws_list(braces(dx_prop)); <Code.Dx_sequent>
| x = DX_VAR; list(WHITESPACE); DX_ENTAILED; pos = ws_list(braces(dx_prop)); { Code.Dx_query (x, pos, []) }
| ~ = DX_VAR; list(WHITESPACE); DX_ENTAILED; ~ = ws_list(braces(dx_prop)); HASH; ~ = ws_list(braces(dx_prop)); <Code.Dx_query>
