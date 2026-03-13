(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open struct
  type binding_info = Forester_core.binding_info = Strict | Lazy
  type math_mode = Forester_core.math_mode = Inline | Display
  type visibility = Forester_core.visibility = Private | Public
  type delim = Forester_core.delim = Braces | Squares | Parens

  let delim_to_strings = Forester_core.delim_to_strings
end

type config = {
  indent_width: int;
  max_line_width: int;
  break_after_frontmatter: bool;
}

type nested_render = {
  pp_nodes: indent: int -> Format.formatter -> Code.t -> unit;
}

let default_config =
  {indent_width = 2; max_line_width = 100; break_after_frontmatter = true}

let frontmatter_commands = [
  "title";
  "taxon";
  "date";
  "author";
  "contributor";
  "parent";
  "number";
  "tag";
  "meta";
]

let is_frontmatter_command (path : Trie.path) =
  match path with
  | [cmd] -> List.mem cmd frontmatter_commands
  | ["author"; "literal"] | ["contributor"; "literal"] -> true
  | _ -> false

let is_frontmatter_node (node : Code.node) =
  match node with Code.Ident path -> is_frontmatter_command path | _ -> false

let pp_path fmt (path : Trie.path) =
  Format.pp_print_list
    ~pp_sep: (fun fmt () -> Format.pp_print_char fmt '/')
    Format.pp_print_string
    fmt
    path

let pp_binding fmt (info, name) =
  match info with
  | Strict -> Format.fprintf fmt "[%s]" name
  | Lazy -> Format.fprintf fmt "[~%s]" name

let pp_bindings fmt bindings = List.iter (pp_binding fmt) bindings

let pp_delim_open fmt delim =
  let open_str, _ = delim_to_strings delim in
  Format.pp_print_string fmt open_str

let pp_delim_close fmt delim =
  let _, close_str = delim_to_strings delim in
  Format.pp_print_string fmt close_str

let indent_string config indent = String.make (indent * config.indent_width) ' '

let rec is_simple_content (nodes : Code.t) =
  match nodes with
  | [] -> true
  | [{value = Code.Text _; _}] -> true
  | [{value = Code.Verbatim _; _}] -> true
  | [{value = Code.Ident _; _}] -> true
  | [{value = Code.Hash_ident _; _}] -> true
  | [{value = Code.Get _; _}] -> true
  | _ when List.length nodes <= 3 ->
    List.for_all
      (fun {Range.value; _} ->
        match value with
        | Code.Text s -> String.length s < 40 && not (String.contains s '\n')
        | Code.Ident _ | Code.Hash_ident _ | Code.Get _ -> true
        | Code.Group (_, inner) ->
          is_simple_content inner && List.length inner <= 2
        | Code.Math (Inline, _) -> true
        | _ -> false
      )
      nodes
  | _ -> false

let is_block_node (node : Code.node) =
  match node with
  | Code.Subtree _
  | Code.Def _
  | Code.Let _
  | Code.Namespace _
  | Code.Object _
  | Code.Patch _
  | Code.Scope _
  | Code.Import _
  | Code.Decl_xmlns _
  | Code.Comment _ ->
    true
  | Code.Ident path -> is_frontmatter_command path
  | Code.Math (Display, _) -> true
  | _ -> false

let pp_verbatim ~inline fmt s =
  if String.contains s '|' then
    if inline then Format.fprintf fmt "\\verb!%s!" s
    else Format.fprintf fmt "\\startverb<<|@\n%s@\n<<|" s
  else Format.fprintf fmt "\\verb|%s|" s

let pp_xml_ident fmt (prefix, name) =
  match prefix with
  | None -> Format.fprintf fmt "\\<%s>" name
  | Some p -> Format.fprintf fmt "\\<%s:%s>" p name

let pp_inline_import fmt visibility target =
  let cmd = match visibility with Private -> "import" | Public -> "export" in
  Format.fprintf fmt "\\%s{%s}" cmd target

let rec pp_inline_nodes ~render ~config fmt (nodes : Code.t) =
  List.iter (fun node -> pp_inline_node ~render ~config fmt node) nodes

and pp_body ~render ~config ~indent ~closing_indent fmt body =
  if is_simple_content body then pp_inline_nodes ~render ~config fmt body
  else
    begin
      Format.pp_print_newline fmt ();
      render.pp_nodes ~indent: (indent + 1) fmt body;
      Format.pp_print_string fmt closing_indent
    end

and pp_braced_body ~render ~config ~indent fmt body =
  Format.pp_print_char fmt '{';
  pp_body
    ~render
    ~config
    ~indent
    ~closing_indent: (indent_string config indent)
    fmt
    body;
  Format.pp_print_char fmt '}'

and pp_forced_block_body ~render ~config ~indent fmt body =
  Format.pp_print_char fmt '{';
  Format.pp_print_newline fmt ();
  render.pp_nodes ~indent: (indent + 1) fmt body;
  Format.pp_print_string fmt (indent_string config indent);
  Format.pp_print_char fmt '}'

and pp_methods_block ~render ~config ~indent fmt methods =
  List.iter
    (fun (name, body) ->
      let method_indent = indent_string config (indent + 1) in
      Format.fprintf fmt "%s[%s]{" method_indent name;
      pp_body
        ~render
        ~config
        ~indent: (indent + 1)
        ~closing_indent: method_indent
        fmt
        body;
      Format.pp_print_char fmt '}';
      Format.pp_print_newline fmt ()
    )
    methods

and pp_node
    ~render
    ~config
    ~indent
    fmt
    ({Range.value; _}: Code.node Range.located)
  =
  let indent_str = indent_string config indent in
  match value with
  | Code.Text s -> Format.pp_print_string fmt s
  | Code.Verbatim s -> pp_verbatim ~inline: false fmt s
  | Code.Comment s -> Format.fprintf fmt "%% %s" s
  | Code.Group (delim, inner) ->
    pp_delim_open fmt delim;
    pp_body ~render ~config ~indent ~closing_indent: indent_str fmt inner;
    pp_delim_close fmt delim
  | Code.Math (mode, inner) ->
    begin
      match mode with
      | Inline ->
        Format.pp_print_string fmt "#{";
        pp_inline_nodes ~render ~config fmt inner;
        Format.pp_print_char fmt '}'
      | Display ->
        Format.pp_print_string fmt "##{";
        pp_body ~render ~config ~indent ~closing_indent: indent_str fmt inner;
        Format.pp_print_char fmt '}'
    end
  | Code.Ident path -> Format.fprintf fmt "\\%a" pp_path path
  | Code.Hash_ident s -> Format.fprintf fmt "#%s" s
  | Code.Xml_ident (prefix, name) -> pp_xml_ident fmt (prefix, name)
  | Code.Subtree (addr, inner) ->
    begin
      match addr with
      | None -> Format.pp_print_string fmt "\\subtree"
      | Some a -> Format.fprintf fmt "\\subtree[%s]" a
    end;
    pp_forced_block_body ~render ~config ~indent fmt inner
  | Code.Let (path, bindings, body) ->
    Format.fprintf fmt "\\let\\%a" pp_path path;
    pp_bindings fmt bindings;
    pp_braced_body ~render ~config ~indent fmt body
  | Code.Def (path, bindings, body) ->
    Format.fprintf fmt "\\def\\%a" pp_path path;
    pp_bindings fmt bindings;
    pp_braced_body ~render ~config ~indent fmt body
  | Code.Open path -> Format.fprintf fmt "\\open\\%a" pp_path path
  | Code.Scope body ->
    Format.pp_print_string fmt "\\scope";
    pp_forced_block_body ~render ~config ~indent fmt body
  | Code.Put (path, body) ->
    Format.fprintf fmt "\\put\\%a" pp_path path;
    pp_braced_body ~render ~config ~indent fmt body
  | Code.Default (path, body) ->
    Format.fprintf fmt "\\put?\\%a" pp_path path;
    pp_braced_body ~render ~config ~indent fmt body
  | Code.Get path -> Format.fprintf fmt "\\get\\%a" pp_path path
  | Code.Fun (bindings, body) ->
    Format.pp_print_string fmt "\\fun";
    pp_bindings fmt bindings;
    pp_braced_body ~render ~config ~indent fmt body
  | Code.Object {self; methods} ->
    Format.pp_print_string fmt "\\object";
    Option.iter (Format.fprintf fmt "[%s]") self;
    Format.pp_print_char fmt '{';
    Format.pp_print_newline fmt ();
    pp_methods_block ~render ~config ~indent fmt methods;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'
  | Code.Patch {obj; self; super; methods} ->
    Format.pp_print_string fmt "\\patch{";
    pp_inline_nodes ~render ~config fmt obj;
    Format.pp_print_char fmt '}';
    Option.iter (Format.fprintf fmt "[%s]") self;
    Option.iter (Format.fprintf fmt "[%s]") super;
    Format.pp_print_char fmt '{';
    Format.pp_print_newline fmt ();
    pp_methods_block ~render ~config ~indent fmt methods;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'
  | Code.Call (target, method_name) ->
    pp_inline_nodes ~render ~config fmt target;
    Format.fprintf fmt "#%s" method_name
  | Code.Import (visibility, target) -> pp_inline_import fmt visibility target
  | Code.Decl_xmlns (prefix, uri) ->
    Format.fprintf fmt "\\xmlns:%s{%s}" prefix uri
  | Code.Alloc path -> Format.fprintf fmt "\\alloc\\%a" pp_path path
  | Code.Namespace (path, body) ->
    Format.fprintf fmt "\\namespace{%a}" pp_path path;
    pp_forced_block_body ~render ~config ~indent fmt body
  | Code.Dx_sequent (conclusion, premises) ->
    pp_inline_nodes ~render ~config fmt conclusion;
    Format.pp_print_string fmt " -: ";
    let rec print_premises = function
      | [] -> ()
      | [premise] -> pp_inline_nodes ~render ~config fmt premise
      | premise :: rest ->
        pp_inline_nodes ~render ~config fmt premise;
        Format.pp_print_string fmt ", ";
        print_premises rest
    in
    print_premises premises
  | Code.Dx_query (var, args, results) ->
    Format.fprintf fmt "?%s" var;
    List.iter
      (fun arg ->
        Format.pp_print_char fmt '{';
        pp_inline_nodes ~render ~config fmt arg;
        Format.pp_print_char fmt '}'
      )
      args;
    if results <> [] then
      begin
        Format.pp_print_string fmt " # ";
        List.iter
          (fun result ->
            pp_inline_nodes ~render ~config fmt result;
            Format.pp_print_char fmt ' '
          )
          results
      end
  | Code.Dx_prop (name, args) ->
    pp_inline_nodes ~render ~config fmt name;
    List.iter
      (fun arg ->
        Format.pp_print_char fmt '{';
        pp_inline_nodes ~render ~config fmt arg;
        Format.pp_print_char fmt '}'
      )
      args
  | Code.Dx_var var -> Format.fprintf fmt "?%s" var
  | Code.Dx_const_content body ->
    Format.pp_print_char fmt '\'';
    pp_inline_nodes ~render ~config fmt body
  | Code.Dx_const_uri body ->
    Format.pp_print_char fmt '@';
    pp_inline_nodes ~render ~config fmt body
  | Code.Error s -> Format.fprintf fmt "%% ERROR: %s" s

and pp_inline_node
    ~render
    ~config
    fmt
    ({Range.value; _}: Code.node Range.located)
  =
  match value with
  | Code.Text s -> Format.pp_print_string fmt s
  | Code.Verbatim s -> pp_verbatim ~inline: true fmt s
  | Code.Comment s -> Format.fprintf fmt "%% %s" s
  | Code.Group (delim, inner) ->
    pp_delim_open fmt delim;
    pp_inline_nodes ~render ~config fmt inner;
    pp_delim_close fmt delim
  | Code.Math (mode, inner) ->
    begin
      match mode with
      | Inline ->
        Format.fprintf fmt "#{";
        pp_inline_nodes ~render ~config fmt inner;
        Format.pp_print_char fmt '}'
      | Display ->
        Format.fprintf fmt "##{";
        pp_inline_nodes ~render ~config fmt inner;
        Format.pp_print_char fmt '}'
    end
  | Code.Ident path -> Format.fprintf fmt "\\%a" pp_path path
  | Code.Hash_ident s -> Format.fprintf fmt "#%s" s
  | Code.Xml_ident (prefix, name) -> pp_xml_ident fmt (prefix, name)
  | Code.Get path -> Format.fprintf fmt "\\get\\%a" pp_path path
  | Code.Import (visibility, target) -> pp_inline_import fmt visibility target
  | Code.Dx_var var -> Format.fprintf fmt "?%s" var
  | Code.Dx_const_content body ->
    Format.pp_print_char fmt '\'';
    pp_inline_nodes ~render ~config fmt body
  | Code.Dx_const_uri body ->
    Format.pp_print_char fmt '@';
    pp_inline_nodes ~render ~config fmt body
  | _ -> pp_node ~render ~config ~indent: 0 fmt {Range.value; loc = None}
