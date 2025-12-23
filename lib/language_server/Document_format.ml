(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** Pretty-printer for Forester markup language.
    Converts parsed AST back to formatted source code. *)

open Forester_prelude
open Forester_core

open struct
  module L = Lsp.Types
  (* Re-bind these types to avoid potential conflicts with other Base modules *)
  type binding_info = Forester_core.binding_info = Strict | Lazy
  type math_mode = Forester_core.math_mode = Inline | Display
  type visibility = Forester_core.visibility = Private | Public
  type delim = Forester_core.delim = Braces | Squares | Parens
  let delim_to_strings = Forester_core.delim_to_strings
end

(** Configuration for the formatter *)
type config = {
  indent_width: int;        (** Number of spaces per indentation level *)
  max_line_width: int;      (** Target maximum line width *)
  break_after_frontmatter: bool;  (** Add blank line after frontmatter *)
}

let default_config = {
  indent_width = 2;
  max_line_width = 100;
  break_after_frontmatter = true;
}

(** Frontmatter commands that should appear at the top *)
let frontmatter_commands = [
  "title"; "taxon"; "date"; "author"; "contributor";
  "parent"; "number"; "tag"; "meta"
]

(** Check if a path represents a frontmatter command *)
let is_frontmatter_command (path : Trie.path) =
  match path with
  | [cmd] -> List.mem cmd frontmatter_commands
  | ["author"; "literal"] | ["contributor"; "literal"] -> true
  | _ -> false

(** Check if a node is a frontmatter item *)
let is_frontmatter_node (node : Code.node) =
  match node with
  | Code.Ident path -> is_frontmatter_command path
  | _ -> false

(** Pretty-print a path (namespace/command) *)
let pp_path fmt (path : Trie.path) =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '/')
    Format.pp_print_string
    fmt
    path

(** Pretty-print a binding *)
let pp_binding fmt (info, name) =
  match info with
  | Strict -> Format.fprintf fmt "[%s]" name
  | Lazy -> Format.fprintf fmt "[~%s]" name

(** Pretty-print a list of bindings *)
let pp_bindings fmt bindings =
  List.iter (pp_binding fmt) bindings

(** Pretty-print delimiter open *)
let pp_delim_open fmt delim =
  let open_str, _ = delim_to_strings delim in
  Format.pp_print_string fmt open_str

(** Pretty-print delimiter close *)
let pp_delim_close fmt delim =
  let _, close_str = delim_to_strings delim in
  Format.pp_print_string fmt close_str

(** Determine if content should be on a single line *)
let rec is_simple_content (nodes : Code.t) =
  match nodes with
  | [] -> true
  | [{value = Code.Text _; _}] -> true
  | [{value = Code.Verbatim _; _}] -> true
  | [{value = Code.Ident _; _}] -> true
  | [{value = Code.Hash_ident _; _}] -> true
  | [{value = Code.Get _; _}] -> true
  | _ when List.length nodes <= 3 ->
    List.for_all (fun {Range.value; _} ->
      match value with
      | Code.Text s -> String.length s < 40 && not (String.contains s '\n')
      | Code.Ident _ | Code.Hash_ident _ | Code.Get _ -> true
      | Code.Group (_, inner) -> is_simple_content inner && List.length inner <= 2
      | Code.Math (Inline, _) -> true
      | _ -> false
    ) nodes
  | _ -> false

(** Check if a node is a block-level construct that should have its own line *)
let is_block_node (node : Code.node) =
  match node with
  | Code.Subtree _ | Code.Def _ | Code.Let _ | Code.Namespace _
  | Code.Object _ | Code.Patch _ | Code.Scope _
  | Code.Import _ | Code.Decl_xmlns _
  | Code.Comment _ -> true
  | Code.Ident path -> is_frontmatter_command path
  | Code.Math (Display, _) -> true
  | _ -> false

(** Pretty-print the AST to a formatter *)
let rec pp_nodes ~config ~indent fmt (nodes : Code.t) =
  let indent_str = String.make (indent * config.indent_width) ' ' in

  (* Separate into frontmatter and body *)
  let frontmatter, body =
    let rec split_frontmatter acc = function
      | [] -> List.rev acc, []
      | ({Range.value; _} as node) :: rest ->
        if is_frontmatter_node value then
          split_frontmatter (node :: acc) rest
        else begin match value with
          | Code.Comment _ ->
            (* Comments at the top stay with frontmatter *)
            split_frontmatter (node :: acc) rest
          | _ ->
            List.rev acc, node :: rest
        end
    in
    if indent = 0 then split_frontmatter [] nodes
    else [], nodes
  in

  (* Print frontmatter *)
  List.iter (fun node ->
    pp_node ~config ~indent fmt node;
    Format.pp_print_newline fmt ()
  ) frontmatter;

  (* Add blank line after frontmatter if configured and there's body content *)
  if config.break_after_frontmatter && frontmatter <> [] && body <> [] then
    Format.pp_print_newline fmt ();

  (* Print body *)
  let rec print_body prev_was_block = function
    | [] -> ()
    | ({Range.value; _} as node) :: rest ->
      let is_block = is_block_node value in
      (* Add blank line between block elements *)
      if prev_was_block && is_block then
        Format.pp_print_newline fmt ();
      pp_node ~config ~indent fmt node;
      (* Add newline after block elements *)
      if is_block then
        Format.pp_print_newline fmt ();
      print_body is_block rest
  in
  print_body false body

and pp_node ~config ~indent fmt ({Range.value; _} : Code.node Range.located) =
  let indent_str = String.make (indent * config.indent_width) ' ' in
  match value with
  | Code.Text s ->
    (* Preserve text as-is, but handle leading whitespace *)
    Format.pp_print_string fmt s

  | Code.Verbatim s ->
    (* Use appropriate verbatim syntax *)
    if String.contains s '|' then
      Format.fprintf fmt "\\startverb<<|@\n%s@\n<<|" s
    else
      Format.fprintf fmt "\\verb|%s|" s

  | Code.Comment s ->
    Format.fprintf fmt "%% %s" s

  | Code.Group (delim, inner) ->
    pp_delim_open fmt delim;
    if is_simple_content inner then
      pp_inline_nodes ~config fmt inner
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt inner;
      Format.pp_print_string fmt indent_str
    end;
    pp_delim_close fmt delim

  | Code.Math (mode, inner) ->
    begin match mode with
    | Inline ->
      Format.pp_print_string fmt "#{";
      pp_inline_nodes ~config fmt inner;
      Format.pp_print_char fmt '}'
    | Display ->
      Format.pp_print_string fmt "##{";
      if is_simple_content inner then
        pp_inline_nodes ~config fmt inner
      else begin
        Format.pp_print_newline fmt ();
        pp_nodes ~config ~indent:(indent + 1) fmt inner;
        Format.pp_print_string fmt indent_str
      end;
      Format.pp_print_char fmt '}'
    end

  | Code.Ident path ->
    Format.fprintf fmt "\\%a" pp_path path

  | Code.Hash_ident s ->
    Format.fprintf fmt "#%s" s

  | Code.Xml_ident (prefix, name) ->
    begin match prefix with
    | None -> Format.fprintf fmt "\\<%s>" name
    | Some p -> Format.fprintf fmt "\\<%s:%s>" p name
    end

  | Code.Subtree (addr, inner) ->
    begin match addr with
    | None -> Format.pp_print_string fmt "\\subtree"
    | Some a -> Format.fprintf fmt "\\subtree[%s]" a
    end;
    Format.pp_print_char fmt '{';
    Format.pp_print_newline fmt ();
    pp_nodes ~config ~indent:(indent + 1) fmt inner;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'

  | Code.Let (path, bindings, body) ->
    Format.fprintf fmt "\\let\\%a" pp_path path;
    pp_bindings fmt bindings;
    Format.pp_print_char fmt '{';
    if is_simple_content body then
      pp_inline_nodes ~config fmt body
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt body;
      Format.pp_print_string fmt indent_str
    end;
    Format.pp_print_char fmt '}'

  | Code.Def (path, bindings, body) ->
    Format.fprintf fmt "\\def\\%a" pp_path path;
    pp_bindings fmt bindings;
    Format.pp_print_char fmt '{';
    if is_simple_content body then
      pp_inline_nodes ~config fmt body
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt body;
      Format.pp_print_string fmt indent_str
    end;
    Format.pp_print_char fmt '}'

  | Code.Open path ->
    Format.fprintf fmt "\\open\\%a" pp_path path

  | Code.Scope body ->
    Format.pp_print_string fmt "\\scope{";
    Format.pp_print_newline fmt ();
    pp_nodes ~config ~indent:(indent + 1) fmt body;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'

  | Code.Put (path, body) ->
    Format.fprintf fmt "\\put\\%a" pp_path path;
    Format.pp_print_char fmt '{';
    if is_simple_content body then
      pp_inline_nodes ~config fmt body
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt body;
      Format.pp_print_string fmt indent_str
    end;
    Format.pp_print_char fmt '}'

  | Code.Default (path, body) ->
    Format.fprintf fmt "\\put?\\%a" pp_path path;
    Format.pp_print_char fmt '{';
    if is_simple_content body then
      pp_inline_nodes ~config fmt body
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt body;
      Format.pp_print_string fmt indent_str
    end;
    Format.pp_print_char fmt '}'

  | Code.Get path ->
    Format.fprintf fmt "\\get\\%a" pp_path path

  | Code.Fun (bindings, body) ->
    Format.pp_print_string fmt "\\fun";
    pp_bindings fmt bindings;
    Format.pp_print_char fmt '{';
    if is_simple_content body then
      pp_inline_nodes ~config fmt body
    else begin
      Format.pp_print_newline fmt ();
      pp_nodes ~config ~indent:(indent + 1) fmt body;
      Format.pp_print_string fmt indent_str
    end;
    Format.pp_print_char fmt '}'

  | Code.Object {self; methods} ->
    Format.pp_print_string fmt "\\object";
    begin match self with
    | None -> ()
    | Some s -> Format.fprintf fmt "[%s]" s
    end;
    Format.pp_print_char fmt '{';
    Format.pp_print_newline fmt ();
    List.iter (fun (name, body) ->
      let method_indent = String.make ((indent + 1) * config.indent_width) ' ' in
      Format.fprintf fmt "%s[%s]{" method_indent name;
      if is_simple_content body then
        pp_inline_nodes ~config fmt body
      else begin
        Format.pp_print_newline fmt ();
        pp_nodes ~config ~indent:(indent + 2) fmt body;
        Format.pp_print_string fmt method_indent
      end;
      Format.pp_print_char fmt '}';
      Format.pp_print_newline fmt ()
    ) methods;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'

  | Code.Patch {obj; self; super; methods} ->
    Format.pp_print_string fmt "\\patch{";
    pp_inline_nodes ~config fmt obj;
    Format.pp_print_char fmt '}';
    begin match self with
    | None -> ()
    | Some s -> Format.fprintf fmt "[%s]" s
    end;
    begin match super with
    | None -> ()
    | Some s -> Format.fprintf fmt "[%s]" s
    end;
    Format.pp_print_char fmt '{';
    Format.pp_print_newline fmt ();
    List.iter (fun (name, body) ->
      let method_indent = String.make ((indent + 1) * config.indent_width) ' ' in
      Format.fprintf fmt "%s[%s]{" method_indent name;
      if is_simple_content body then
        pp_inline_nodes ~config fmt body
      else begin
        Format.pp_print_newline fmt ();
        pp_nodes ~config ~indent:(indent + 2) fmt body;
        Format.pp_print_string fmt method_indent
      end;
      Format.pp_print_char fmt '}';
      Format.pp_print_newline fmt ()
    ) methods;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'

  | Code.Call (target, method_name) ->
    pp_inline_nodes ~config fmt target;
    Format.fprintf fmt "#%s" method_name

  | Code.Import (visibility, target) ->
    let cmd = match visibility with
      | Private -> "import"
      | Public -> "export"
    in
    Format.fprintf fmt "\\%s{%s}" cmd target

  | Code.Decl_xmlns (prefix, uri) ->
    Format.fprintf fmt "\\xmlns:%s{%s}" prefix uri

  | Code.Alloc path ->
    Format.fprintf fmt "\\alloc\\%a" pp_path path

  | Code.Namespace (path, body) ->
    Format.fprintf fmt "\\namespace{%a}{" pp_path path;
    Format.pp_print_newline fmt ();
    pp_nodes ~config ~indent:(indent + 1) fmt body;
    Format.pp_print_string fmt indent_str;
    Format.pp_print_char fmt '}'

  | Code.Dx_sequent (conclusion, premises) ->
    (* Datalog sequent: conclusion -: premise1, premise2, ... *)
    pp_inline_nodes ~config fmt conclusion;
    Format.pp_print_string fmt " -: ";
    let rec print_premises = function
      | [] -> ()
      | [p] -> pp_inline_nodes ~config fmt p
      | p :: rest ->
        pp_inline_nodes ~config fmt p;
        Format.pp_print_string fmt ", ";
        print_premises rest
    in
    print_premises premises

  | Code.Dx_query (var, args, results) ->
    Format.fprintf fmt "?%s" var;
    List.iter (fun arg ->
      Format.pp_print_char fmt '{';
      pp_inline_nodes ~config fmt arg;
      Format.pp_print_char fmt '}'
    ) args;
    if results <> [] then begin
      Format.pp_print_string fmt " # ";
      List.iter (fun r ->
        pp_inline_nodes ~config fmt r;
        Format.pp_print_char fmt ' '
      ) results
    end

  | Code.Dx_prop (name, args) ->
    pp_inline_nodes ~config fmt name;
    List.iter (fun arg ->
      Format.pp_print_char fmt '{';
      pp_inline_nodes ~config fmt arg;
      Format.pp_print_char fmt '}'
    ) args

  | Code.Dx_var var ->
    Format.fprintf fmt "?%s" var

  | Code.Dx_const_content body ->
    Format.pp_print_char fmt '\'';
    pp_inline_nodes ~config fmt body

  | Code.Dx_const_uri body ->
    Format.pp_print_char fmt '@';
    pp_inline_nodes ~config fmt body

  | Code.Error s ->
    (* Preserve errors as comments *)
    Format.fprintf fmt "%% ERROR: %s" s

(** Print nodes inline (no newlines) *)
and pp_inline_nodes ~config fmt (nodes : Code.t) =
  List.iter (fun node -> pp_inline_node ~config fmt node) nodes

and pp_inline_node ~config fmt ({Range.value; _} : Code.node Range.located) =
  match value with
  | Code.Text s -> Format.pp_print_string fmt s
  | Code.Verbatim s ->
    if String.contains s '|' then
      Format.fprintf fmt "\\verb!%s!" s
    else
      Format.fprintf fmt "\\verb|%s|" s
  | Code.Comment s -> Format.fprintf fmt "%% %s" s
  | Code.Group (delim, inner) ->
    pp_delim_open fmt delim;
    pp_inline_nodes ~config fmt inner;
    pp_delim_close fmt delim
  | Code.Math (mode, inner) ->
    begin match mode with
    | Inline -> Format.fprintf fmt "#{"; pp_inline_nodes ~config fmt inner; Format.pp_print_char fmt '}'
    | Display -> Format.fprintf fmt "##{"; pp_inline_nodes ~config fmt inner; Format.pp_print_char fmt '}'
    end
  | Code.Ident path -> Format.fprintf fmt "\\%a" pp_path path
  | Code.Hash_ident s -> Format.fprintf fmt "#%s" s
  | Code.Xml_ident (prefix, name) ->
    begin match prefix with
    | None -> Format.fprintf fmt "\\<%s>" name
    | Some p -> Format.fprintf fmt "\\<%s:%s>" p name
    end
  | Code.Get path -> Format.fprintf fmt "\\get\\%a" pp_path path
  | Code.Import (visibility, target) ->
    let cmd = match visibility with Private -> "import" | Public -> "export" in
    Format.fprintf fmt "\\%s{%s}" cmd target
  | Code.Dx_var var -> Format.fprintf fmt "?%s" var
  | Code.Dx_const_content body ->
    Format.pp_print_char fmt '\'';
    pp_inline_nodes ~config fmt body
  | Code.Dx_const_uri body ->
    Format.pp_print_char fmt '@';
    pp_inline_nodes ~config fmt body
  | _ ->
    (* For complex nodes inline, delegate to the block printer *)
    pp_node ~config ~indent:0 fmt {Range.value; loc = None}

(** Format code to a string *)
let format_code ?(config = default_config) (code : Code.t) : string =
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt config.max_line_width;
  pp_nodes ~config ~indent:0 fmt code;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Compute formatting edits for a document *)
let compute (params : L.DocumentFormattingParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let uri = params.textDocument.uri in
  let forester_uri = URI_scheme.lsp_uri_to_uri ~base: forest.config.url uri in

  match Forester_compiler.State.get_code forest forester_uri with
  | None ->
    Logs.debug (fun m -> m "Format: Could not find code for %a" URI.pp forester_uri);
    None
  | Some {nodes; _} ->
    (* Get the original document to determine range *)
    let doc_opt =
      match Forester_compiler.State.find_opt forest forester_uri with
      | Some tree -> Forester_core.Tree.to_doc tree
      | None -> None
    in
    match doc_opt with
    | None ->
      Logs.debug (fun m -> m "Format: Could not find document for %a" URI.pp forester_uri);
      None
    | Some doc ->
      let text = Lsp.Text_document.text doc in
      let line_count =
        let count = ref 0 in
        String.iter (fun c -> if c = '\n' then incr count) text;
        !count + 1
      in
      let last_line_length =
        match String.rindex_opt text '\n' with
        | None -> String.length text
        | Some i -> String.length text - i - 1
      in

      (* Format the code *)
      let config = {
        default_config with
        indent_width = params.options.tabSize;
      } in
      let formatted = format_code ~config nodes in

      (* Create a single edit that replaces the entire document *)
      let range = L.Range.create
        ~start:(L.Position.create ~line:0 ~character:0)
        ~end_:(L.Position.create ~line:line_count ~character:last_line_length)
      in
      let edit = L.TextEdit.create ~range ~newText:formatted in
      Some [edit]

(** Compute range formatting edits *)
let compute_range (params : L.DocumentRangeFormattingParams.t) =
  (* For now, just format the whole document *)
  (* A proper implementation would format only the selected range *)
  let full_params = L.DocumentFormattingParams.create
    ~textDocument:params.textDocument
    ~options:params.options
    ()
  in
  compute full_params
