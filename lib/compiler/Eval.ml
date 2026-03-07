(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open struct
  module T = Types
  module String_map = Value.String_map
  module Symbol_map = Value.Symbol_map

  type located = Value.t Range.located
end

module Args = Eval_args
module Datalog = Eval_datalog
module Object = Eval_object
module Transclusion = Eval_transclusion

let extract_content = Args.extract_content
let extract_text_loc = Args.extract_text_loc
let extract_text = Args.extract_text
let extract_obj_ptr = Args.extract_obj_ptr
let extract_sym = Args.extract_sym
let extract_bool = Args.extract_bool
let default_backmatter = Args.default_backmatter

type result = {
  articles : T.content T.article list;
  jobs : Job.job Range.located list;
}
[@@deriving show]

module Tape = Tape_effect.Make ()

module Lex_env = Algaeff.Reader.Make (struct
  type t = Value.t String_map.t
end)

module Dyn_env = Algaeff.Reader.Make (struct
  type t = Value.t Symbol_map.t
end)

module Config_env = Algaeff.Reader.Make (struct
  type t = Config.t
end)

module Heap = Algaeff.State.Make (struct
  type t = Value.obj Symbol_map.t
end)

module Emitted_trees = Algaeff.State.Make (struct
  type t = T.content T.article list
end)

module Jobs = Algaeff.State.Make (struct
  type t = Job.job Range.located list
end)

module Frontmatter = Algaeff.State.Make (struct
  type t = T.content T.frontmatter
end)

module Mode_env = Algaeff.Reader.Make (struct
  type t = eval_mode
end)

let get_current_uri ~loc =
  Transclusion.get_current_uri ~loc (Frontmatter.get ())

let get_transclusion_flags ~loc =
  Transclusion.get_transclusion_flags ~loc ~dynenv:(Dyn_env.read ())
    ~extract_bool

let resolve_uri ~loc:_ str = Args.resolve_uri ~base:(Config_env.read ()).url str

let extract_uri (node : located) =
  Args.extract_uri ~base:(Config_env.read ()).url node

let extract_dx_term = Datalog.extract_dx_term
let extract_dx_prop = Datalog.extract_dx_prop
let extract_dx_sequent = Datalog.extract_dx_sequent

let extract_vertex ~type_ (node : located) =
  Args.extract_vertex ~base:(Config_env.read ()).url ~type_ node

let pp_tex_cs = Args.pp_tex_cs

let rec process_tape () =
  match Tape.pop_node_opt () with
  | None -> Value.Content (T.Content [])
  | Some node -> eval_node node

and eval_tape tape = Tape.run ~tape process_tape
and eval_pop_arg ~loc = Tape.pop_arg ~loc |> Range.map eval_tape
and pop_content_arg ~loc = eval_pop_arg ~loc |> extract_content
and pop_text_arg ~loc = eval_pop_arg ~loc |> extract_text
and pop_text_arg_loc ~loc = eval_pop_arg ~loc |> extract_text_loc

and eval_node node : Value.t =
  let loc = node.loc in
  match node.value with
  | Var x -> eval_var ~loc x
  | Text str -> emit_content_node ~loc @@ T.Text str
  | Prim p ->
      let content =
        pop_content_arg ~loc |> T.extract_content |> T.trim_whitespace
      in
      emit_content_node ~loc @@ T.prim p @@ T.Content content
  | Fun (xs, body) ->
      let env = Lex_env.read () in
      focus_clo ?loc env (List.map (fun (info, x) -> (info, Some x)) xs) body
  | Ref -> begin
      match eval_pop_arg ~loc |> extract_uri with
      | Ok href ->
          let content =
            T.Content
              [
                T.Transclude { href; target = T.Taxon };
                T.Text " ";
                T.Contextual_number href;
              ]
          in
          emit_content_node ~loc @@ Link { href; content }
      | Error _ ->
          Reporter.fatal ?loc
            (Type_error { got = None; expected = [ URI ] })
            ~extra_remarks:
              [ Asai.Diagnostic.loctextf "Expected valid URI in ref" ]
    end
  | Link { title; dest } ->
      let dest = { node with value = dest } |> Range.map eval_tape in
      let href =
        match extract_uri dest with
        | Ok uri -> uri
        | Error error ->
            Reporter.fatal ?loc
              (Type_error { expected = [ URI ]; got = None })
              ~extra_remarks:[ Asai.Diagnostic.loctext error ]
        (* "Expected valid URI in link") *)
      in
      let content =
        match title with
        | None ->
            T.Content
              [
                T.Transclude
                  { href; target = T.Title { empty_when_untitled = false } };
              ]
        | Some title -> { node with value = eval_tape title } |> extract_content
      in
      emit_content_node ~loc @@ Link { href; content }
  | Math (mode, body) ->
      let content =
        let@ () = Mode_env.run ~env:TeX_mode in
        { node with value = eval_tape body } |> extract_content
      in
      emit_content_node ~loc @@ KaTeX (mode, content)
  | Xml_tag (name, attrs, body) ->
      let rec process : _ list -> _ T.xml_attr list = function
        | [] -> []
        | (key, v) :: attrs ->
            { T.key; value = extract_content { node with value = eval_tape v } }
            :: process attrs
      in
      let name =
        T.{ prefix = name.prefix; uname = name.uname; xmlns = name.xmlns }
      in
      let content = { node with value = eval_tape body } |> extract_content in
      emit_content_node ~loc
      @@ T.Xml_elt { name; attrs = process attrs; content }
  | TeX_cs cs ->
      emit_content_node ~loc @@ T.Text (Format.asprintf "%a" pp_tex_cs cs)
  | Unresolved_ident (visible, path) ->
      let tex_cs_opt =
        match path with [ name ] -> TeX_cs.parse name | _ -> None
      in
      begin match (Mode_env.read (), tex_cs_opt) with
      | TeX_mode, Some (cs, rest) ->
          emit_content_node ~loc
          @@ T.Text (Format.asprintf "%a%s" pp_tex_cs cs rest)
      | _, _ ->
          let extra_remarks = Suggestions.create_suggestions ~visible path in
          Reporter.emit ?loc ~extra_remarks
            (Unresolved_identifier (visible, path));
          emit_content_node ~loc
          @@ T.Text (Format.asprintf "\\%a" Resolver.Scope.pp_path path)
      end
  | Transclude ->
      Transclusion.eval_transclude ~loc
        ~flags:(get_transclusion_flags ~loc)
        ~eval_pop_arg ~extract_uri ~emit_content_node
  | Subtree (addr_opt, nodes) ->
      Transclusion.eval_subtree ~loc
        ~flags:(get_transclusion_flags ~loc)
        ~config:(Config_env.read ()) ~parent_frontmatter:(Frontmatter.get ())
        ~addr_opt ~nodes ~eval_tree_inner
        ~emit_tree:(fun subtree -> Emitted_trees.modify @@ List.cons subtree)
        ~emit_content_node
  | Results_of_query ->
      Transclusion.eval_results_of_query ~loc ~eval_pop_arg ~emit_content_node
  | Syndicate_query_as_json_blob ->
      let name = pop_text_arg ~loc in
      let query_arg = eval_pop_arg ~loc in
      begin match query_arg.value with
      | Dx_query query ->
          let job =
            Transclusion.json_blob_job ~loc ~config:(Config_env.read ()) ~name
              query
          in
          Jobs.modify @@ List.cons job;
          process_tape ()
      | other ->
          Reporter.fatal ?loc:query_arg.loc
            (Type_error { expected = [ Dx_query ]; got = Some other })
      end
  | Syndicate_current_tree_as_atom_feed ->
      let source_uri = get_current_uri ~loc:node.loc in
      let job = Transclusion.atom_feed_job ~loc ~source_uri in
      Jobs.modify @@ List.cons job;
      process_tape ()
  | Embed_tex ->
      let config = Config_env.read () in
      let preamble, body =
        let@ () = Mode_env.run ~env:TeX_mode in
        let preamble = pop_content_arg ~loc |> TeX_like.string_of_content in
        let body = pop_content_arg ~loc |> TeX_like.string_of_content in
        (preamble, body)
      in
      let job, artefact =
        Transclusion.latex_artefact ~loc ~config ~preamble ~body
      in
      Jobs.modify (List.cons job);
      emit_content_node ~loc @@ T.Artefact artefact
  | Route_asset ->
      let Range.{ value = source_path; loc = path_loc } =
        pop_text_arg_loc ~loc
      in
      emit_content_nodes ~loc
      @@ Transclusion.route_asset_nodes ?loc:path_loc ~source_path ()
  | Object { self; methods } ->
      let sym, obj =
        Object.build_object ~prototype:None ~env:(Lex_env.read ()) ~self
          ~super:None methods
      in
      Heap.modify @@ Symbol_map.add sym obj;
      focus ?loc:node.loc @@ Value.Obj sym
  | Patch { obj; self; super; methods } ->
      let obj_ptr =
        { node with value = obj } |> Range.map eval_tape |> extract_obj_ptr
      in
      let sym, obj =
        Object.build_object ~prototype:(Some obj_ptr) ~env:(Lex_env.read ())
          ~self ~super methods
      in
      Heap.modify @@ Symbol_map.add sym obj;
      focus ?loc:node.loc @@ Value.Obj sym
  | Group (d, body) ->
      let l, r = delim_to_strings d in
      let content =
        let body = extract_content { node with value = eval_tape body } in
        T.Content ((T.Text l :: T.extract_content body) @ [ T.Text r ])
      in
      focus ?loc:node.loc @@ Value.Content (T.compress_content content)
  | Call (obj, method_name) ->
      let sym =
        { node with value = obj } |> Range.map eval_tape |> extract_obj_ptr
      in
      let result =
        Object.call_method ~loc:node.loc ~sym ~method_name ~heap:(Heap.get ())
          ~eval_body:(fun env body ->
            let@ () = Lex_env.run ~env in
            eval_tape body)
      in
      focus ?loc:node.loc result
  | Put (k, v, body) ->
      let k = { node with value = k } |> Range.map eval_tape |> extract_sym in
      let body =
        let@ () = Dyn_env.scope (Symbol_map.add k (eval_tape v)) in
        eval_tape body
      in
      focus ?loc:node.loc body
  | Default (k, v, body) ->
      let k = { node with value = k } |> Range.map eval_tape |> extract_sym in
      let body =
        let upd flenv =
          if Symbol_map.mem k flenv then flenv
          else Symbol_map.add k (eval_tape v) flenv
        in
        let@ () = Dyn_env.scope upd in
        eval_tape body
      in
      focus ?loc:node.loc body
  | Get k ->
      let k = { node with value = k } |> Range.map eval_tape |> extract_sym in
      let env = Dyn_env.read () in
      begin match Symbol_map.find_opt k env with
      | None -> Reporter.fatal ?loc:node.loc (Unbound_fluid_symbol k)
      | Some v -> focus ?loc:node.loc v
      end
  | Verbatim str -> emit_content_node ~loc @@ CDATA str
  | Title ->
      let title = pop_content_arg ~loc in
      Frontmatter.modify (fun fm -> { fm with title = Some title });
      process_tape ()
  | Parent ->
      let parent_arg = eval_pop_arg ~loc in
      let parent =
        match extract_uri parent_arg with
        | Ok uri -> uri
        | Error _ ->
            Reporter.fatal ?loc Invalid_URI
              ~extra_remarks:
                [
                  Asai.Diagnostic.loctext
                    "Expected valid URI in parent declaration";
                ]
      in
      Frontmatter.modify (fun fm -> { fm with designated_parent = Some parent });
      process_tape ()
  | Meta ->
      let k = pop_text_arg ~loc in
      let v = pop_content_arg ~loc in
      Frontmatter.modify (fun fm -> { fm with metas = fm.metas @ [ (k, v) ] });
      process_tape ()
  | Attribution (role, type_) ->
      let arg = eval_pop_arg ~loc in
      let vertex =
        match extract_vertex ~type_ arg with
        | Ok vtx -> vtx
        | Error _ ->
            let corrected_attribution_code =
              match role with
              | Author -> "\\author/literal"
              | Contributor -> "\\contributor/literal"
            in
            Reporter.emit ?loc Type_warning
              ~extra_remarks:
                [
                  Asai.Diagnostic.loctextf
                    "Expected valid URI in attribution. Use `%s` instead if \
                     you intend an unlinked attribution."
                    corrected_attribution_code;
                ];
            T.Content_vertex (extract_content arg)
      in
      let attribution = T.{ role; vertex } in
      Frontmatter.modify (fun fm ->
          { fm with attributions = fm.attributions @ [ attribution ] });
      process_tape ()
  | Tag type_ ->
      let arg = eval_pop_arg ~loc in
      let vertex =
        match extract_vertex ~type_ arg with
        | Ok vtx -> vtx
        | Error _ ->
            let corrected = "\\tag/content" in
            Reporter.emit ?loc Type_warning
              ~extra_remarks:
                [
                  Asai.Diagnostic.loctextf
                    "Expected valid URI in tag. Use `%s` instead if you intend \
                     an unlinked attribution."
                    corrected;
                ];
            T.Content_vertex (extract_content arg)
      in
      Frontmatter.modify (fun fm -> { fm with tags = fm.tags @ [ vertex ] });
      process_tape ()
  | Date ->
      let date_str = pop_text_arg ~loc in
      begin match Human_datetime.parse_string date_str with
      | None ->
          Reporter.fatal ?loc:node.loc Parse_error
            ~extra_remarks:
              [ Asai.Diagnostic.loctextf "Invalid date string `%s`" date_str ]
      | Some date ->
          Frontmatter.modify (fun fm -> { fm with dates = fm.dates @ [ date ] });
          process_tape ()
      end
  | Number ->
      let num = pop_text_arg ~loc in
      Frontmatter.modify (fun fm -> { fm with number = Some num });
      process_tape ()
  | Taxon ->
      let taxon = Some (pop_content_arg ~loc) in
      Frontmatter.modify (fun fm -> { fm with taxon });
      process_tape ()
  | Sym sym -> focus ?loc:node.loc @@ Value.Sym sym
  | Dx_prop (rel, args) ->
      Datalog.eval_prop ~loc:node.loc ~rel ~args ~eval_tape ~extract_text ~focus
  | Dx_sequent (conclusion, premises) ->
      Datalog.eval_sequent ~loc:node.loc ~conclusion ~premises ~eval_tape ~focus
  | Dx_query (var, positives, negatives) ->
      Datalog.eval_query ~loc:node.loc ~var ~positives ~negatives ~eval_tape
        ~focus
  | Dx_var name -> focus ?loc:node.loc @@ Dx_var name
  | Dx_const (type_, arg) ->
      Datalog.eval_const ~loc:node.loc ~type_ ~arg ~eval_tape ~extract_content
        ~extract_uri ~focus
  | Dx_execute ->
      Datalog.eval_execute ~loc:node.loc ~eval_pop_arg ~emit_content_node
  | Current_tree ->
      emit_content_node ~loc:node.loc
      @@ Transclusion.current_tree_node ~uri:(get_current_uri ~loc:node.loc)

and eval_var ~loc (x : string) =
  let env = Lex_env.read () in
  match String_map.find_opt x env with
  | Some v -> focus ?loc v
  | None -> Reporter.fatal ?loc (Unbound_variable x)

and focus ?loc = function
  | Clo (rho, xs, body) -> focus_clo ?loc rho xs body
  | Content content -> begin
      match process_tape () with
      | Content content' ->
          Value.Content (T.concat_compressed_content content content')
      | value -> value
    end
  | ( Sym _ | Obj _ | Dx_prop _ | Dx_sequent _ | Dx_query _ | Dx_var _
    | Dx_const _ ) as v -> begin
      match process_tape () with
      | Content content when T.strip_whitespace content = T.Content [] -> v
      | v' ->
          Reporter.fatal ?loc
            (Type_error { expected = []; got = None })
            ~extra_remarks:
              [
                Asai.Diagnostic.loctextf
                  "Expected solitary node but got %a / %a" Value.pp v Value.pp
                  v';
              ]
    end

and focus_clo ?loc rho (xs : string option binding list) body =
  match xs with
  | [] ->
      focus ?loc
      @@
      let@ () = Lex_env.run ~env:rho in
      eval_tape body
  | (info, y) :: ys -> (
      match Tape.pop_arg_opt () with
      | Some arg ->
          let yval =
            match info with
            | Strict -> eval_tape arg.value
            | Lazy -> Clo (Lex_env.read (), [ (Strict, None) ], arg.value)
          in
          let rhoy =
            match y with Some y -> String_map.add y yval rho | None -> rho
          in
          focus_clo ?loc rhoy ys body
      | None -> begin
          match process_tape () with
          | Content nodes when T.strip_whitespace nodes = T.Content [] ->
              Clo (rho, xs, body)
          | _ ->
              Reporter.fatal ?loc Missing_argument
                ~extra_remarks:
                  [
                    Asai.Diagnostic.loctextf "Expected %i additional arguments"
                      (List.length xs);
                  ]
        end)

and emit_content_nodes ~loc content =
  focus ?loc @@ Content (T.Content (T.compress_nodes content))

and emit_content_node ~loc content = emit_content_nodes ~loc [ content ]

and eval_tree_inner ?(uri : URI.t option) (syn : Syn.t) : T.content T.article =
  let attribution_is_author attr =
    match T.(attr.role) with T.Author -> true | _ -> false
  in
  let outer_frontmatter = Frontmatter.get () in
  let attributions =
    List.filter attribution_is_author outer_frontmatter.attributions
  in
  let frontmatter =
    T.default_frontmatter ?uri ~attributions
      ?source_path:outer_frontmatter.source_path ~dates:outer_frontmatter.dates
      ()
  in
  let@ () = Frontmatter.run ~init:frontmatter in
  let mainmatter = { value = eval_tape syn; loc = None } |> extract_content in
  let frontmatter = Frontmatter.get () in
  let backmatter =
    match uri with Some uri -> default_backmatter ~uri | None -> Content []
  in
  T.{ frontmatter; mainmatter; backmatter }

let empty_result = { articles = []; jobs = [] }

let eval_tree ~(config : Config.t) ~(uri : URI.t) ~(source_path : string option)
    (tree : Syn.t) : result * Reporter.diagnostic list =
  let diagnostics = ref [] in
  let push d = diagnostics := d :: !diagnostics in
  let res =
    Reporter.run
      ~fatal:(fun d ->
        push d;
        empty_result)
      ~emit:push
    @@ fun () ->
    let fm = T.default_frontmatter ~uri ?source_path () in
    let@ () = Mode_env.run ~env:Text_mode in
    let@ () = Frontmatter.run ~init:fm in
    let@ () = Emitted_trees.run ~init:[] in
    let@ () = Jobs.run ~init:[] in
    let@ () = Heap.run ~init:Symbol_map.empty in
    let@ () = Lex_env.run ~env:String_map.empty in
    let@ () = Dyn_env.run ~env:Symbol_map.empty in
    let@ () = Config_env.run ~env:config in
    let main = eval_tree_inner ~uri tree in
    let side = Emitted_trees.get () in
    let jobs = Jobs.get () in
    { articles = main :: side; jobs }
  in
  (res, !diagnostics)
