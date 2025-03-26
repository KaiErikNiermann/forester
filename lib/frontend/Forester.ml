(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module M = URI.Map
module T = Types
module EP = Eio.Path

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

type target = HTML | JSON | XML | STRING

let (let*) = Option.bind

let output_dir_name = "output"

let create_tree ~env ~dest_dir ~prefix ~template ~mode ~config ~(forest : State.t) =
  let addrs =
    let@ article = List.filter_map @~ Forest.get_all_articles forest.resources in
    let@ uri = Option.bind article.frontmatter.uri in
    let* path = article.frontmatter.source_path in
    Some (uri, path)
  in
  let next, next_dir = URI_util.next_uri addrs ~prefix ~mode ~config in
  let fname = next ^ ".tree" in
  let now = Human_datetime.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name ->
      EP.load
        EP.(Eio.Stdenv.cwd env / "templates" / (name ^ ".tree"))
  in
  let body = Format.asprintf "\\date{%a}\n" Human_datetime.pp now in
  let create = `Exclusive 0o644 in
  (* If no dest_dir is passed, use the directory of the last previous tree *)
  let dir =
    match dest_dir with
    | Some dir -> dir
    | None ->
      match next_dir with
      | Some next_dir -> next_dir
      | None -> Reporter.fatal Missing_argument ~extra_remarks: [Asai.Diagnostic.loctext "Unable to guess destination director for new tree; please supply one."]
  in
  let path =
    EP.(env#fs / dir / fname)
  in
  EP.save ~create path @@ body ^ template_content;
  EP.native_exn path

let complete ~(forest : State.t) prefix : (string * string) Seq.t =
  let config = forest.config in
  let@ article = Seq.filter_map @~ List.to_seq @@ Forest.get_all_articles forest.resources in
  let@ uri = Option.bind article.frontmatter.uri in
  let@ uri = Option.bind @@ Option_util.guard URI_scheme.is_named_uri uri in
  let uri = URI.relative_path_string ~host: config.host uri in
  let@ title = Option.bind article.frontmatter.title in
  let title =
    Plain_text_client.string_of_content
      ~forest: forest.resources
      ~router: (Legacy_xml_client.route forest)
      title
  in
  if String.starts_with ~prefix title then
    Some (uri, title)
  else
    None

let is_hidden_file fname =
  String.starts_with ~prefix: "." fname

let copy_contents_of_dir ~env dir =
  Logs.debug (fun m -> m "copying contents of directory %s." (Eio.Path.native_exn dir));
  let cwd = Eio.Stdenv.cwd env in
  let@ fname = List.iter @~ EP.read_dir dir in
  if not @@ is_hidden_file fname then
    let path = EP.(dir / fname) in
    let source = EP.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir: output_dir_name

let json_manifest ~dev ~(forest : State.t) : string =
  let render = Json_manifest_client.render_tree ~forest in
  forest.resources
  |> Forest.get_all_articles
  |> List.filter_map (fun tree -> render ~dev tree)
  |> (fun t -> `Assoc t)
  |> Yojson.Safe.to_string

let render_forest ~dev ~(forest : State.t) : unit =
  let cwd = Eio.Stdenv.cwd forest.env in
  let all_resources = forest.resources |> Forest.get_all_resources in
  List.iter (fun t -> Forest.plant_resource t forest.graphs forest.resources) all_resources;
  Logs.debug (fun m -> m "Rendering %i resources" (List.length all_resources));
  begin
    let json_string = json_manifest ~dev ~forest in
    let json_path = EP.(cwd / output_dir_name / "forest.json") in
    Eio_util.ensure_context_of_path ~perm: 0o755 json_path;
    EP.save ~create: (`Or_truncate 0o644) json_path json_string
  end;
  let module Graphs = (val forest.graphs) in
  let jobs =
    (* TODO: this takes a long time, but it does not seem to be the case that parallising helps at all. *)
    let@ resource = List.filter_map @~ all_resources in
    match resource with
    | T.Article article ->
      let@ uri = Option.map @~ article.frontmatter.uri in
      let route = Legacy_xml_client.route forest uri in
      let content = Format.asprintf "%a" Legacy_xml_client.(pp_xml ~forest ~stylesheet: "default.xsl") article in
      route, content
    | T.Asset asset ->
      Option.some @@
        let route = Legacy_xml_client.route forest asset.uri in
        route, asset.content
  in
  Logs.debug (fun m -> m "Writing %i files to output" (List.length jobs));
  begin
    (* Note: this part appears to be fast! *)
    let@ (route, content) = Eio.Fiber.List.iter ~max_fibers: 20 @~ jobs in
    let@ () = Reporter.easy_run in
    let path = EP.(cwd / output_dir_name / URI.path_string route) in
    Eio_util.ensure_context_of_path ~perm: 0o755 path;
    EP.save ~create: (`Or_truncate 0o644) path content;
  end

let export ~(forest : State.t) : unit =
  Reporter.log Format.pp_print_string "Exporting forest";
  let local_resources =
    let@ resource = List.filter @~ Forest.get_all_resources forest.resources in
    match resource with
    | T.Article {frontmatter = {uri = Some uri; _}; _} ->
      URI.host uri = Some forest.config.host
    | T.Asset asset -> URI.host asset.uri = Some forest.config.host
    | _ -> false
  in
  let cwd = Eio.Stdenv.cwd forest.env in
  let result = Repr.to_json_string ~minify: true (T.forest_t T.content_t) @@ local_resources in
  let dir = Eio.Path.(cwd / "export") in
  let filename = forest.config.host ^ ".json" in
  Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
  Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / filename) result
