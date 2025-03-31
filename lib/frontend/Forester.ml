(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

open struct
  module M = URI.Map
  module T = Types
  module EP = Eio.Path
end

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

type target = HTML | JSON | XML | STRING

let output_dir_name = "output"

let create_tree ~env ~dest_dir ~prefix ~template ~mode ~(forest : State.t) =
  let next, next_dir = URI_util.next_uri ~prefix ~mode ~forest in
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
  (* If no dest_dir is passed, use the directory of the previous tree *)
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

let complete ~(forest : State.t) prefix : (string * string) List.t =
  let config = forest.config in
  let@ article = List.filter_map @~ List.of_seq @@ State.get_all_articles forest in
  let@ uri = Option.bind article.frontmatter.uri in
  let short_uri = URI.display_path_string ~base: config.url uri in
  let@ title = Option.bind article.frontmatter.title in
  let title =
    Plain_text_client.string_of_content
      ~forest
      ~router: (Legacy_xml_client.route forest)
      title
  in
  if String.starts_with ~prefix title then
    Some (short_uri, title)
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
  forest
  |> State.get_all_articles
  |> Seq.filter_map (fun tree -> render ~dev tree)
  |> List.of_seq
  |> (fun t -> `Assoc t)
  |> Yojson.Safe.to_string

let html_redirect uri_string =
  Pure_html.to_xml @@
    let open Pure_html in
    let open HTML in
    html
      []
      [
        head
          []
          [
            meta
              [
                http_equiv `refresh;
                content "0;url=%s" uri_string
              ]
          ]
      ]

let render_forest ~dev ~(forest : State.t) : unit =
  let cwd = Eio.Stdenv.cwd forest.env in
  let all_resources = forest |> State.get_all_resources in
  Seq.iter (fun t -> State.plant_resource t forest) all_resources;
  Logs.debug (fun m -> m "Rendering %i resources" (Seq.length all_resources));
  begin
    let json_string = json_manifest ~dev ~forest in
    let json_path = EP.(cwd / output_dir_name / "forest.json") in
    Eio_util.ensure_context_of_path ~perm: 0o755 json_path;
    EP.save ~create: (`Or_truncate 0o644) json_path json_string
  end;
  let jobs =
    let home_route = String.concat "/" @@ URI.path_components forest.config.url @ ["index.html"] in
    let home_content = html_redirect @@ String.concat "/" @@ Legacy_xml_client.local_path_components forest.config (Config.home_uri forest.config) in
    List.cons [home_route, home_content] @@
      let@ resource = Eio.Fiber.List.map ~max_fibers: 40 @~ List.of_seq all_resources in
      let@ () = Reporter.easy_run in
      match resource with
      | T.Article article ->
        begin
          match article.frontmatter.uri with
          | None -> []
          | Some uri ->
            let path_components = Legacy_xml_client.local_path_components forest.config uri in
            let xml_route = String.concat "/" @@ path_components @ ["index.xml"] in
            let html_route = String.concat "/" @@ path_components @ ["index.html"] in
            let xml_content = Format.asprintf "%a" (Legacy_xml_client.pp_xml ~forest ~stylesheet: "default.xsl") article in
            let html_content = html_redirect "index.xml" in
            [xml_route, xml_content; html_route, html_content]
        end
      | T.Asset asset ->
        let route = URI.path_string @@ Legacy_xml_client.route forest asset.uri in
        [route, asset.content]
  in
  Logs.debug (fun m -> m "Writing %i files to output" (List.length jobs));
  begin
    (* Note: this part appears to be fast! *)
    let@ items = Eio.Fiber.List.iter ~max_fibers: 20 @~ jobs in
    let@ (route : string), content = List.iter @~ items in
    let@ () = Reporter.easy_run in
    let path = EP.(cwd / output_dir_name / route) in
    Eio_util.ensure_context_of_path ~perm: 0o755 path;
    EP.save ~create: (`Or_truncate 0o644) path content;
  end
