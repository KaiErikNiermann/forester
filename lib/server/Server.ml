(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend

module T = Types

module EP = Eio.Path

type theme = {
  stylesheet: string;
  htmx: string;
  js_bundle: string;
  font_dir: string;
  favicon: string;
}

let load_theme ~env theme_location =
  assert (List.length theme_location = 1);
  let base_dir = List.hd theme_location in
  let theme_dir = EP.(env#fs / base_dir / "theme") in
  let stylesheet = EP.(load (theme_dir / "style.css")) in
  let htmx = EP.(load (theme_dir / "htmx.js")) in
  let js_bundle = EP.(load (env#fs / base_dir / "min.js")) in
  let favicon = EP.(load (theme_dir / "favicon.ico")) in
  let font_dir = EP.(native_exn @@ theme_dir / "fonts") in
  {stylesheet; htmx; js_bundle; font_dir; favicon;}

let lookup_font ~env theme font =
  Eio.Path.(load (env#fs / theme.font_dir / font))

let handler
  : env: < fs: [> Eio.Fs.dir_ty] Eio.Path.t; .. > ->
  theme: theme ->
  forest: State.t ->
  Cohttp_eio.Server.conn ->
  Http.Request.t ->
  Cohttp_eio.Body.t ->
  Cohttp_eio.Server.response
= fun
    ~env
    ~theme
    ~forest
    _socket
    request
    body
  ->
  let resource = Uri.of_string request.resource in
  let path = Uri.path resource in
  match Routes.match' ~target: path Router.routes with
  | Routes.FullMatch r
  | Routes.MatchWithTrailingSlash r ->
    begin
      match r with
      | Font fontname ->
        let body = lookup_font ~env theme fontname in
        let headers =
          let ext = Filename.extension fontname in
          let mimetype =
            match ext with
            | ".ttf" -> "font/ttf"
            | ".woff" -> "font/woff"
            | ".woff2" -> "font/woff2"
            | _ -> assert false
          in
          Http.Header.of_list ["Content-Type", mimetype]
        in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body ()
      | Stylesheet ->
        let headers = Http.Header.of_list ["Content-Type", "text/css"; "charset", "utf-8"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.stylesheet ()
      | Js_bundle ->
        let headers = Http.Header.of_list ["Content-Type", "application/javascript"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.js_bundle ()
      | Index ->
        Cohttp_eio.Server.respond_string ~status: `OK ~body: (Pure_html.to_string (Index.v ())) ()
      | Favicon ->
        let headers = Http.Header.of_list ["Content-Type", "image/x-icon"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.favicon ()
      | Tree s ->
        let href = Iri_scheme.user_iri ~host: State.(forest.config.host) s in
        let request_headers = Http.Request.headers request in
        let is_htmx = Option.is_some @@ Http.Header.get request_headers "Hx-Request" in
        begin
          if is_htmx then
            (* If it is an HTMX request, we just send a fragment. *)
            begin
              match Headers.parse_content_target request_headers with
              | None ->
                begin
                  match Forest.get_article href forest.resources with
                  | None -> Cohttp_eio.Server.respond_string ~status: `Not_found ~body: "" ()
                  | Some content ->
                    let response =
                      Pure_html.(
                        to_string @@
                          (Htmx_client.render_article forest content)
                      )
                    in
                    Cohttp_eio.Server.respond_string ~status: `OK ~body: response ()
                end
              | Some target ->
                let modifier = Option.value ~default: T.Identity (Headers.parse_modifier request_headers) in
                match Forest.get_content_of_transclusion
                  {target; href; modifier;}
                  forest.resources with
                | None -> Cohttp_eio.Server.respond_string ~status: `Not_found ~body: "" ()
                | Some content ->
                  let response =
                    Pure_html.(
                      to_string @@
                        HTML.span [] (Htmx_client.render_content forest content)
                    )
                  in
                  Cohttp_eio.Server.respond_string ~status: `OK ~body: response ()
            end
          else
            (* If it is not an HTMX request, we need to send the whole page. *)
            match Forest.get_article href forest.resources with
            | Some article ->
              let content =
                Pure_html.to_string @@
                  Index.v
                    ~c: (Htmx_client.render_article forest article)
                    ()
              in
              let headers = Http.Header.of_list ["Content-Type", "text/html"] in
              Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: content ()
            | None -> Cohttp_eio.Server.respond_string ~status: `Not_found ~body: "" ()
        end
      | Search ->
        if request.meth = `POST then
          let body = Eio.Flow.read_all body in
          let search_term =
            String.concat "" @@
            snd @@
            List.find
              (fun (s, _) ->
                s = "search"
              )
              (Uri.query_of_encoded body)
          in
          let search_results = Forester_search.Index.search forest.search_index search_term in
          let response =
            List.concat_map
              (fun iris ->
                let open Pure_html in
                let open HTML in
                [ul [] (List.map (fun iri -> li [] [txt "%s" (Format.asprintf "%a" pp_iri iri)]) iris)]
              )
              search_results
          in
          Cohttp_eio.Server.respond_string ~status: `OK ~body: (Pure_html.to_string @@ Pure_html.HTML.ul [] response) ()
        else
          Cohttp_eio.Server.respond_string ~status: `Method_not_allowed ~body: "" ()
      | Searchmenu ->
        Cohttp_eio.Server.respond_string ~status: `OK ~body: Search_menu.v ()
      | Nil ->
        Cohttp_eio.Server.respond_string ~status: `OK ~body: "" ()
      | Home ->
        begin
          match forest.config.home with
          | None ->
            Cohttp_eio.Server.respond_string ~status: `OK ~body: "" ()
          | Some home ->
            let home = Iri_scheme.user_iri ~host: forest.config.host home in
            match Forest.get_article home forest.resources with
            | None ->
              Cohttp_eio.Server.respond_string ~status: `OK ~body: "" ()
            | Some home_tree ->
              let content = Pure_html.to_string @@ Htmx_client.render_article forest home_tree in
              let headers = Http.Header.of_list ["Content-Type", "text/html"] in
              Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: content ()
        end
      | Query ->
        let q = Uri.get_query_param resource "query" in
        let response =
          q
          |> Option.get
          |> Iri_types.pct_decode
          |> Repr.of_json_string
              Datalog_expr.(query_t Repr.string (T.vertex_t T.content_t)) |> function
            | Ok q ->
              Logs.app (fun m -> m "parsed successfully");
              let (_, _, result) = State_machine.update (Query q) forest in
              begin
                match result with
                | Vertex_set vs -> Htmx_client.render_query_result forest vs
                | Got _
                | Error _
                | Nothing ->
                  None
                (* Pure_html.txt "failed to run" *)
              end
            | Error (`Msg str) ->
              Logs.app (fun m -> m "failed to parse: %s" str);
              (* Pure_html.txt "failed to parse: %s" str *)
              None
        in
        begin
          match response with
          | Some nodes ->
            Cohttp_eio.Server.respond_string
              ~status: `OK
              ~body: (Format.asprintf "%a" Pure_html.pp nodes)
              ()
          | None ->
            (* If result is empty, use
               [hx-retarget](https://htmx.org/reference/#response_headers) to
               hide the entire section. Right now I am just trying to get the
               backmatter to render correctly, I don't know if this is
               compatible with the other use cases of queries. I can think of
               multiple ways to work around this. We could use a separate
               endpoint to get the backmatter, or we could do some more
               HTMXing. I guess the question boils down to which approach is
               more in line with our overarching goal of making forester a
               genuine hypermedia format
               *)
            let headers =
              Http.Header.of_list
                [
                  "Hx-Retarget", "closest section.backmatter-section";
                  "Hx-Swap", "delete"
                ]
            in
            Cohttp_eio.Server.respond_string
              ~headers
              ~status: `OK
              ~body: ""
              ()
        end
      | Htmx ->
        let headers = Http.Header.of_list ["Content-Type", "application/javascript"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.htmx ()
    end
  | Routes.NoMatch ->
    Cohttp_eio.Server.respond_string ~status: `Not_found ~body: "" ()

let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

let run ~env ~port ~forest theme_location =
  let@ sw = Eio.Switch.run ?name: None in
  let port = ref port in
  let theme = load_theme ~env theme_location in
  let socket =
    Eio.Net.listen
      env#net
      ~sw
      ~backlog: 128
      ~reuse_addr: true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
  and server = Cohttp_eio.Server.make ~callback: (handler ~env ~theme ~forest) ()
  in
  Cohttp_eio.Server.run
    socket
    server
    ~on_error: log_warning
