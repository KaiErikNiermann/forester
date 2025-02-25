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
  index: string;
  js_bundle: string;
  font_dir: string;
  favicon: string;
}

let load_theme ~env theme_location =
  assert (List.length theme_location = 1);
  let base_dir = List.hd theme_location in
  let theme_dir = EP.(env#fs / base_dir / "theme") in
  let stylesheet = EP.(load (theme_dir / "style.css")) in
  let index = EP.(load (theme_dir / "index.html")) in
  let js_bundle = EP.(load (env#fs / base_dir / "min.js")) in
  let favicon = EP.(load (theme_dir / "favicon.ico")) in
  let font_dir = EP.(native_exn @@ theme_dir / "fonts") in
  {stylesheet; index; js_bundle; font_dir; favicon;}

let lookup_font ~env theme font =
  Eio.Path.(load (env#fs / theme.font_dir / font))

let handler
  : env: < fs: [> Eio.Fs.dir_ty] Eio.Path.t; .. > ->
  theme: theme ->
  forest: _ ->
  'a ->
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
        let headers = Http.Header.of_list ["Content-Type", "text/css"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.stylesheet ()
      | Js_bundle ->
        let headers = Http.Header.of_list ["Content-Type", "application/javascript"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.js_bundle ()
      | Index ->
        Cohttp_eio.Server.respond_string ~status: `OK ~body: theme.index ()
      | Favicon ->
        let headers = Http.Header.of_list ["Content-Type", "image/x-icon"] in
        Cohttp_eio.Server.respond_string ~headers ~status: `OK ~body: theme.favicon ()
      | Tree s ->
        let iri = Iri_scheme.user_iri ~host: State.(forest.config.host) s in
        begin
          match Forest.get_article iri forest.resources with
          | None -> Cohttp_eio.Server.respond_string ~status: `Not_found ~body: "" ()
          | Some article ->
            let content =
              Render.(
                Format.asprintf
                  "%a"
                  (pp ~dev: forest.dev forest HTML)
                  (Article article)
              )
            in
            Cohttp_eio.Server.respond_string ~status: `OK ~body: content ()
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
          Cohttp_eio.Server.respond_string ~status: `OK ~body: (Format.asprintf "%s" search_term) ()
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
              begin
                let content =
                  Render.(
                    Format.asprintf
                      "%a"
                      (pp ~dev: forest.dev forest HTML)
                      (Article home_tree)
                  )
                in
                Cohttp_eio.Server.respond_string ~status: `OK ~body: content ()
              end
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
                | Vertex_set vs ->
                  Htmx_client.render_query_result forest vs
                | Render_result _
                | Error _
                | Nothing ->
                  [Pure_html.txt "failed to run"]
              end
            | Error (`Msg str) ->
              [Pure_html.txt "failed to parse: %s" str]
        in
        Cohttp_eio.Server.respond_string
          ~status: `OK
          ~body: (
            Format.asprintf
              "%a"
              Pure_html.pp
              (Pure_html.HTML.ul [] response)
          )
          ()
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
