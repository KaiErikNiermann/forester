(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* TODO: should just use cram tests for this instead. *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forester_frontend

module T = Types
module HTML = Pure_html.HTML

let config = {Config.default with trees = ["transclude"]; host = "test"}
let host = config.host

let href = Iri_scheme.user_iri ~host "transcludee"

module Transclusions = struct
  (* It would be cool to use quickcheck here, but no good way to test the result*)
  open T
  let full_default = {
    href;
    target = Full default_section_flags;
    modifier = Identity
  }

  let metadata_shown = {default_section_flags with
    metadata_shown =
    Some true
  }
end

let () =
  let@ env = Eio_main.run in
  Logs.set_level (Some Debug);
  let@ () = Reporter.easy_run in
  let iri = Iri_scheme.user_iri ~host "transcludee" in
  let resources = Iri_tbl.create 10 in
  Iri_tbl.add
    resources
    iri
    (
      T.(
        Article
          {
            frontmatter =
            default_frontmatter
              ~iri: (Iri.of_string "forest://test/transcludee")
              ~title: (Content [Text "I am being transcluded"])
              ();
            mainmatter = Content [Text "Hello"];
            backmatter = Content []
          }
      )
    );
  let forest = {(State.make ~env ~config ~dev: false ()) with resources} in
  let print_transclusion : T.transclusion -> unit = fun t ->
    let content = Option.get @@ Forest.get_content_of_transclusion t forest.resources in
    Format.printf
      "%a"
      Legacy_xml_client.(pp_xml ~forest ?stylesheet: None)
      (
        T.{
          frontmatter = default_frontmatter ~iri: href ();
          mainmatter = content;
          backmatter = Content []
        }
      )
  in
  let test_full_default () =
    print_transclusion
      Transclusions.full_default
  in
  let test_title_default () =
    print_transclusion
      {
        href = iri;
        target = Title {empty_when_untitled = false};
        modifier = Identity
      }
  in
  let test_full_metadata () =
    print_transclusion
      {
        href = iri;
        target = Full Transclusions.metadata_shown;
        modifier = Identity
      }
  in
  List.iter
    (fun f -> f ())
    [
      test_full_default;
      test_title_default;
      test_full_metadata
    ]
