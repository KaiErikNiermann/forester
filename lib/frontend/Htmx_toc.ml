(*
 * SPDX-FileCopyrightText: 2026 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open struct
  module T = Types
end

open Pure_html
open HTML

type toc_config = {
  suffix: string;
  taxon: string;
  number: string;
  fallback_number: string;
  in_backmatter: bool;
  is_root: bool;
  implicitly_unnumbered: bool;
}

let default_toc_config
  ?(suffix = "")
  ?(taxon = "")
  ?(number = "")
  ?(fallback_number = "")
  ?(in_backmatter = false)
  ()
= {
  suffix;
  taxon;
  number;
  fallback_number;
  in_backmatter;
  is_root = false;
  implicitly_unnumbered = false;
}

let contextual_number (_tree : T.content T.section) (cfg : toc_config) =
  let should_number =
    cfg.number <> ""
    || (not cfg.in_backmatter)
    && (not cfg.is_root)
    && not cfg.implicitly_unnumbered
  in
  let taxon =
    if cfg.taxon <> "" then
      cfg.taxon ^ if should_number || cfg.fallback_number <> "" then " " else ""
    else ""
  in
  let number =
    if should_number then
      if cfg.number <> String.empty then cfg.number else assert false
    else if cfg.fallback_number <> String.empty then cfg.fallback_number
    else ""
  in
  let suffix =
    if cfg.taxon <> String.empty
      || cfg.fallback_number <> String.empty
      || should_number then cfg.suffix
    else ""
  in
  null [txt "%s %s %s" taxon suffix number]

let _tree_taxon_with_number (tree : T.content T.section) cfg =
  contextual_number tree cfg

let _render_toc_item
    ~render_content
    (forest : Forester_compiler.State.t)
    (item : T.content T.section)
  =
  let to_str =
    Plain_text_client.string_of_content
      ~forest
      ~router: (Legacy_xml_client.route forest)
  in
  null
    [
      a
        [class_ "bullet";
        href "";
        title_
          "%s%s"
          (
            Option.value ~default: "" @@
              Option.map to_str item.frontmatter.title
          )
          (
            Option.value ~default: "" @@
              Option.map (Format.asprintf "[%a]" URI.pp) item.frontmatter.uri
          );
        ]
        [txt "■"];
      span
        [class_ "link local"]
        [
          span
            [class_ "taxon"]
            [_tree_taxon_with_number item (default_toc_config ())];
        ];
      ul [] (render_content forest item.mainmatter);
    ]

let rec render_toc_mainmatter content =
  let (T.Content nodes) = content in
  ul [class_ "block"] @@
    let@ node = List.filter_map @~ nodes in
    match node with T.Section section -> Some (render_toc section) | _ -> None

and render_toc (section : T.content T.section) =
  if Some false
    = List.find_map
        (fun (key, value) ->
          if key = "toc" && value = T.Content [T.Text "true"] then Some true
          else None
        )
        section.frontmatter.metas then null []
  else
    nav
      [id "toc"; Hx.swap_oob "true"]
      [
        div
          [class_ "block"]
          [
            h1 [] [txt "Table of contents"];
            render_toc_mainmatter section.mainmatter;
          ];
      ]
