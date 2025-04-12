(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_search
open struct
  module T = Forester_core.Types
  module Trie = Yuujinchou.Trie
end

let doc1 =
  T.{
    frontmatter =
    default_frontmatter
      ~uri: (URI.of_string_exn "forest://doc1")
      ~title: (T.Content [T.Text "Title of tremendous importance"])
      ();
    mainmatter =
    T.Content [T.Text "A donut on a glass plate. Only the donuts."];
    backmatter = T.Content [];
  }

let doc2 =
  T.{
    frontmatter = default_frontmatter ~uri: (URI.of_string_exn "forest://doc2") ();
    mainmatter =
    T.Content [T.Text "donut is a donut"];
    backmatter = T.Content [];
  }

let pp_pair pp_fst pp_snd out (x, y) =
  Format.fprintf out "(%a, %a)" pp_fst x pp_snd y

let test_context path doc =
  Format.(
    printf
      "word at path %a: %s@."
      (pp_print_list pp_print_int)
      path
      (Context.render_context_article path doc)
  )

let test_nth_word () =
  let corpus =
    "Far far away, behind the word mountains, far from the countries Vokalia and Consonantia, there live the blind texts. Separated they live in Bookmarksgrove right at the coast of the Semantics, a large language ocean. A small river named Duden flows by their place and supplies it with the necessary regelialia. It is a paradisematic country, in which roasted parts of sentences fly into your mouth. Even the all-powerful Pointing has no control about the blind texts it is an almost unorthographic life One day however a small line of blind text by the name of Lorem Ipsum decided to leave for the far World of Grammar. The Big Oxmox advised her not to do so, because there were thousands of bad Commas, wild Question Marks and devious Semikoli, but the Little Blind Text didn’t listen. She packed her seven versalia, put her initial into the belt and made herself on the way. When she reached the first hills of the Italic Mountains, she had a last view back on the skyline of her hometown Bookmarksgrove, the headline of Alphabet Village and the subline of her own road, the Line Lane. Pityful a rethoric question ran over her cheek, then"
  in
  let tokens = Tokenizer.tokenize corpus in
  List.iteri
    (fun i token ->
      Alcotest.(check string)
        "Using get_nth_word and stemming the word should be the same as the nth
        element of tokens. Will be used to render the context of a token."
        (
          Context.get_nth_word i corpus
          |> String.lowercase_ascii
          |> Stemming.stem
        )
        token
    )
    tokens

let test_tokenize_content () =
  let content =
    let open Forester_frontend.DSL in
    T.Content
      [
        p
          [
            ol
              [
                li [txt "First item"];
                li [txt "Second item"]
              ];
            ul
              [
                li [txt "First item in second list"];
                li [txt "Second item in second list"]
              ];
          ]
      ]
  in
  let tokens = Tokenizer.tokenize_content [] In_mainmatter content in
  let locations = List.map (fun (x, y) -> List.rev x) tokens in
  let render path = Context.render_context_content path content in
  Alcotest.(check @@ list string)
    ""
    [
      "first";
      "item";
      "second";
      "item";
      "first";
      "item";
      "second";
      "list";
      "second";
      "item";
      "second";
      "list"
    ]
    (List.map render locations)

let test_render_context_frontmatter () =
  let open Forester_frontend.DSL in
  let frontmatter =
    T.default_frontmatter
      ~uri: (URI.of_string_exn "forest://test/asdf")
      ~source_path: "/foo/bar"
      ~taxon: (T.Content [T.Text "Hello"])
      ~title: (T.Content [txt "title"; katex Display [txt "a=b"]])
      ~attributions: [
        {
          role = T.Author;
          vertex = Uri_vertex (URI.of_string_exn "forest://test/kentookura");
        };
        {
          role = T.Contributor;
          vertex = Uri_vertex (URI.of_string_exn "forest://test/jonmsterling")
        }
      ]
      ()
  in
  let tokens = Tokenizer.tokenize_frontmatter [] frontmatter in
  let locations = List.map (fun (x, y) -> List.rev x) tokens in
  let render path = Context.render_context_frontmatter path frontmatter in
  Alcotest.(check @@ list string)
    ""
    ["title"; "hello"]
    (List.map render locations)

let test_ranking () =
  Alcotest.(check string)
    ""
    ""
    ""

let () =
  let open Alcotest in
  run
    "Test_forester_search"
    [
      "tokenizer",
      [
        test_case "get_nth_word" `Quick test_nth_word;
        test_case "tokenize_content" `Quick test_tokenize_content;
      ];
      "context",
      [
        test_case "render_context_frontmatter" `Quick test_render_context_frontmatter;
      ];
    ]

(* let () = *)
(*   let forest = [doc1; doc2] in *)
(*   print_endline "Tokens:"; *)
(*   forest *)
(*   |> List.iter *)
(*       (fun doc -> *)
(*         print_endline @@ *)
(*           Format.( *)
(*             asprintf *)
(*               "%a %a@." *)
(*               URI.pp *)
(*               T.(Option.get doc.frontmatter.uri) *)
(*               ( *)
(*                 pp_print_list *)
(*                   ~pp_sep: (fun out () -> fprintf out ", ") *)
(*                   ( *)
(*                     pp_pair *)
(*                       (pp_print_list pp_print_int) *)
(*                       pp_print_string *)
(*                   ) *)
(*               ) *)
(*               (Tokenizer.tokenize_article doc) *)
(*           ) *)
(*       ); *)
(*   let index = Index.of_list forest in *)
(*   index *)
(*   |> Spelll.Index.iter *)
(*       (fun term uris -> *)
(*         let ocurrences = *)
(*           List.map *)
(*             (fun (x, y) -> y, x) *)
(*             (Index.Ocurrences.to_list uris) *)
(*         in *)
(*         Format.( *)
(*           printf *)
(*             "%s: %a@." *)
(*             term *)
(*             ( *)
(*               pp_print_list *)
(*                 ~pp_sep: (fun out () -> fprintf out ", ") *)
(*                 ( *)
(*                   pp_pair *)
(*                     URI.pp *)
(*                     (pp_print_list @@ pp_print_list ~pp_sep: (fun out () -> fprintf out "->") pp_print_int) *)
(*                 ) *)
(*             ) *)
(*             ocurrences *)
(*         ) *)
(*       ); *)
(*   (* Format.printf "\nSearch for donus:@."; *) *)
(*   (* List.concat_map Index.Ocurrences.to_list @@ *) *)
(*   (*   Spelll.Index.retrieve_l ~limit: 1 index "donus" *) *)
(*   (* |> List.iter (fun (_, uri) -> Format.printf "%a@." URI.pp uri); *) *)
(*   test_context [0; 1; 0; 2;] doc1; *)
(*   test_context [1; 0; 0;] doc2; *)
