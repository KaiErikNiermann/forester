(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_test
open Testables
open Forester_compiler
open Forester_frontend

let extra_remarks_to_strings remarks =
  List.map (fun Asai.Range.{value; _} -> Asai.Diagnostic.string_of_text value) @@ Bwd.Bwd.to_list remarks

let test_parsing () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      prefixes = ["foo"; "bar"; "baz"];
      assets = [];
      base_url = "http://www.forester-notes.org/";
      foreign = ["foreign/forest.json"];
      theme = "theme";
    }
    begin
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        host = "test"
        trees = ["trees"]
        prefixes = ["foo", "bar", "baz"]
        foreign = ["foreign/forest.json"]

        [renderer]
        home = "index"
        base_url = "http://www.forester-notes.org/"
        |}
    end

let test_missing_fields () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      theme = "theme";
      assets = [];
      foreign = [];
      base_url = "/";
      prefixes = [];
    }
    (
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        host = "test"
        trees = ["trees"]
        [renderer]
        home = "index"
        |}
    )

let test_missing_host () =
  Alcotest.(check unit)
    "is the same"
    ()
    begin
      let fatal = function
        | {message; extra_remarks; _} ->
          Alcotest.(check Testables.message) "" Configuration_error message;
          Alcotest.(check @@ list string)
            ""
            ["You need to set the `host' key in your configuration file; this is a global identifier that will be used to distinguish your forest from other forests (you can use your name, e.g. `johnqpublic')"]
            (extra_remarks_to_strings extra_remarks)
      in
      let emit = function
        | {message; _} ->
          Alcotest.(check Testables.message) "" Configuration_error message
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      let _ =
        Config_parser.parse_forest_config_string
          {|
        [forest]
        trees = ["trees"]
        [renderer]
        home = "index"
        |}
      in
      assert false
    end

let test_parse_error () =
  Alcotest.(check unit)
    "is the same"
    ()
    begin
      Forester_core.Reporter.run
        ~fatal: (function
          | {extra_remarks; _} ->
            Alcotest.(check @@ list string)
              ""
              ["Error in <anonymous> at line 2 at column 11 (position 12)"]
              (extra_remarks_to_strings extra_remarks);
        (* (Asai.Diagnostic.string_of_text explanation.value) *)
        )
        ~emit: (fun _ -> ())
        @@ fun () ->
        let _ =
          Config_parser.parse_forest_config_string
            {|
          ]]]
        |}
        in
        assert false
    end

let test_stylesheet_warning () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      theme = "theme";
      base_url = "/";
      assets = [];
      foreign = [];
      prefixes = [];
    }
    begin
      let fatal _ = assert false in
      let emit = function
        | {extra_remarks; _} ->
          (
            Alcotest.(check @@ list string)
              ""
              ["Custom XSL stylesheet injection is no longer supported; please remove the `stylesheet' key from the [forest] group."]
              (extra_remarks_to_strings extra_remarks);
          )
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      Config_parser.parse_forest_config_string
        {|[forest]
        host = "test"
        trees = ["trees"]
        stylesheet = "custom.xsl"
        [renderer]
        home = "index"
        |}
    end

let test_root_warning () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = None;
      theme = "theme";
      base_url = "/";
      assets = [];
      foreign = [];
      prefixes = [];
    }
    begin
      let fatal _ = assert false in
      let emit = function
        | {extra_remarks; _} ->
          Alcotest.(check @@ list string)
            ""
            ["In your configuration file, change `root' key to `home' in the [forest] group."]
            (extra_remarks_to_strings extra_remarks)
      (* (Asai.Diagnostic.string_of_text explanation.value) *)
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      Config_parser.parse_forest_config_string
        {|[forest]
        host = "test"
        trees = ["trees"]
        root = "index"
        |}
    end

let () =
  let open Alcotest in
  run
    "Config parsing"
    [
      "example config works",
      [
        test_case "it parses correctly" `Quick test_parsing;
      ];
      "can parse config with missing fields",
      [
        test_case "" `Quick test_missing_fields;
      ];
      "handles errors correctly",
      [
        test_case "" `Quick test_missing_host;
        test_case "" `Quick test_parse_error;
        test_case "" `Quick test_stylesheet_warning;
        test_case "" `Quick test_root_warning;
      ];
    ]
