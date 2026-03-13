(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_test
open Testables
open Forester_core
open Forester_frontend

let test_parsing () =
  Alcotest.(check config)
    "is the same"
    Config.{trees = ["trees"];
      assets = [];
      url = URI.of_string_exn "https://www.forester-notes.org/";
      home = URI.of_string_exn "https://www.forester-notes.org/index/";
      foreign = [{path = "foreign/forest.json"; route_locally = true; include_in_manifest = true}];
      latex = Config.default_latex;
    }
    begin
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        trees = ["trees"]
        foreign = [{path = "foreign/forest.json"}]
        url = "https://www.forester-notes.org/"
        home = "index"
        |}
    end

let test_missing_fields () =
  Alcotest.(check config)
    "is the same"
    Config.{trees = ["trees"];
      assets = [];
      foreign = [];
      url = URI.of_string_exn "/";
      home = URI.of_string_exn "/index/";
      latex = Config.default_latex;
    }
    (
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        trees = ["trees"]
        url = "/"
        |}
    )

let test_custom_latex_settings () =
  let expected_latex =
    Config.{document_class = "article";
      document_class_options = ["12pt"; "draft"];
      compile_command = ["tectonic"; "-Z1"];
      dvisvgm_command = ["dvisvgm"; "--pdf"];
    }
  in
  Alcotest.(check config)
    "parses custom latex block"
    Config.{trees = ["trees"];
      assets = [];
      foreign = [];
      url = URI.of_string_exn "/";
      home = URI.of_string_exn "/index/";
      latex = expected_latex;
    }
    (
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        trees = ["trees"]
        url = "/"

        [forest.latex]
        document_class = "article"
        document_class_options = ["12pt", "draft"]
        compile_command = ["tectonic", "-Z1"]
        dvisvgm_command = ["dvisvgm", "--pdf"]
        |}
    )

let () =
  let open Alcotest in
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs.format_reporter ());
  run
    "Config parsing"
    [
      "example config works",
      [test_case "it parses correctly" `Quick test_parsing;
      ];
      "can parse config with missing fields",
      [test_case "" `Quick test_missing_fields;
      ];
      "custom latex settings",
      [test_case "" `Quick test_custom_latex_settings;
      ];
    ]
