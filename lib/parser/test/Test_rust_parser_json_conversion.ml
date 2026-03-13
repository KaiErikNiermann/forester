(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Forester_test
open Forester_core
open Testables

module Rust_parser = Forester_parser.Rust_parser

let str value = `String value
let int value = `Int value
let list values = `List values
let obj fields = `Assoc fields

let node ty fields = obj (("type", str ty) :: fields)
let located ?span value =
  obj
    (
      ("value", value) ::
      match span with None -> [] | Some span_json -> [("span", span_json)]
    )

let text_json content = node "text" [("content", str content)]
let verbatim_json content = node "verbatim" [("content", str content)]
let comment_json content = node "comment" [("content", str content)]
let error_json message = node "error" [("message", str message)]
let ident_json path = node "ident" [("path", list (List.map str path))]
let hash_ident_json name = node "hash_ident" [("name", str name)]

let xml_ident_json ?prefix name =
  node
    "xml_ident"
    [
      (
        "prefix",
        match prefix with None -> `Null | Some prefix_value -> str prefix_value
      );
      ("name", str name);
    ]

let binding_json info name = list [str info; str name]
let nodes_json values = list (List.map located values)

let group_json delim body =
  node "group" [("delim", str delim); ("body", nodes_json body)]

let math_json mode body =
  node "math" [("mode", str mode); ("body", nodes_json body)]

let method_json name body = list [str name; nodes_json body]

let loc value = ({Range.loc = None; value}: Code.node Range.located)
let code_text value = loc (Code.Text value)
let code_verbatim value = loc (Code.Verbatim value)
let code_comment value = loc (Code.Comment value)
let code_error value = loc (Code.Error value)
let code_ident path = loc (Code.Ident path)
let code_hash_ident name = loc (Code.Hash_ident name)
let code_xml_ident prefix name = loc (Code.Xml_ident (prefix, name))
let code_group delim body = loc (Code.Group (delim, body))
let code_math mode body = loc (Code.Math (mode, body))
let code_import visibility target = loc (Code.Import (visibility, target))

let span_json
    ~start_offset
    ~start_line
    ~start_column
    ~end_offset
    ~end_line
    ~end_column
  =
  obj
    [
      (
        "start",
        obj
          [
            ("offset", int start_offset);
            ("line", int start_line);
            ("column", int start_column);
          ]
      );
      (
        "end",
        obj
          [
            ("offset", int end_offset);
            ("line", int end_line);
            ("column", int end_column);
          ]
      );
    ]

let exhaustive_document_json () =
  obj
    [
      ("status", str "ok");
      (
        "document",
        obj
          [
            (
              "nodes",
              list
                [
                  located (text_json "alpha");
                  located (verbatim_json "literal");
                  located (comment_json "note");
                  located (error_json "bad");
                  located (group_json "braces" [text_json "inside"]);
                  located (group_json "squares" [ident_json ["slot"]]);
                  located (group_json "parens" [text_json "paren"]);
                  located (math_json "inline" [text_json "x + y"]);
                  located (math_json "display" [text_json "z"]);
                  located (ident_json ["alpha"; "beta"]);
                  located (hash_ident_json "topic");
                  located (xml_ident_json ~prefix: "svg" "path");
                  located
                    (
                      node
                        "let"
                        [
                          ("path", list [str "value"]);
                          (
                            "bindings",
                            list
                              [binding_json "lazy" "x"; binding_json "strict" "y"]
                          );
                          ("body", nodes_json [text_json "body"]);
                        ]
                    );
                  located
                    (
                      node
                        "def"
                        [
                          ("path", list [str "macro"]);
                          ("bindings", list [binding_json "strict" "arg"]);
                          ("body", nodes_json [text_json "result"]);
                        ]
                    );
                  located
                    (
                      node
                        "fun"
                        [
                          ("bindings", list [binding_json "lazy" "param"]);
                          ("body", nodes_json [text_json "lambda"]);
                        ]
                    );
                  located (node "scope" [("body", nodes_json [text_json "hidden"])]);
                  located
                    (
                      node
                        "namespace"
                        [
                          ("path", list [str "docs"; str "api"]);
                          ("body", nodes_json [ident_json ["title"]; group_json "braces" [text_json "API"]]);
                        ]
                    );
                  located (node "open" [("path", list [str "store"; str "item"])]);
                  located
                    (
                      node
                        "put"
                        [
                          ("path", list [str "store"; str "item"]);
                          ("body", nodes_json [verbatim_json "payload"]);
                        ]
                    );
                  located
                    (
                      node
                        "default"
                        [
                          ("path", list [str "store"; str "fallback"]);
                          ("body", nodes_json [text_json "default"]);
                        ]
                    );
                  located (node "get" [("path", list [str "store"; str "item"])]);
                  located (node "alloc" [("path", list [str "store"; str "slot"])]);
                  located
                    (
                      node
                        "object"
                        [
                          (
                            "def",
                            obj
                              [
                                ("self_name", str "self");
                                (
                                  "methods",
                                  list
                                    [
                                      method_json "render" [text_json "body"];
                                      method_json "raw" [verbatim_json "literal"];
                                    ]
                                );
                              ]
                          );
                        ]
                    );
                  located
                    (
                      node
                        "patch"
                        [
                          (
                            "def",
                            obj
                              [
                                ("obj", nodes_json [ident_json ["base"]]);
                                ("self_name", str "self");
                                ("super_name", str "super");
                                ("methods", list [method_json "render" [text_json "override"]]);
                              ]
                          );
                        ]
                    );
                  located
                    (
                      node
                        "call"
                        [
                          (
                            "target",
                            nodes_json
                              [ident_json ["ref"]; group_json "braces" [text_json "alpha"]]
                          );
                          ("method", str "render");
                        ]
                    );
                  located
                    (
                      node
                        "subtree"
                        [
                          ("addr", str "appendix / notes");
                          ("body", nodes_json [text_json "child"]);
                        ]
                    );
                  located (node "import" [("visibility", str "private"); ("target", str "foundation / intro")]);
                  located (node "import" [("visibility", str "public"); ("target", str "public/tree")]);
                  located (node "decl_xmlns" [("prefix", str "svg"); ("uri", str "https://example.com/svg")]);
                  located
                    (
                      node
                        "dx_sequent"
                        [
                          (
                            "conclusion",
                            nodes_json
                              [
                                node
                                  "dx_prop"
                                  [
                                    ("relation", nodes_json [ident_json ["rel"; "links-to"]]);
                                    ("args", list [nodes_json [node "dx_var" [("name", str "X")]]]);
                                  ];
                              ]
                          );
                          (
                            "premises",
                            list
                              [
                                nodes_json
                                  [
                                    node
                                      "dx_prop"
                                      [
                                        ("relation", nodes_json [ident_json ["rel"; "is-node"]]);
                                        ("args", list [nodes_json [node "dx_var" [("name", str "X")]]]);
                                      ];
                                  ];
                              ]
                          );
                        ]
                    );
                  located
                    (
                      node
                        "dx_query"
                        [
                          ("var", str "Result");
                          (
                            "positives",
                            list
                              [
                                nodes_json
                                  [
                                    node
                                      "dx_prop"
                                      [
                                        ("relation", nodes_json [ident_json ["rel"; "keep"]]);
                                        ("args", list [nodes_json [node "dx_var" [("name", str "Result")]]]);
                                      ];
                                  ];
                              ]
                          );
                          (
                            "negatives",
                            list
                              [
                                nodes_json
                                  [
                                    node
                                      "dx_prop"
                                      [
                                        ("relation", nodes_json [ident_json ["rel"; "hide"]]);
                                        ("args", list [nodes_json [node "dx_var" [("name", str "Result")]]]);
                                      ];
                                  ];
                              ]
                          );
                        ]
                    );
                  located
                    (
                      node
                        "dx_prop"
                        [
                          ("relation", nodes_json [ident_json ["rel"; "tagged"]]);
                          (
                            "args",
                            list
                              [
                                nodes_json [node "dx_var" [("name", str "X")]];
                                nodes_json [node "dx_const_content" [("body", nodes_json [text_json "tag"])]];
                                nodes_json [node "dx_const_uri" [("body", nodes_json [text_json "https://example.com"])]];
                              ]
                          );
                        ]
                    );
                  located (node "dx_var" [("name", str "Y")]);
                  located (node "dx_const_content" [("body", nodes_json [text_json "body"])]);
                  located (node "dx_const_uri" [("body", nodes_json [text_json "uri"])]);
                ]
            );
          ]
      );
    ]

let exhaustive_expected_code () : Code.t = [
  code_text "alpha";
  code_verbatim "literal";
  code_comment "note";
  code_error "bad";
  code_group Braces [code_text "inside"];
  code_group Squares [code_ident ["slot"]];
  code_group Parens [code_text "paren"];
  code_math Inline [code_text "x + y"];
  code_math Display [code_text "z"];
  code_ident ["alpha"; "beta"];
  code_hash_ident "topic";
  code_xml_ident (Some "svg") "path";
  loc (Code.Let (["value"], [(Lazy, "x"); (Strict, "y")], [code_text "body"]));
  loc (Code.Def (["macro"], [(Strict, "arg")], [code_text "result"]));
  loc (Code.Fun ([(Lazy, "param")], [code_text "lambda"]));
  loc (Code.Scope [code_text "hidden"]);
  loc (Code.Namespace (["docs"; "api"], [code_ident ["title"]; code_group Braces [code_text "API"]]));
  loc (Code.Open ["store"; "item"]);
  loc (Code.Put (["store"; "item"], [code_verbatim "payload"]));
  loc (Code.Default (["store"; "fallback"], [code_text "default"]));
  loc (Code.Get ["store"; "item"]);
  loc (Code.Alloc ["store"; "slot"]);
  loc (Code.Object {self = Some "self"; methods = [("render", [code_text "body"]); ("raw", [code_verbatim "literal"])]});
  loc (Code.Patch {obj = [code_ident ["base"]]; self = Some "self"; super = Some "super"; methods = [("render", [code_text "override"])]});
  loc (Code.Call ([code_ident ["ref"]; code_group Braces [code_text "alpha"]], "render"));
  loc (Code.Subtree (Some "appendix / notes", [code_text "child"]));
  code_import Private "foundation / intro";
  code_import Public "public/tree";
  loc (Code.Decl_xmlns ("svg", "https://example.com/svg"));
  loc
    (
      Code.Dx_sequent
        (
          [loc (Code.Dx_prop ([code_ident ["rel"; "links-to"]], [[loc (Code.Dx_var "X")]]))],
          [[loc (Code.Dx_prop ([code_ident ["rel"; "is-node"]], [[loc (Code.Dx_var "X")]]))]]
        )
    );
  loc
    (
      Code.Dx_query
        (
          "Result",
          [[loc (Code.Dx_prop ([code_ident ["rel"; "keep"]], [[loc (Code.Dx_var "Result")]]))]],
          [[loc (Code.Dx_prop ([code_ident ["rel"; "hide"]], [[loc (Code.Dx_var "Result")]]))]]
        )
    );
  loc
    (
      Code.Dx_prop
        (
          [code_ident ["rel"; "tagged"]],
          [[loc (Code.Dx_var "X")];
          [loc (Code.Dx_const_content [code_text "tag"])];
          [loc (Code.Dx_const_uri [code_text "https://example.com"])];
          ]
        )
    );
  loc (Code.Dx_var "Y");
  loc (Code.Dx_const_content [code_text "body"]);
  loc (Code.Dx_const_uri [code_text "uri"]);
]

let test_parse_outcome_of_json_decodes_all_node_variants () =
  match Rust_parser.parse_outcome_of_json
    "ignored input"
    (exhaustive_document_json ()) with
  | Rust_parser.Parsed decoded_code ->
    Alcotest.(check Testables.code)
      "decoded exhaustive node set"
      (exhaustive_expected_code ())
      decoded_code
  | Rust_parser.Recovered (_, errors) ->
    Alcotest.failf
      "Expected Parsed outcome, got Recovered with %d errors"
      (List.length errors)
  | Rust_parser.Failed errors ->
    Alcotest.failf
      "Expected Parsed outcome, got Failed with %d errors"
      (List.length errors)

let test_located_node_of_json_preserves_spans () =
  let source = Rust_parser.source_of_input ~source_path: "synthetic.tree" "hello" in
  let span =
    span_json
      ~start_offset: 0
      ~start_line: 1
      ~start_column: 1
      ~end_offset: 5
      ~end_line: 1
      ~end_column: 6
  in
  let decoded =
    Rust_parser.located_node_of_json ~source (located ~span (text_json "hello"))
  in
  Alcotest.(check code_node) "decoded node value" (Code.Text "hello") decoded.value;
  match decoded.loc with
  | None -> Alcotest.fail "Expected decoded node to carry a location"
  | Some loc ->
    let expected = Rust_parser.range_of_json ~source span in
    Alcotest.(check bool) "decoded location" true (loc = expected)

let test_unknown_node_type_maps_to_code_error () =
  let source = Rust_parser.source_of_input "ignored" in
  let decoded =
    Rust_parser.node_of_json ~source (node "mystery" [("payload", str "x")])
  in
  Alcotest.(check code_node)
    "unknown node type"
    (Code.Error "Unknown Rust AST node type: mystery")
    decoded

let test_parse_outcome_of_json_handles_recovered_and_unknown_status () =
  let recovered_json =
    obj
      [
        ("status", str "recovered");
        ("document", obj [("nodes", nodes_json [text_json "tail"])]);
        (
          "errors",
          list
            [
              obj
                [
                  ("message", str "synthetic error");
                  ("start_offset", int 1);
                  ("end_offset", int 2);
                  ("report", str "Error: synthetic error");
                  ("details", obj [("kind", str "synthetic_kind")]);
                ];
            ]
        );
      ]
  in
  (
    match Rust_parser.parse_outcome_of_json "tail" recovered_json with
    | Rust_parser.Recovered (decoded_code, [error]) ->
      Alcotest.(check Testables.code)
        "recovered code"
        [code_text "tail"]
        decoded_code;
      Alcotest.(check string)
        "recovered error message"
        "synthetic error"
        error.message;
      Alcotest.(check int) "recovered start_offset" 1 error.start_offset;
      Alcotest.(check int) "recovered end_offset" 2 error.end_offset;
      Alcotest.(check string)
        "recovered report"
        "Error: synthetic error"
        error.report;
      (
        match error.details with
        | None -> Alcotest.fail "Expected recovered error details"
        | Some details ->
          Alcotest.(check string) "details kind" "synthetic_kind" details.kind;
          Alcotest.(check (list string)) "details expected" [] details.expected;
          Alcotest.(check (option string)) "details found" None details.found;
          Alcotest.(check int) "details labels" 0 (List.length details.labels);
          Alcotest.(check (list string)) "details notes" [] details.notes
      )
    | Rust_parser.Parsed _ -> Alcotest.fail "Expected Recovered outcome"
    | Rust_parser.Recovered (_, errors) ->
      Alcotest.failf "Expected one recovered error, got %d" (List.length errors)
    | Rust_parser.Failed errors ->
      Alcotest.failf
        "Expected Recovered outcome, got Failed with %d errors"
        (List.length errors)
  );
  match Rust_parser.parse_outcome_of_json
    "ignored"
    (obj [("status", str "mystery"); ("errors", list [])]) with
  | Rust_parser.Failed [error] ->
    Alcotest.(check string)
      "unknown status message"
      "Unknown status: mystery"
      error.message
  | Rust_parser.Failed errors ->
    Alcotest.failf
      "Expected one unknown-status error, got %d"
      (List.length errors)
  | Rust_parser.Parsed _ | Rust_parser.Recovered _ ->
    Alcotest.fail "Expected unknown status to decode as Failed"

let () =
  Alcotest.run
    "Rust parser JSON conversion"
    [
      (
        "json-conversion",
        [Alcotest.test_case
          "decodes all node variants"
          `Quick
          test_parse_outcome_of_json_decodes_all_node_variants;
        Alcotest.test_case
          "preserves span ranges"
          `Quick
          test_located_node_of_json_preserves_spans;
        Alcotest.test_case
          "maps unknown node types to Code.Error"
          `Quick
          test_unknown_node_type_maps_to_code_error;
        Alcotest.test_case
          "handles recovered and unknown statuses"
          `Quick
          test_parse_outcome_of_json_handles_recovered_and_unknown_status;
        ]
      );
    ]
