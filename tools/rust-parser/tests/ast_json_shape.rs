// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::BTreeSet;

use forester_rust_parser::{
    BindingInfo, Delim, Located, MathMode, Node, ObjectDef, PatchDef, Visibility,
};

fn assert_node_shape(node: Node, expected_type: &str, expected_fields: &[&str]) {
    let json =
        serde_json::to_value(Located::without_span(node.clone())).expect("serialize located node");
    let reparsed: Located<Node> =
        serde_json::from_value(json.clone()).expect("round-trip located node");
    assert_eq!(reparsed.value, node);

    let value = json
        .get("value")
        .and_then(serde_json::Value::as_object)
        .expect("located node should contain value object");

    assert_eq!(
        value
            .get("type")
            .and_then(serde_json::Value::as_str)
            .expect("node type tag"),
        expected_type
    );

    let actual_fields: BTreeSet<&str> = value
        .keys()
        .filter_map(|key| (key != "type").then_some(key.as_str()))
        .collect();
    let expected_fields: BTreeSet<&str> = expected_fields.iter().copied().collect();

    assert_eq!(actual_fields, expected_fields);
}

#[test]
fn node_json_shape_matches_ocaml_bridge_contract() {
    let sample_binding = vec![(BindingInfo::Lazy, "x".to_string())];
    let sample_body = vec![Located::without_span(Node::Text {
        content: "body".to_string(),
    })];
    let sample_relation = vec![Located::without_span(Node::Ident {
        path: vec!["rel".to_string()],
    })];

    assert_node_shape(
        Node::Text {
            content: "text".to_string(),
        },
        "text",
        &["content"],
    );
    assert_node_shape(
        Node::Verbatim {
            content: "verbatim".to_string(),
        },
        "verbatim",
        &["content"],
    );
    assert_node_shape(
        Node::Comment {
            content: "comment".to_string(),
        },
        "comment",
        &["content"],
    );
    assert_node_shape(
        Node::Error {
            message: "error".to_string(),
        },
        "error",
        &["message"],
    );
    assert_node_shape(
        Node::Group {
            delim: Delim::Braces,
            body: sample_body.clone(),
        },
        "group",
        &["delim", "body"],
    );
    assert_node_shape(
        Node::Math {
            mode: MathMode::Inline,
            body: sample_body.clone(),
        },
        "math",
        &["mode", "body"],
    );
    assert_node_shape(
        Node::Ident {
            path: vec!["alpha".to_string(), "beta".to_string()],
        },
        "ident",
        &["path"],
    );
    assert_node_shape(
        Node::HashIdent {
            name: "tag".to_string(),
        },
        "hash_ident",
        &["name"],
    );
    assert_node_shape(
        Node::XmlIdent {
            prefix: Some("svg".to_string()),
            name: "rect".to_string(),
        },
        "xml_ident",
        &["prefix", "name"],
    );
    assert_node_shape(
        Node::Let {
            path: vec!["foo".to_string()],
            bindings: sample_binding.clone(),
            body: sample_body.clone(),
        },
        "let",
        &["path", "bindings", "body"],
    );
    assert_node_shape(
        Node::Def {
            path: vec!["foo".to_string()],
            bindings: sample_binding.clone(),
            body: sample_body.clone(),
        },
        "def",
        &["path", "bindings", "body"],
    );
    assert_node_shape(
        Node::Fun {
            bindings: sample_binding.clone(),
            body: sample_body.clone(),
        },
        "fun",
        &["bindings", "body"],
    );
    assert_node_shape(
        Node::Scope {
            body: sample_body.clone(),
        },
        "scope",
        &["body"],
    );
    assert_node_shape(
        Node::Namespace {
            path: vec!["ns".to_string()],
            body: sample_body.clone(),
        },
        "namespace",
        &["path", "body"],
    );
    assert_node_shape(
        Node::Open {
            path: vec!["ns".to_string()],
        },
        "open",
        &["path"],
    );
    assert_node_shape(
        Node::Put {
            path: vec!["slot".to_string()],
            body: sample_body.clone(),
        },
        "put",
        &["path", "body"],
    );
    assert_node_shape(
        Node::Default {
            path: vec!["slot".to_string()],
            body: sample_body.clone(),
        },
        "default",
        &["path", "body"],
    );
    assert_node_shape(
        Node::Get {
            path: vec!["slot".to_string()],
        },
        "get",
        &["path"],
    );
    assert_node_shape(
        Node::Alloc {
            path: vec!["slot".to_string()],
        },
        "alloc",
        &["path"],
    );
    assert_node_shape(
        Node::Object {
            def: ObjectDef {
                self_name: Some("self".to_string()),
                methods: vec![("render".to_string(), sample_body.clone())],
            },
        },
        "object",
        &["def"],
    );
    assert_node_shape(
        Node::Patch {
            def: PatchDef {
                obj: sample_body.clone(),
                self_name: Some("self".to_string()),
                super_name: Some("super".to_string()),
                methods: vec![("render".to_string(), sample_body.clone())],
            },
        },
        "patch",
        &["def"],
    );
    assert_node_shape(
        Node::Call {
            target: sample_body.clone(),
            method: "render".to_string(),
        },
        "call",
        &["target", "method"],
    );
    assert_node_shape(
        Node::Subtree {
            addr: Some("alpha".to_string()),
            body: sample_body.clone(),
        },
        "subtree",
        &["addr", "body"],
    );
    assert_node_shape(
        Node::Import {
            visibility: Visibility::Private,
            target: "alpha".to_string(),
        },
        "import",
        &["visibility", "target"],
    );
    assert_node_shape(
        Node::DeclXmlns {
            prefix: "svg".to_string(),
            uri: "https://www.w3.org/2000/svg".to_string(),
        },
        "decl_xmlns",
        &["prefix", "uri"],
    );
    assert_node_shape(
        Node::DxSequent {
            conclusion: sample_relation.clone(),
            premises: vec![sample_relation.clone()],
        },
        "dx_sequent",
        &["conclusion", "premises"],
    );
    assert_node_shape(
        Node::DxQuery {
            var: "X".to_string(),
            positives: vec![sample_relation.clone()],
            negatives: vec![sample_relation.clone()],
        },
        "dx_query",
        &["var", "positives", "negatives"],
    );
    assert_node_shape(
        Node::DxProp {
            relation: sample_relation.clone(),
            args: vec![sample_body.clone()],
        },
        "dx_prop",
        &["relation", "args"],
    );
    assert_node_shape(
        Node::DxVar {
            name: "X".to_string(),
        },
        "dx_var",
        &["name"],
    );
    assert_node_shape(
        Node::DxConstContent {
            body: sample_body.clone(),
        },
        "dx_const_content",
        &["body"],
    );
    assert_node_shape(
        Node::DxConstUri { body: sample_body },
        "dx_const_uri",
        &["body"],
    );
}
