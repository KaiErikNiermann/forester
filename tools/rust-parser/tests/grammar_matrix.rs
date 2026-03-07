// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::{parse, Node, Nodes, Visibility};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

fn positive_fixture_paths() -> Vec<PathBuf> {
    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/positive");
    let mut paths: Vec<_> = fs::read_dir(&fixture_dir)
        .expect("fixture directory should exist")
        .map(|entry| entry.expect("fixture entry should be readable").path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("tree"))
        .collect();
    paths.sort();
    paths
}

fn insert_markers(nodes: &Nodes, markers: &mut BTreeSet<String>) {
    for node in nodes {
        match &node.value {
            Node::Text { .. } => {
                markers.insert("text".to_string());
            }
            Node::Verbatim { .. } => {
                markers.insert("verbatim".to_string());
            }
            Node::Comment { .. } => {
                markers.insert("comment".to_string());
            }
            Node::Error { .. } => {
                markers.insert("error".to_string());
            }
            Node::Group { delim, body } => {
                let name = match delim {
                    forester_rust_parser::Delim::Braces => "group:braces",
                    forester_rust_parser::Delim::Squares => "group:squares",
                    forester_rust_parser::Delim::Parens => "group:parens",
                };
                markers.insert(name.to_string());
                insert_markers(body, markers);
            }
            Node::Math { mode, body } => {
                let name = match mode {
                    forester_rust_parser::MathMode::Inline => "math:inline",
                    forester_rust_parser::MathMode::Display => "math:display",
                };
                markers.insert(name.to_string());
                insert_markers(body, markers);
            }
            Node::Ident { .. } => {
                markers.insert("ident".to_string());
            }
            Node::HashIdent { .. } => {
                markers.insert("hash_ident".to_string());
            }
            Node::XmlIdent { .. } => {
                markers.insert("xml_ident".to_string());
            }
            Node::Let { body, .. } => {
                markers.insert("let".to_string());
                insert_markers(body, markers);
            }
            Node::Def { body, .. } => {
                markers.insert("def".to_string());
                insert_markers(body, markers);
            }
            Node::Fun { body, .. } => {
                markers.insert("fun".to_string());
                insert_markers(body, markers);
            }
            Node::Scope { body } => {
                markers.insert("scope".to_string());
                insert_markers(body, markers);
            }
            Node::Namespace { body, .. } => {
                markers.insert("namespace".to_string());
                insert_markers(body, markers);
            }
            Node::Open { .. } => {
                markers.insert("open".to_string());
            }
            Node::Put { body, .. } => {
                markers.insert("put".to_string());
                insert_markers(body, markers);
            }
            Node::Default { body, .. } => {
                markers.insert("default".to_string());
                insert_markers(body, markers);
            }
            Node::Get { .. } => {
                markers.insert("get".to_string());
            }
            Node::Alloc { .. } => {
                markers.insert("alloc".to_string());
            }
            Node::Object { def } => {
                markers.insert("object".to_string());
                for (_, body) in &def.methods {
                    insert_markers(body, markers);
                }
            }
            Node::Patch { def } => {
                markers.insert("patch".to_string());
                insert_markers(&def.obj, markers);
                for (_, body) in &def.methods {
                    insert_markers(body, markers);
                }
            }
            Node::Call { target, .. } => {
                markers.insert("call".to_string());
                insert_markers(target, markers);
            }
            Node::Subtree { body, .. } => {
                markers.insert("subtree".to_string());
                insert_markers(body, markers);
            }
            Node::Import { visibility, .. } => {
                let name = match visibility {
                    Visibility::Private => "import:private",
                    Visibility::Public => "import:public",
                };
                markers.insert(name.to_string());
            }
            Node::DeclXmlns { .. } => {
                markers.insert("decl_xmlns".to_string());
            }
            Node::DxSequent {
                conclusion,
                premises,
            } => {
                markers.insert("dx_sequent".to_string());
                insert_markers(conclusion, markers);
                for premise in premises {
                    insert_markers(premise, markers);
                }
            }
            Node::DxQuery {
                positives,
                negatives,
                ..
            } => {
                markers.insert("dx_query".to_string());
                for positive in positives {
                    insert_markers(positive, markers);
                }
                for negative in negatives {
                    insert_markers(negative, markers);
                }
            }
            Node::DxProp { relation, args } => {
                markers.insert("dx_prop".to_string());
                insert_markers(relation, markers);
                for arg in args {
                    insert_markers(arg, markers);
                }
            }
            Node::DxVar { .. } => {
                markers.insert("dx_var".to_string());
            }
            Node::DxConstContent { body } => {
                markers.insert("dx_const_content".to_string());
                insert_markers(body, markers);
            }
            Node::DxConstUri { body } => {
                markers.insert("dx_const_uri".to_string());
                insert_markers(body, markers);
            }
        }
    }
}

#[test]
fn positive_fixture_corpus_covers_every_implemented_grammar_family() {
    let mut markers = BTreeSet::new();

    for path in positive_fixture_paths() {
        let input = fs::read_to_string(&path).expect("fixture should be readable");
        let document = parse(&input).unwrap_or_else(|errors| {
            let rendered = errors
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join("; ");
            panic!("fixture {} failed to parse: {}", path.display(), rendered);
        });
        insert_markers(&document.nodes, &mut markers);
    }

    let required = [
        "text",
        "verbatim",
        "group:braces",
        "group:squares",
        "group:parens",
        "math:inline",
        "math:display",
        "ident",
        "hash_ident",
        "xml_ident",
        "def",
        "let",
        "fun",
        "scope",
        "namespace",
        "open",
        "put",
        "default",
        "get",
        "alloc",
        "object",
        "patch",
        "call",
        "subtree",
        "import:private",
        "import:public",
        "decl_xmlns",
        "dx_sequent",
        "dx_query",
        "dx_prop",
        "dx_var",
        "dx_const_content",
        "dx_const_uri",
    ];

    let missing: Vec<_> = required
        .into_iter()
        .filter(|marker| !markers.contains(*marker))
        .collect();

    assert!(
        missing.is_empty(),
        "positive fixtures are missing grammar markers: {missing:?}\nobserved markers: {markers:?}"
    );
}
