// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::parse;
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

#[test]
fn positive_fixture_corpus_parses() {
    let paths = positive_fixture_paths();
    assert!(!paths.is_empty(), "expected positive parser fixtures");

    for path in paths {
        let input = fs::read_to_string(&path).expect("fixture should be readable");
        match parse(&input) {
            Ok(document) => {
                assert!(
                    !document.nodes.is_empty(),
                    "fixture {} parsed but produced no nodes",
                    path.display()
                );
            }
            Err(errors) => {
                let rendered = errors
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("; ");
                panic!("fixture {} failed to parse: {}", path.display(), rendered);
            }
        }
    }
}
