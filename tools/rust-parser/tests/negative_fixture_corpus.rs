// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::parse;
use std::fs;
use std::path::{Path, PathBuf};

fn negative_fixture_paths() -> Vec<PathBuf> {
    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/negative");
    let mut paths: Vec<_> = fs::read_dir(&fixture_dir)
        .expect("fixture directory should exist")
        .map(|entry| entry.expect("fixture entry should be readable").path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("tree"))
        .collect();
    paths.sort();
    paths
}

#[test]
fn negative_fixture_corpus_fails() {
    let paths = negative_fixture_paths();
    assert!(!paths.is_empty(), "expected negative parser fixtures");

    for path in paths {
        let input = fs::read_to_string(&path).expect("fixture should be readable");
        let result = parse(&input);
        assert!(
            result.is_err(),
            "fixture {} unexpectedly parsed",
            path.display()
        );
        let errors = result.expect_err("fixture should fail");
        assert!(
            !errors.is_empty(),
            "fixture {} failed without surfaced parser errors",
            path.display()
        );
    }
}
