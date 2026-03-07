// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::json::ParseResult;
use forester_rust_parser::parse;
use std::fs;
use std::path::{Path, PathBuf};

fn golden_fixture_paths() -> Vec<PathBuf> {
    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/error-golden");
    let mut paths: Vec<_> = fs::read_dir(&fixture_dir)
        .expect("golden fixture directory should exist")
        .map(|entry| {
            entry
                .expect("golden fixture entry should be readable")
                .path()
        })
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("tree"))
        .collect();
    paths.sort();
    paths
}

fn strip_ansi(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && chars.peek() == Some(&'[') {
            let _ = chars.next();
            for next in chars.by_ref() {
                if ('@'..='~').contains(&next) {
                    break;
                }
            }
        } else {
            out.push(ch);
        }
    }

    out.replace("\r\n", "\n")
}

fn normalized_error_snapshot(input: &str, filename: &str) -> serde_json::Value {
    let result = ParseResult::from_parse_result(parse(input), filename, input);
    let mut value = serde_json::to_value(result).expect("serialize parse result");

    let errors = value
        .get_mut("errors")
        .and_then(serde_json::Value::as_array_mut)
        .expect("golden fixture should produce parser errors");

    for error in errors {
        let report = error
            .get("report")
            .and_then(serde_json::Value::as_str)
            .expect("error report should be a string");
        error["report"] = serde_json::Value::String(strip_ansi(report));
    }

    value
}

#[test]
fn error_golden_outputs_remain_stable() {
    let fixtures = golden_fixture_paths();
    assert!(!fixtures.is_empty(), "expected golden error fixtures");

    for fixture in fixtures {
        let input = fs::read_to_string(&fixture).expect("golden fixture input should be readable");
        let filename = fixture
            .file_name()
            .and_then(|name| name.to_str())
            .expect("fixture filename should be valid UTF-8");
        let expected_path = fixture.with_extension("json");
        let expected = fs::read_to_string(&expected_path)
            .unwrap_or_else(|_| panic!("missing expected snapshot {}", expected_path.display()));
        let expected_json: serde_json::Value =
            serde_json::from_str(&expected).expect("golden snapshot should be valid JSON");

        let actual_json = normalized_error_snapshot(&input, filename);

        assert_eq!(
            actual_json,
            expected_json,
            "error snapshot mismatch for {}",
            fixture.display()
        );
    }
}
