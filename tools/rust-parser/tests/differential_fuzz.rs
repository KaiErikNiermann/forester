// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::{parse, Document};
use proptest::prelude::*;
use proptest::proptest;
use proptest::string::string_regex;
use proptest::test_runner::{Config as ProptestConfig, FileFailurePersistence};
use serde::Deserialize;
use serde_json::Value;
use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;

const DIFFERENTIAL_CASES: u32 = 96;
const OCAML_ORACLE_ENV: &str = "FORESTER_OCAML_PARSER_ORACLE_PATH";
const REQUIRE_OCAML_ORACLE_ENV: &str = "FORESTER_REQUIRE_OCAML_ORACLE";

#[derive(Debug, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
enum OcamlOracleResult {
    Parsed { document: Value },
    Failed { diagnostic: String },
}

#[derive(Debug)]
enum DifferentialOutcome {
    Parsed(Value),
    Failed(String),
}

impl DifferentialOutcome {
    fn failure_detail(&self) -> Option<&str> {
        match self {
            DifferentialOutcome::Parsed(_) => None,
            DifferentialOutcome::Failed(message) => Some(message),
        }
    }
}

static OCAML_ORACLE_PATH: OnceLock<Result<Option<PathBuf>, String>> = OnceLock::new();

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("tools/rust-parser should live two levels below the repo root")
        .to_path_buf()
}

fn bool_env(name: &str) -> bool {
    matches!(
        env::var(name).ok().as_deref(),
        Some("1" | "true" | "yes" | "on")
    )
}

fn resolve_ocaml_oracle_path() -> Result<Option<PathBuf>, String> {
    if let Ok(path) = env::var(OCAML_ORACLE_ENV) {
        let candidate = PathBuf::from(path);
        if candidate.is_file() {
            return Ok(Some(candidate));
        }
        return Err(format!(
            "{OCAML_ORACLE_ENV} points to a missing parser oracle binary: {}",
            candidate.display()
        ));
    }

    let repo = repo_root();
    let built_path = repo.join("_build/default/lib/parser/test/Parser_oracle.exe");
    if built_path.is_file() {
        return Ok(Some(built_path));
    }

    let opam_exists = Command::new("sh")
        .args(["-c", "command -v opam >/dev/null 2>&1"])
        .current_dir(&repo)
        .status()
        .map(|status| status.success())
        .unwrap_or(false);
    if !opam_exists {
        return Ok(None);
    }

    let status = Command::new("opam")
        .args([
            "exec",
            "--",
            "dune",
            "build",
            "lib/parser/test/Parser_oracle.exe",
        ])
        .current_dir(&repo)
        .status()
        .map_err(|error| format!("failed to build OCaml parser oracle: {error}"))?;
    if !status.success() {
        return Err("failed to build OCaml parser oracle with dune".to_string());
    }

    if built_path.is_file() {
        Ok(Some(built_path))
    } else {
        Err(format!(
            "dune reported success but parser oracle binary was not found at {}",
            built_path.display()
        ))
    }
}

fn ocaml_oracle_path() -> Option<&'static Path> {
    match OCAML_ORACLE_PATH.get_or_init(resolve_ocaml_oracle_path) {
        Ok(Some(path)) => Some(path.as_path()),
        Ok(None) => {
            if bool_env(REQUIRE_OCAML_ORACLE_ENV) {
                panic!(
                    "OCaml parser oracle is unavailable; set {OCAML_ORACLE_ENV} or install opam+dune"
                );
            }
            eprintln!("differential_fuzz skipped because the OCaml parser oracle is unavailable");
            None
        }
        Err(message) => panic!("{message}"),
    }
}

fn normalize_document(document: &Document) -> Value {
    let mut value = serde_json::to_value(document).expect("document should serialize");
    strip_json_noise(&mut value);
    collapse_text_runs(&mut value);
    value
}

fn strip_json_noise(value: &mut Value) {
    match value {
        Value::Array(items) => {
            for item in items {
                strip_json_noise(item);
            }
        }
        Value::Object(map) => {
            map.remove("span");
            map.remove("source_path");
            for item in map.values_mut() {
                strip_json_noise(item);
            }
        }
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
    }
}

fn collapse_text_runs(value: &mut Value) {
    match value {
        Value::Array(items) => {
            for item in items.iter_mut() {
                collapse_text_runs(item);
            }
            collapse_located_text_items(items);
        }
        Value::Object(map) => {
            for item in map.values_mut() {
                collapse_text_runs(item);
            }
        }
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => {}
    }
}

fn collapse_located_text_items(items: &mut Vec<Value>) {
    let mut collapsed = Vec::with_capacity(items.len());

    for item in items.drain(..) {
        let next_text = item
            .get("value")
            .and_then(|value| value.get("type"))
            .and_then(Value::as_str)
            .zip(
                item.get("value")
                    .and_then(|value| value.get("content"))
                    .and_then(Value::as_str),
            );

        match (collapsed.last_mut(), next_text) {
            (Some(Value::Object(previous)), Some(("text", next_text))) => {
                let previous_text = previous
                    .get_mut("value")
                    .and_then(Value::as_object_mut)
                    .filter(|value| value.get("type").and_then(Value::as_str) == Some("text"))
                    .and_then(|value| value.get_mut("content"));

                if let Some(Value::String(previous_text)) = previous_text {
                    previous_text.push_str(next_text);
                } else {
                    collapsed.push(item);
                }
            }
            _ => collapsed.push(item),
        }
    }

    *items = collapsed;
}

fn rust_outcome(input: &str) -> DifferentialOutcome {
    match parse(input) {
        Ok(document) => DifferentialOutcome::Parsed(normalize_document(&document)),
        Err(errors) => DifferentialOutcome::Failed(
            errors
                .into_iter()
                .map(|error| error.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
        ),
    }
}

fn ocaml_outcome(oracle_path: &Path, input: &str) -> Result<DifferentialOutcome, String> {
    let mut child = Command::new(oracle_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|error| format!("failed to spawn OCaml parser oracle: {error}"))?;

    child
        .stdin
        .take()
        .expect("stdin should be piped")
        .write_all(input.as_bytes())
        .map_err(|error| format!("failed to write parser oracle input: {error}"))?;

    let output = child
        .wait_with_output()
        .map_err(|error| format!("failed to read parser oracle output: {error}"))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "OCaml parser oracle exited with {}: {}",
            output.status,
            stderr.trim()
        ));
    }

    let response: OcamlOracleResult = serde_json::from_slice(&output.stdout)
        .map_err(|error| format!("failed to decode OCaml parser oracle JSON: {error}"))?;
    Ok(match response {
        OcamlOracleResult::Parsed { mut document } => {
            collapse_text_runs(&mut document);
            DifferentialOutcome::Parsed(document)
        }
        OcamlOracleResult::Failed { diagnostic } => DifferentialOutcome::Failed(diagnostic),
    })
}

fn ident_fragment() -> BoxedStrategy<String> {
    string_regex("[a-z]{1,4}")
        .expect("ident regex should compile")
        .boxed()
}

fn text_fragment() -> BoxedStrategy<String> {
    string_regex("[A-Za-z0-9]{1,6}")
        .expect("text regex should compile")
        .boxed()
}

fn path_fragment() -> BoxedStrategy<String> {
    prop::collection::vec(ident_fragment(), 1..4)
        .prop_map(|parts| parts.join("/"))
        .boxed()
}

fn ws_fragment() -> BoxedStrategy<String> {
    prop_oneof![
        Just(" ".to_string()),
        Just("\n".to_string()),
        Just("\n  ".to_string()),
        Just("\t".to_string()),
    ]
    .boxed()
}

fn leaf_fragment() -> BoxedStrategy<String> {
    let text = text_fragment();
    prop_oneof![
        4 => text.clone(),
        2 => ws_fragment(),
        1 => Just("%comment\n".to_string()),
        1 => Just("\\%".to_string()),
        1 => Just("\\_".to_string()),
        1 => Just("\\startverb\nbody\\stopverb".to_string()),
        1 => ident_fragment().prop_map(|name| format!("\\{name}")),
        1 => path_fragment().prop_map(|path| format!("\\open\\{path}")),
        1 => path_fragment().prop_map(|path| format!("\\get\\{path}")),
        1 => text.clone().prop_map(|name| format!("#{name}")),
        1 => text.prop_map(|name| format!("'{name}")),
    ]
    .boxed()
}

fn fragment() -> BoxedStrategy<String> {
    leaf_fragment().prop_recursive(4, 128, 8, |inner| {
        let body = prop::collection::vec(inner.clone(), 0..4).prop_map(|parts| parts.concat());
        prop_oneof![
            3 => body.clone().prop_map(|body| format!("{{{body}}}")),
            2 => body.clone().prop_map(|body| format!("[{body}]")),
            2 => body.clone().prop_map(|body| format!("({body})")),
            2 => body.clone().prop_map(|body| format!("#{{{body}}}")),
            1 => body.clone().prop_map(|body| format!("##{{{body}}}")),
            2 => (ident_fragment(), body.clone()).prop_map(|(name, body)| format!("\\{name}{{{body}}}")),
            1 => (path_fragment(), body.clone()).prop_map(|(path, body)| format!("\\namespace\\{path}{{{body}}}")),
            1 => (path_fragment(), body.clone()).prop_map(|(path, body)| format!("\\put\\{path}{{{body}}}")),
            1 => body.clone().prop_map(|body| format!("\\scope{{{body}}}")),
            1 => body.clone().prop_map(|body| format!("\\subtree{{{body}}}")),
            3 => prop::collection::vec(inner.clone(), 0..4).prop_map(|parts| parts.concat()),
        ]
    })
    .boxed()
}

fn generated_input() -> BoxedStrategy<String> {
    prop_oneof![
        6 => prop::collection::vec(fragment(), 0..6).prop_map(|parts| parts.concat()),
        1 => prop::collection::vec(fragment(), 1..6).prop_map(|parts| {
            let mut joined = parts.concat();
            let _ = joined.pop();
            joined
        }),
        1 => prop::collection::vec(fragment(), 0..4).prop_map(|parts| format!("{}]", parts.concat())),
        1 => prop::collection::vec(fragment(), 0..4).prop_map(|parts| format!("{}\\", parts.concat())),
    ]
    .boxed()
}

fn differential_config() -> ProptestConfig {
    let mut config = ProptestConfig::with_failure_persistence(FileFailurePersistence::Direct(
        "proptest-regressions/differential_fuzz.txt",
    ))
    .clone_with_source_file(file!());
    config.cases = DIFFERENTIAL_CASES;
    config
}

proptest! {
    #![proptest_config(differential_config())]

    #[test]
    fn differential_fuzz_matches_ocaml(input in generated_input()) {
        let Some(oracle_path) = ocaml_oracle_path() else {
            return Ok(());
        };

        let rust = rust_outcome(&input);
        let ocaml = ocaml_outcome(oracle_path, &input).map_err(TestCaseError::fail)?;

        match (&rust, &ocaml) {
            (DifferentialOutcome::Parsed(rust_document), DifferentialOutcome::Parsed(ocaml_document)) => {
                prop_assert_eq!(
                    rust_document,
                    ocaml_document,
                    "Rust/OCaml parser AST mismatch for generated input {:?}",
                    input,
                );
            }
            (DifferentialOutcome::Failed(_), DifferentialOutcome::Failed(_)) => {}
            _ => {
                prop_assert!(
                    false,
                    "Rust/OCaml parser outcome mismatch for generated input {:?}\nRust: {:#?}\nRust failure: {:?}\nOCaml: {:#?}\nOCaml failure: {:?}",
                    input,
                    rust,
                    rust.failure_detail(),
                    ocaml,
                    ocaml.failure_detail(),
                );
            }
        }
    }
}
