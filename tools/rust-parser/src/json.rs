// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared JSON contract for the Rust parser CLI, FFI, and OCaml bridge.

use schemars::{schema_for, JsonSchema};
use serde::Serialize;

use crate::error::ParseError;
use crate::Document;

/// Result of parsing, returned as JSON to external consumers.
#[derive(Debug, Clone, PartialEq, Serialize, JsonSchema)]
#[serde(tag = "status")]
pub enum ParseResult {
    #[serde(rename = "ok")]
    Ok { document: Document },
    #[serde(rename = "error")]
    Error { errors: Vec<ErrorInfo> },
}

#[derive(Debug, Clone, PartialEq, Serialize, JsonSchema)]
pub struct ErrorInfo {
    pub message: String,
    pub start_offset: usize,
    pub end_offset: usize,
    /// Pretty-printed error report using ariadne.
    pub report: String,
}

impl ErrorInfo {
    pub fn from_error(error: &ParseError, filename: &str, source: &str) -> Self {
        let span = error.span();
        Self {
            message: error.to_string(),
            start_offset: span.start,
            end_offset: span.end,
            report: error.report(filename, source),
        }
    }
}

impl ParseResult {
    pub fn from_parse_result(
        result: Result<Document, Vec<ParseError>>,
        filename: &str,
        source: &str,
    ) -> Self {
        match result {
            Ok(document) => Self::Ok { document },
            Err(errors) => Self::Error {
                errors: errors
                    .iter()
                    .map(|error| ErrorInfo::from_error(error, filename, source))
                    .collect(),
            },
        }
    }
}

pub fn schema_json() -> serde_json::Value {
    serde_json::to_value(schema_for!(ParseResult)).expect("serialize parse-result schema")
}

pub fn schema_pretty_string() -> String {
    let mut json =
        serde_json::to_string_pretty(&schema_json()).expect("render parse-result schema");
    json.push('\n');
    json
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_result_schema_matches_checked_in_contract() {
        let actual = schema_json();
        let expected = serde_json::from_str::<serde_json::Value>(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/json-schema/parse-result.schema.json"
        )))
        .expect("parse checked-in schema");

        assert_eq!(actual, expected);
    }

    #[test]
    fn parse_result_serializes_status_envelope() {
        let result = ParseResult::Ok {
            document: Document::new(vec![]),
        };

        let json = serde_json::to_value(result).expect("serialize parse result");
        assert_eq!(json["status"], "ok");
        assert!(json.get("document").is_some());
    }
}
