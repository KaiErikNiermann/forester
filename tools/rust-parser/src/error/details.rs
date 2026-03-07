// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use schemars::JsonSchema;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedTokens(Vec<String>);

impl ExpectedTokens {
    pub fn new(values: Vec<String>) -> Self {
        Self(values)
    }

    pub fn values(&self) -> &[String] {
        &self.0
    }

    pub fn contains(&self, value: &str) -> bool {
        self.0.iter().any(|candidate| candidate == value)
    }
}

impl fmt::Display for ExpectedTokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.as_slice() {
            [] => write!(f, "end of input"),
            [only] => write!(f, "{only}"),
            [rest @ .., last] => {
                let joined = rest.join(", ");
                write!(f, "{joined} or {last}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ParseErrorKind {
    UnexpectedToken,
    UnexpectedEof,
    UnclosedDelimiter,
    MismatchedDelimiter,
    UnexpectedClosingDelimiter,
    InvalidEscape,
    LexerError,
    Custom,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ParseErrorLabelKind {
    Primary,
    Context,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, JsonSchema)]
pub struct ParseErrorLabel {
    pub kind: ParseErrorLabelKind,
    pub message: String,
    pub start_offset: usize,
    pub end_offset: usize,
}

impl ParseErrorLabel {
    pub(super) fn new(
        kind: ParseErrorLabelKind,
        message: impl Into<String>,
        span: std::ops::Range<usize>,
    ) -> Self {
        Self {
            kind,
            message: message.into(),
            start_offset: span.start,
            end_offset: span.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, JsonSchema)]
pub struct ParseErrorDetails {
    pub kind: ParseErrorKind,
    pub expected: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub found: Option<String>,
    pub labels: Vec<ParseErrorLabel>,
    pub notes: Vec<String>,
}

pub(super) const VALID_ESCAPED_NAMES_HINT: &str =
    "valid escaped names after a backslash here are %, space, \\\\, ,, \", `, _, ;, #, {, }, [, ], and |";

fn has_targeted_expectations(expected: &ExpectedTokens) -> bool {
    expected.values().len() <= 3
}

pub(super) fn expected_note(expected: &ExpectedTokens) -> Option<String> {
    (!expected.values().is_empty()).then(|| format!("expected {}", expected))
}

pub(super) fn delimiter_expectation_note(expected: &ExpectedTokens) -> Option<String> {
    (has_targeted_expectations(expected)
        && (expected.contains("'}'") || expected.contains("']'") || expected.contains("')'")))
    .then(|| "a preceding delimiter may still need its closing token".to_string())
}

pub(super) fn braced_body_note(expected: &ExpectedTokens) -> Option<String> {
    (has_targeted_expectations(expected) && expected.contains("'{'"))
        .then(|| "this construct requires a braced argument or body at this point".to_string())
}

pub(super) fn path_continuation_note(expected: &ExpectedTokens) -> Option<String> {
    (has_targeted_expectations(expected) && expected.contains("'/'"))
        .then(|| "slash-separated identifier paths may continue with '/'".to_string())
}

pub(super) fn plain_text_note(found: &str) -> Option<String> {
    found.starts_with("'text:").then(|| {
        "plain text is only valid inside textual bodies; at top level you likely need a command such as \\p{...}".to_string()
    })
}

pub(super) fn custom_label(message: &str) -> String {
    if message.starts_with("syntax error, unexpected ") {
        "unexpected lexeme here".to_string()
    } else if message == "unterminated verbatim" {
        "verbatim block did not reach its closing marker".to_string()
    } else if message.contains("XML identifier") {
        "invalid XML command syntax here".to_string()
    } else if message.contains("xmlns prefix") {
        "invalid xmlns command syntax here".to_string()
    } else {
        "error occurred here".to_string()
    }
}

pub(super) fn custom_notes(message: &str) -> Vec<String> {
    match message {
        "unterminated verbatim" => vec![
            "inline verbatim closes when the opening herald reappears".to_string(),
            "block verbatim closes only at \\stopverb".to_string(),
        ],
        msg if msg.starts_with("syntax error, unexpected ") => vec![
            "the lexer could not continue from this lexeme".to_string(),
            VALID_ESCAPED_NAMES_HINT.to_string(),
        ],
        "invalid XML identifier after backslash"
        | "invalid qualified XML identifier after backslash" => {
            vec!["expected an XML command of the form \\<name> or \\<prefix:name>".to_string()]
        }
        "unterminated XML identifier after backslash" => {
            vec!["close the XML command with '>'".to_string()]
        }
        "invalid xmlns prefix after backslash" => {
            vec!["expected an xmlns declaration of the form \\xmlns:prefix".to_string()]
        }
        _ => vec![],
    }
}
