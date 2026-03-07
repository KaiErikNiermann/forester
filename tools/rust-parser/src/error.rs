// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Error types for the Forester parser

use ariadne::{Color, Label, Report, ReportKind, Source};
use schemars::JsonSchema;
use serde::Serialize;
use std::fmt;
use thiserror::Error;

#[allow(unused_imports)]
use crate::ast::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpectedTokens(Vec<String>);

impl ExpectedTokens {
    pub fn new(values: Vec<String>) -> Self {
        Self(values)
    }

    pub fn values(&self) -> &[String] {
        &self.0
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
    fn new(
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

/// Parse error type
#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: ExpectedTokens,
        found: String,
        span: std::ops::Range<usize>,
    },

    #[error("Unexpected end of input")]
    UnexpectedEof {
        expected: ExpectedTokens,
        span: std::ops::Range<usize>,
    },

    #[error("Unclosed delimiter: {delim}; expected {expected_close} before end of input")]
    UnclosedDelimiter {
        delim: String,
        expected_close: String,
        open_span: std::ops::Range<usize>,
        eof_span: std::ops::Range<usize>,
    },

    #[error("Mismatched delimiter: {open_delim} opened here, expected {expected_close}, found {found_close}")]
    MismatchedDelimiter {
        open_delim: String,
        expected_close: String,
        found_close: String,
        open_span: std::ops::Range<usize>,
        found_span: std::ops::Range<usize>,
    },

    #[error("Unexpected closing delimiter: {found_close}")]
    UnexpectedClosingDelimiter {
        found_close: String,
        span: std::ops::Range<usize>,
    },

    #[error("Invalid escape sequence: \\{char}")]
    InvalidEscape {
        char: char,
        span: std::ops::Range<usize>,
    },

    #[error("Lexer error at position {position}")]
    LexerError {
        position: usize,
        span: std::ops::Range<usize>,
    },

    #[error("{message}")]
    Custom {
        message: String,
        span: std::ops::Range<usize>,
    },
}

impl ParseError {
    pub fn details(&self) -> ParseErrorDetails {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => ParseErrorDetails {
                kind: ParseErrorKind::UnexpectedToken,
                expected: expected.values().to_vec(),
                found: Some(found.clone()),
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    format!("expected {}, found {}", expected, found),
                    span.clone(),
                )],
                notes: vec![],
            },
            ParseError::UnexpectedEof { expected, span } => ParseErrorDetails {
                kind: ParseErrorKind::UnexpectedEof,
                expected: expected.values().to_vec(),
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    format!("expected {} here", expected),
                    span.clone(),
                )],
                notes: vec![],
            },
            ParseError::UnclosedDelimiter {
                delim,
                expected_close,
                open_span,
                eof_span,
            } => ParseErrorDetails {
                kind: ParseErrorKind::UnclosedDelimiter,
                expected: vec![expected_close.clone()],
                found: None,
                labels: vec![
                    ParseErrorLabel::new(
                        ParseErrorLabelKind::Context,
                        format!("{} opened here", delim),
                        open_span.clone(),
                    ),
                    ParseErrorLabel::new(
                        ParseErrorLabelKind::Primary,
                        format!("expected {} before end of input", expected_close),
                        eof_span.clone(),
                    ),
                ],
                notes: vec![format!("close {} with {}", delim, expected_close)],
            },
            ParseError::MismatchedDelimiter {
                open_delim,
                expected_close,
                found_close,
                open_span,
                found_span,
            } => ParseErrorDetails {
                kind: ParseErrorKind::MismatchedDelimiter,
                expected: vec![expected_close.clone()],
                found: Some(found_close.clone()),
                labels: vec![
                    ParseErrorLabel::new(
                        ParseErrorLabelKind::Context,
                        format!("{} opened here", open_delim),
                        open_span.clone(),
                    ),
                    ParseErrorLabel::new(
                        ParseErrorLabelKind::Primary,
                        format!(
                            "found {} here, but {} was required to close {}",
                            found_close, expected_close, open_delim
                        ),
                        found_span.clone(),
                    ),
                ],
                notes: vec![format!(
                    "replace {} with {} or close {} earlier",
                    found_close, expected_close, open_delim
                )],
            },
            ParseError::UnexpectedClosingDelimiter { found_close, span } => ParseErrorDetails {
                kind: ParseErrorKind::UnexpectedClosingDelimiter,
                expected: vec![],
                found: Some(found_close.clone()),
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    format!(
                        "{} does not match any currently open delimiter",
                        found_close
                    ),
                    span.clone(),
                )],
                notes: vec!["remove the closing delimiter or add the matching opener".to_string()],
            },
            ParseError::InvalidEscape { char, span } => ParseErrorDetails {
                kind: ParseErrorKind::InvalidEscape,
                expected: vec![],
                found: Some(format!("\\{}", char)),
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    "not a valid escape",
                    span.clone(),
                )],
                notes: vec![],
            },
            ParseError::LexerError { span, .. } => ParseErrorDetails {
                kind: ParseErrorKind::LexerError,
                expected: vec![],
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    "unexpected character",
                    span.clone(),
                )],
                notes: vec![],
            },
            ParseError::Custom { span, .. } => ParseErrorDetails {
                kind: ParseErrorKind::Custom,
                expected: vec![],
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    "error occurred here",
                    span.clone(),
                )],
                notes: vec![],
            },
        }
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            ParseError::UnexpectedToken { span, .. } => span.clone(),
            ParseError::UnexpectedEof { span, .. } => span.clone(),
            ParseError::UnclosedDelimiter { eof_span, .. } => eof_span.clone(),
            ParseError::MismatchedDelimiter { found_span, .. } => found_span.clone(),
            ParseError::UnexpectedClosingDelimiter { span, .. } => span.clone(),
            ParseError::InvalidEscape { span, .. } => span.clone(),
            ParseError::LexerError { span, .. } => span.clone(),
            ParseError::Custom { span, .. } => span.clone(),
        }
    }

    /// Generate a pretty error report using ariadne
    pub fn report(&self, filename: &str, source: &str) -> String {
        let mut output = Vec::new();
        let source_len = source.len();
        let details = self.details();

        // Helper to clamp span to source bounds
        let clamp_span = |span: &std::ops::Range<usize>| -> std::ops::Range<usize> {
            let start = span.start.min(source_len.saturating_sub(1));
            let end = span.end.min(source_len);
            // Ensure we have at least 1 char span for visibility
            if start >= end && source_len > 0 {
                start..start.saturating_add(1).min(source_len)
            } else {
                start..end
            }
        };

        let headline = match self {
            ParseError::UnexpectedToken { .. } => "Unexpected token".to_string(),
            ParseError::UnexpectedEof { .. } => "Unexpected end of input".to_string(),
            ParseError::UnclosedDelimiter {
                delim,
                expected_close,
                ..
            } => format!(
                "Unclosed delimiter: {} never reached {}",
                delim, expected_close
            ),
            ParseError::MismatchedDelimiter {
                expected_close,
                found_close,
                ..
            } => format!(
                "Mismatched delimiter: expected {}, found {}",
                expected_close, found_close
            ),
            ParseError::UnexpectedClosingDelimiter { found_close, .. } => {
                format!("Unexpected closing delimiter: {}", found_close)
            }
            ParseError::InvalidEscape { char, .. } => {
                format!("Invalid escape sequence: \\{}", char)
            }
            ParseError::LexerError { .. } => "Lexer error".to_string(),
            ParseError::Custom { message, .. } => message.clone(),
        };

        let anchor = details
            .labels
            .first()
            .map(|label| label.start_offset)
            .unwrap_or_else(|| self.span().start);

        let mut builder = Report::build(ReportKind::Error, filename, anchor).with_message(headline);
        for label in &details.labels {
            let color = match label.kind {
                ParseErrorLabelKind::Primary => Color::Red,
                ParseErrorLabelKind::Context => Color::Yellow,
            };
            builder = builder.with_label(
                Label::new((
                    filename,
                    clamp_span(&(label.start_offset..label.end_offset)),
                ))
                .with_message(label.message.clone())
                .with_color(color),
            );
        }
        for note in &details.notes {
            builder = builder.with_note(note.clone());
        }
        let report = builder.finish();

        report
            .write((filename, Source::from(source)), &mut output)
            .unwrap();
        String::from_utf8(output).unwrap_or_else(|_| self.to_string())
    }
}

/// Result type alias for parse operations
pub type ParseResult<T> = Result<T, Vec<ParseError>>;
