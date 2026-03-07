// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use ariadne::{Color, Label, Report, ReportKind, Source};
use thiserror::Error;

use super::details::{
    braced_body_note, custom_label, custom_notes, delimiter_expectation_note, expected_note,
    path_continuation_note, plain_text_note, VALID_ESCAPED_NAMES_HINT,
};
use super::{
    ExpectedTokens, ParseErrorDetails, ParseErrorKind, ParseErrorLabel, ParseErrorLabelKind,
};

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
                    format!("found {} here", found),
                    span.clone(),
                )],
                notes: expected_note(expected)
                    .into_iter()
                    .chain(plain_text_note(found))
                    .chain(braced_body_note(expected))
                    .chain(path_continuation_note(expected))
                    .collect(),
            },
            ParseError::UnexpectedEof { expected, span } => ParseErrorDetails {
                kind: ParseErrorKind::UnexpectedEof,
                expected: expected.values().to_vec(),
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    "input ended here",
                    span.clone(),
                )],
                notes: expected_note(expected)
                    .into_iter()
                    .chain(braced_body_note(expected))
                    .chain(path_continuation_note(expected))
                    .chain(delimiter_expectation_note(expected))
                    .chain(expected.values().is_empty().then(|| {
                        "input ended before the current construct was complete".to_string()
                    }))
                    .collect(),
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
                    "invalid escape sequence here",
                    span.clone(),
                )],
                notes: vec![VALID_ESCAPED_NAMES_HINT.to_string()],
            },
            ParseError::LexerError { span, .. } => ParseErrorDetails {
                kind: ParseErrorKind::LexerError,
                expected: vec![],
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    "lexer stopped on this character",
                    span.clone(),
                )],
                notes: vec![
                    "inspect surrounding escapes, verbatim heralds, or XML command syntax"
                        .to_string(),
                ],
            },
            ParseError::Custom { message, span } => ParseErrorDetails {
                kind: ParseErrorKind::Custom,
                expected: vec![],
                found: None,
                labels: vec![ParseErrorLabel::new(
                    ParseErrorLabelKind::Primary,
                    custom_label(message),
                    span.clone(),
                )],
                notes: custom_notes(message),
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

        let clamp_span = |span: &std::ops::Range<usize>| -> std::ops::Range<usize> {
            let start = span.start.min(source_len.saturating_sub(1));
            let end = span.end.min(source_len);
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
