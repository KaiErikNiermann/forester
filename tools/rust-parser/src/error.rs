// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Error types for the Forester parser

use ariadne::{Color, Label, Report, ReportKind, Source};
use thiserror::Error;

#[allow(unused_imports)]
use crate::ast::Span;

/// Parse error type
#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: std::ops::Range<usize>,
    },

    #[error("Unexpected end of input")]
    UnexpectedEof {
        expected: String,
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

        let report = match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                let clamped = clamp_span(span);
                Report::build(ReportKind::Error, filename, clamped.start)
                    .with_message("Unexpected token".to_string())
                    .with_label(
                        Label::new((filename, clamped))
                            .with_message(format!("expected {}, found {}", expected, found))
                            .with_color(Color::Red),
                    )
                    .finish()
            }

            ParseError::UnexpectedEof { expected, span } => {
                // For EOF, point to the last character of the file
                let clamped = clamp_span(span);
                Report::build(ReportKind::Error, filename, clamped.start)
                    .with_message("Unexpected end of input")
                    .with_label(
                        Label::new((filename, clamped))
                            .with_message(format!("expected {} here", expected))
                            .with_color(Color::Red),
                    )
                    .finish()
            }

            ParseError::UnclosedDelimiter {
                delim,
                expected_close,
                open_span,
                eof_span,
            } => {
                let open_clamped = clamp_span(open_span);
                let eof_clamped = clamp_span(eof_span);
                Report::build(ReportKind::Error, filename, open_span.start)
                    .with_message(format!(
                        "Unclosed delimiter: {} never reached {}",
                        delim, expected_close
                    ))
                    .with_label(
                        Label::new((filename, open_clamped))
                            .with_message(format!("{} opened here", delim))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((filename, eof_clamped))
                            .with_message(format!(
                                "expected {} before end of input",
                                expected_close
                            ))
                            .with_color(Color::Red),
                    )
                    .with_note(format!("close {} with {}", delim, expected_close))
                    .finish()
            }

            ParseError::MismatchedDelimiter {
                open_delim,
                expected_close,
                found_close,
                open_span,
                found_span,
            } => {
                let open_clamped = clamp_span(open_span);
                let found_clamped = clamp_span(found_span);
                Report::build(ReportKind::Error, filename, found_span.start)
                    .with_message(format!(
                        "Mismatched delimiter: expected {}, found {}",
                        expected_close, found_close
                    ))
                    .with_label(
                        Label::new((filename, open_clamped))
                            .with_message(format!("{} opened here", open_delim))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new((filename, found_clamped))
                            .with_message(format!(
                                "found {} here, but {} was required to close {}",
                                found_close, expected_close, open_delim
                            ))
                            .with_color(Color::Red),
                    )
                    .with_note(format!(
                        "replace {} with {} or close {} earlier",
                        found_close, expected_close, open_delim
                    ))
                    .finish()
            }

            ParseError::UnexpectedClosingDelimiter { found_close, span } => {
                let clamped = clamp_span(span);
                Report::build(ReportKind::Error, filename, clamped.start)
                    .with_message(format!("Unexpected closing delimiter: {}", found_close))
                    .with_label(
                        Label::new((filename, clamped))
                            .with_message(format!(
                                "{} does not match any currently open delimiter",
                                found_close
                            ))
                            .with_color(Color::Red),
                    )
                    .with_note("remove the closing delimiter or add the matching opener")
                    .finish()
            }

            ParseError::InvalidEscape { char, span } => {
                Report::build(ReportKind::Error, filename, span.start)
                    .with_message(format!("Invalid escape sequence: \\{}", char))
                    .with_label(
                        Label::new((filename, span.clone()))
                            .with_message("not a valid escape")
                            .with_color(Color::Red),
                    )
                    .finish()
            }

            ParseError::LexerError { position, span } => {
                Report::build(ReportKind::Error, filename, *position)
                    .with_message("Lexer error")
                    .with_label(
                        Label::new((filename, span.clone()))
                            .with_message("unexpected character")
                            .with_color(Color::Red),
                    )
                    .finish()
            }

            ParseError::Custom { message, span } => {
                Report::build(ReportKind::Error, filename, span.start)
                    .with_message(message)
                    .with_label(
                        Label::new((filename, span.clone()))
                            .with_message("error occurred here")
                            .with_color(Color::Red),
                    )
                    .finish()
            }
        };

        report
            .write((filename, Source::from(source)), &mut output)
            .unwrap();
        String::from_utf8(output).unwrap_or_else(|_| self.to_string())
    }
}

/// Result type alias for parse operations
pub type ParseResult<T> = Result<T, Vec<ParseError>>;
