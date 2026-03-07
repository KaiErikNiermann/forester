// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use crate::ast::{Position, Span};
use crate::error::{ExpectedTokens, ParseError};
use crate::lexer::Token;
use chumsky::error::{Simple, SimpleReason};
use std::cell::RefCell;
use std::ops::Range;

thread_local! {
    // Chumsky only gives the parser closures byte ranges, so stash the current
    // source text while a parse is active to build accurate line/column spans.
    static CURRENT_PARSE_SOURCE: RefCell<Option<String>> = const { RefCell::new(None) };
}

pub(super) struct ParseSourceGuard;

impl Drop for ParseSourceGuard {
    fn drop(&mut self) {
        CURRENT_PARSE_SOURCE.with(|source| {
            source.borrow_mut().take();
        });
    }
}

pub(super) fn set_current_parse_source(input: &str) -> ParseSourceGuard {
    CURRENT_PARSE_SOURCE.with(|source| {
        *source.borrow_mut() = Some(input.to_string());
    });
    ParseSourceGuard
}

fn make_span(input: &str, range: Range<usize>) -> Span {
    let start = compute_position(input, range.start);
    let end = compute_position(input, range.end);
    Span::new(start, end)
}

fn compute_position(input: &str, offset: usize) -> Position {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in input.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    Position::new(offset, line, col)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DelimiterKind {
    Braces,
    Squares,
    Parens,
    InlineMath,
    DisplayMath,
}

impl DelimiterKind {
    fn from_open_token(token: &Token) -> Option<Self> {
        match token {
            Token::LBrace => Some(Self::Braces),
            Token::LSquare => Some(Self::Squares),
            Token::LParen => Some(Self::Parens),
            Token::HashLBrace => Some(Self::InlineMath),
            Token::HashHashLBrace => Some(Self::DisplayMath),
            _ => None,
        }
    }

    fn from_reason_delimiter(delimiter: &str) -> Option<Self> {
        match delimiter {
            "{" => Some(Self::Braces),
            "[" => Some(Self::Squares),
            "(" => Some(Self::Parens),
            "#{" => Some(Self::InlineMath),
            "##{" => Some(Self::DisplayMath),
            _ => None,
        }
    }

    fn open_display(self) -> &'static str {
        match self {
            Self::Braces => "{",
            Self::Squares => "[",
            Self::Parens => "(",
            Self::InlineMath => "#{",
            Self::DisplayMath => "##{",
        }
    }

    fn close_display(self) -> &'static str {
        match self {
            Self::Braces | Self::InlineMath | Self::DisplayMath => "}",
            Self::Squares => "]",
            Self::Parens => ")",
        }
    }

    fn matches_close(self, token: &Token) -> bool {
        matches!(
            (self, token),
            (Self::Braces, Token::RBrace)
                | (Self::Squares, Token::RSquare)
                | (Self::Parens, Token::RParen)
                | (Self::InlineMath, Token::RBrace)
                | (Self::DisplayMath, Token::RBrace)
        )
    }
}

#[derive(Debug, Clone)]
struct DelimiterFrame {
    kind: DelimiterKind,
    open_span: Range<usize>,
}

fn quote_delimiter(delimiter: &str) -> String {
    format!("'{}'", delimiter)
}

fn closing_delimiter_display(token: &Token) -> Option<&'static str> {
    match token {
        Token::RBrace => Some("}"),
        Token::RSquare => Some("]"),
        Token::RParen => Some(")"),
        _ => None,
    }
}

fn delimiter_error_for(
    error: &Simple<Token>,
    tokens: &[(Token, Range<usize>)],
) -> Option<ParseError> {
    let target_index = match error.found() {
        Some(found) => tokens.iter().position(|(token, span)| {
            token == found && span.start == error.span().start && span.end == error.span().end
        }),
        None => None,
    };

    let mut stack: Vec<DelimiterFrame> = Vec::new();

    for (index, (token, span)) in tokens.iter().enumerate() {
        if let Some(kind) = DelimiterKind::from_open_token(token) {
            stack.push(DelimiterFrame {
                kind,
                open_span: span.clone(),
            });
        } else if let Some(found_close) = closing_delimiter_display(token) {
            match stack.last() {
                Some(frame) if frame.kind.matches_close(token) => {
                    stack.pop();
                }
                Some(frame) if target_index.is_none_or(|target| index <= target) => {
                    return Some(ParseError::MismatchedDelimiter {
                        open_delim: quote_delimiter(frame.kind.open_display()),
                        expected_close: quote_delimiter(frame.kind.close_display()),
                        found_close: quote_delimiter(found_close),
                        open_span: frame.open_span.clone(),
                        found_span: span.clone(),
                    });
                }
                None if target_index.is_none_or(|target| index <= target) => {
                    return Some(ParseError::UnexpectedClosingDelimiter {
                        found_close: quote_delimiter(found_close),
                        span: span.clone(),
                    });
                }
                _ => {}
            }
        }

        if target_index.is_some_and(|target| index >= target) {
            break;
        }
    }

    if error.found().is_none() {
        if let Some(frame) = stack.last() {
            return Some(ParseError::UnclosedDelimiter {
                delim: quote_delimiter(frame.kind.open_display()),
                expected_close: quote_delimiter(frame.kind.close_display()),
                open_span: frame.open_span.clone(),
                eof_span: error.span(),
            });
        }
    }

    match error.reason() {
        SimpleReason::Unclosed {
            span: open_span,
            delimiter,
        } => DelimiterKind::from_reason_delimiter(&delimiter.to_string()).map(|kind| {
            ParseError::UnclosedDelimiter {
                delim: quote_delimiter(kind.open_display()),
                expected_close: quote_delimiter(kind.close_display()),
                open_span: open_span.clone(),
                eof_span: error.span(),
            }
        }),
        _ => None,
    }
}

pub(super) fn convert_chumsky_error(
    error: Simple<Token>,
    tokens: &[(Token, Range<usize>)],
) -> ParseError {
    if let Some(delimiter_error) = delimiter_error_for(&error, tokens) {
        return delimiter_error;
    }

    let span = error.span();
    let expected = error
        .expected()
        .filter_map(|token| token.as_ref().map(|token| format!("'{}'", token)))
        .collect();
    let expected_tokens = ExpectedTokens::new(expected);
    let found = match error.found() {
        Some(token) => format!("'{}'", token),
        None => "end of input".to_string(),
    };

    match error.reason() {
        SimpleReason::Unexpected => {
            if error.found().is_none() {
                ParseError::UnexpectedEof {
                    expected: expected_tokens,
                    span,
                }
            } else {
                ParseError::UnexpectedToken {
                    expected: expected_tokens,
                    found,
                    span,
                }
            }
        }
        SimpleReason::Unclosed { span, delimiter } => ParseError::UnclosedDelimiter {
            delim: quote_delimiter(&delimiter.to_string()),
            expected_close: "'end of input'".to_string(),
            open_span: span.clone(),
            eof_span: error.span(),
        },
        SimpleReason::Custom(message) => ParseError::Custom {
            message: message.clone(),
            span,
        },
    }
}

pub(super) fn make_span_from_range(range: Range<usize>) -> Span {
    CURRENT_PARSE_SOURCE.with(|source| {
        let source = source.borrow();
        match source.as_deref() {
            Some(input) => make_span(input, range),
            None => Span::new(
                Position::new(range.start, 1, range.start + 1),
                Position::new(range.end, 1, range.end + 1),
            ),
        }
    })
}
