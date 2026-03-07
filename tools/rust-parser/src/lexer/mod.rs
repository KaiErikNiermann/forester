// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Lexer for Forester markup language.
//!
//! The Rust parser ultimately needs to mirror the OCaml lexer state machine,
//! where `\\` switches the lexer into an identifier-oriented mode instead of
//! emitting a standalone backslash token. This file implements that mode-aware
//! token stream contract for the parser-facing tokens.

mod scanner;
mod token;

#[cfg(test)]
mod tests;

use crate::error::{ParseError, ParseResult};
use scanner::{Mode, Scanner};
use std::ops::Range;

pub use token::{SpannedToken, Token};
fn is_simple_name_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '-'
}

fn is_xml_base_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_xml_base_ident_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '-' | '_')
}

fn is_special_name_char(ch: char) -> bool {
    matches!(
        ch,
        '\\' | ',' | '"' | '`' | '_' | ';' | '#' | '{' | '}' | '[' | ']' | ' ' | '|'
    )
}

fn is_main_text_boundary(ch: char) -> bool {
    matches!(
        ch,
        ' ' | '\t'
            | '\r'
            | '\n'
            | '%'
            | '#'
            | '\\'
            | '{'
            | '}'
            | '['
            | ']'
            | '('
            | ')'
            | '\''
            | '@'
            | '?'
    )
}

fn trim_verbatim(content: &str) -> String {
    let stripped_newlines = content.trim_matches(|c| c == '\r' || c == '\n');
    stripped_newlines
        .trim_end_matches([' ', '\t', '\r', '\n'])
        .to_string()
}

fn consume_newline(scanner: &mut Scanner<'_>) -> Range<usize> {
    let start = scanner.pos;
    if scanner.starts_with("\r\n") {
        scanner.advance_bytes(2);
    } else {
        let _ = scanner.advance_char();
    }
    start..scanner.pos
}

fn push_token(tokens: &mut Vec<SpannedToken>, token: Token, span: Range<usize>) {
    tokens.push(SpannedToken { token, span });
}

fn lexer_error(_position: usize, span: Range<usize>, message: impl Into<String>) -> ParseError {
    ParseError::Custom {
        message: message.into(),
        span: if span.start == span.end {
            span.start..span.end.saturating_add(1)
        } else {
            span
        },
    }
}

fn current_lexeme_span(scanner: &Scanner<'_>) -> Range<usize> {
    let start = scanner.pos;
    if scanner.starts_with("\r\n") {
        start..start + 2
    } else if let Some(ch) = scanner.peek_char() {
        start..start + ch.len_utf8()
    } else {
        start..start
    }
}

fn unexpected_lexeme_error(scanner: &Scanner<'_>, span: Range<usize>) -> ParseError {
    let lexeme = scanner.slice(span.clone());
    ParseError::Custom {
        message: format!("syntax error, unexpected {lexeme:?}"),
        span: if span.start == span.end {
            span.start..span.end.saturating_add(1)
        } else {
            span
        },
    }
}

fn lex_main(scanner: &mut Scanner<'_>, modes: &mut Vec<Mode>, tokens: &mut Vec<SpannedToken>) {
    let Some(ch) = scanner.peek_char() else {
        return;
    };

    match ch {
        '\\' => {
            let start = scanner.pos;
            scanner.advance_char();
            modes.push(Mode::IdentInit {
                backslash_start: start,
            });
        }
        '%' => {
            scanner.advance_char();
            scanner.consume_while(|c| c != '\n' && c != '\r');
            if !scanner.is_eof() {
                let _ = consume_newline(scanner);
                scanner.consume_while(|c| matches!(c, ' ' | '\t'));
            }
        }
        '#' if scanner.starts_with("##{") => {
            let start = scanner.pos;
            scanner.advance_bytes(3);
            push_token(tokens, Token::HashHashLBrace, start..scanner.pos);
        }
        '#' if scanner.starts_with("#{") => {
            let start = scanner.pos;
            scanner.advance_bytes(2);
            push_token(tokens, Token::HashLBrace, start..scanner.pos);
        }
        '#' => {
            let start = scanner.pos;
            scanner.advance_char();
            let name = scanner.consume_while(is_simple_name_char);
            if name.start != name.end {
                push_token(
                    tokens,
                    Token::HashIdent(scanner.slice(name).to_string()),
                    start..scanner.pos,
                );
            } else {
                push_token(tokens, Token::Hash, start..scanner.pos);
            }
        }
        '?' => {
            let start = scanner.pos;
            scanner.advance_char();
            let name = scanner.consume_while(is_simple_name_char);
            push_token(
                tokens,
                Token::DxVar(scanner.slice(name).to_string()),
                start..scanner.pos,
            );
        }
        '\'' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::Tick, start..scanner.pos);
        }
        '@' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::AtSign, start..scanner.pos);
        }
        '{' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::LBrace, start..scanner.pos);
        }
        '}' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::RBrace, start..scanner.pos);
        }
        '[' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::LSquare, start..scanner.pos);
        }
        ']' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::RSquare, start..scanner.pos);
        }
        '(' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::LParen, start..scanner.pos);
        }
        ')' => {
            let start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::RParen, start..scanner.pos);
        }
        '-' if scanner.starts_with("-:") => {
            let start = scanner.pos;
            scanner.advance_bytes(2);
            push_token(tokens, Token::DxEntailed, start..scanner.pos);
        }
        ' ' | '\t' => {
            let span = scanner.consume_while(|c| matches!(c, ' ' | '\t'));
            push_token(
                tokens,
                Token::Whitespace(scanner.slice(span.clone()).to_string()),
                span,
            );
        }
        '\r' | '\n' => {
            let span = consume_newline(scanner);
            push_token(
                tokens,
                Token::Whitespace(scanner.slice(span.clone()).to_string()),
                span,
            );
        }
        _ => {
            let start = scanner.pos;
            let text = scanner.consume_while(|current| !is_main_text_boundary(current));
            if text.start == text.end {
                let _ = scanner.advance_char();
            }
            push_token(
                tokens,
                Token::Text(scanner.slice(start..scanner.pos).to_string()),
                start..scanner.pos,
            );
        }
    }
}

fn try_keyword(
    scanner: &mut Scanner<'_>,
    keyword: &str,
    token: Token,
) -> Option<(Token, Range<usize>)> {
    if scanner.starts_with(keyword) {
        let start = scanner.pos;
        scanner.advance_bytes(keyword.len());
        Some((token, start..scanner.pos))
    } else {
        None
    }
}

fn consume_xml_base_ident(scanner: &mut Scanner<'_>) -> Option<Range<usize>> {
    let start = scanner.pos;
    let ch = scanner.peek_char()?;
    if !is_xml_base_ident_start(ch) {
        return None;
    }

    let _ = scanner.advance_char();
    let _ = scanner.consume_while(is_xml_base_ident_char);
    Some(start..scanner.pos)
}

fn lex_ident_init(
    scanner: &mut Scanner<'_>,
    modes: &mut Vec<Mode>,
    tokens: &mut Vec<SpannedToken>,
    backslash_start: usize,
) -> ParseResult<()> {
    let Some(ch) = scanner.peek_char() else {
        modes.pop();
        push_token(
            tokens,
            Token::Ident(String::new()),
            scanner.pos..scanner.pos,
        );
        return Ok(());
    };

    if ch == '<' {
        let start = scanner.pos;
        let _ = scanner.advance_char();

        let Some(first) = consume_xml_base_ident(scanner) else {
            return Err(vec![lexer_error(
                backslash_start,
                start..scanner.pos,
                "invalid XML identifier after backslash",
            )]);
        };

        let first_ident = scanner.slice(first).to_string();
        let (prefix, name) = if scanner.peek_char() == Some(':') {
            let _ = scanner.advance_char();
            let Some(second) = consume_xml_base_ident(scanner) else {
                return Err(vec![lexer_error(
                    backslash_start,
                    start..scanner.pos,
                    "invalid qualified XML identifier after backslash",
                )]);
            };
            (Some(first_ident), scanner.slice(second).to_string())
        } else {
            (None, first_ident)
        };

        if scanner.peek_char() != Some('>') {
            return Err(vec![lexer_error(
                backslash_start,
                start..scanner.pos,
                "unterminated XML identifier after backslash",
            )]);
        }

        let _ = scanner.advance_char();
        modes.pop();
        push_token(tokens, Token::XmlIdent(prefix, name), start..scanner.pos);
        return Ok(());
    }

    if scanner.starts_with("xmlns:") {
        let start = scanner.pos;
        scanner.advance_bytes("xmlns:".len());
        let Some(prefix) = consume_xml_base_ident(scanner) else {
            return Err(vec![lexer_error(
                backslash_start,
                start..scanner.pos,
                "invalid xmlns prefix after backslash",
            )]);
        };

        modes.pop();
        push_token(
            tokens,
            Token::DeclXmlns(scanner.slice(prefix).to_string()),
            start..scanner.pos,
        );
        return Ok(());
    }

    if scanner.starts_with("startverb") {
        scanner.advance_bytes("startverb".len());
        modes.pop();
        modes.push(Mode::Verbatim {
            herald: "\\stopverb".to_string(),
            token_start: backslash_start,
        });
        return Ok(());
    }

    if scanner.starts_with("verb") {
        let saved = scanner.pos;
        scanner.advance_bytes("verb".len());
        let herald = scanner.consume_while(|c| !matches!(c, ' ' | '\t' | '\r' | '\n' | '|'));
        if herald.start != herald.end && scanner.peek_char() == Some('|') {
            let herald = scanner.slice(herald).to_string();
            scanner.advance_char();
            modes.pop();
            modes.push(Mode::Verbatim {
                herald,
                token_start: backslash_start,
            });
            return Ok(());
        }
        scanner.pos = saved;
    }

    let keywords = [
        ("namespace", Token::KwNamespace),
        ("startverb", Token::Text("startverb".to_string())),
        ("subtree", Token::KwSubtree),
        ("import", Token::KwImport),
        ("export", Token::KwExport),
        ("object", Token::KwObject),
        ("patch", Token::KwPatch),
        ("scope", Token::KwScope),
        ("put?", Token::KwDefault),
        ("alloc", Token::KwAlloc),
        ("open", Token::KwOpen),
        ("call", Token::KwCall),
        ("def", Token::KwDef),
        ("let", Token::KwLet),
        ("fun", Token::KwFun),
        ("put", Token::KwPut),
        ("get", Token::KwGet),
        ("datalog", Token::KwDatalog),
    ];

    for (keyword, token) in keywords {
        if let Some((token, span)) = try_keyword(scanner, keyword, token) {
            modes.pop();
            push_token(tokens, token, span);
            return Ok(());
        }
    }

    if ch == '%' {
        let start = scanner.pos;
        scanner.advance_char();
        modes.pop();
        push_token(tokens, Token::Text("%".to_string()), start..scanner.pos);
        return Ok(());
    }

    let fragment = scanner.consume_while(is_simple_name_char);
    if scanner.peek_char() == Some('/') {
        let ident = scanner.slice(fragment.clone()).to_string();
        push_token(tokens, Token::Ident(ident), fragment.start..fragment.end);
        let slash_start = scanner.pos;
        scanner.advance_char();
        push_token(tokens, Token::Slash, slash_start..scanner.pos);
        modes.pop();
        modes.push(Mode::IdentFragments);
        return Ok(());
    }

    if fragment.start != fragment.end {
        modes.pop();
        push_token(
            tokens,
            Token::Ident(scanner.slice(fragment.clone()).to_string()),
            fragment.start..fragment.end,
        );
        return Ok(());
    }

    if is_special_name_char(ch) {
        let Some((special, span)) = scanner.take_char_span() else {
            unreachable!();
        };
        modes.pop();
        push_token(tokens, Token::Ident(special.to_string()), span);
        return Ok(());
    }

    Err(vec![unexpected_lexeme_error(
        scanner,
        current_lexeme_span(scanner),
    )])
}

fn lex_ident_fragments(
    scanner: &mut Scanner<'_>,
    modes: &mut Vec<Mode>,
    tokens: &mut Vec<SpannedToken>,
) -> ParseResult<()> {
    let fragment = scanner.consume_while(is_simple_name_char);
    if scanner.peek_char() == Some('/') {
        push_token(
            tokens,
            Token::Ident(scanner.slice(fragment.clone()).to_string()),
            fragment.start..fragment.end,
        );
        let slash_start = scanner.pos;
        scanner.advance_char();
        push_token(tokens, Token::Slash, slash_start..scanner.pos);
        return Ok(());
    }

    if fragment.start != fragment.end {
        push_token(
            tokens,
            Token::Ident(scanner.slice(fragment.clone()).to_string()),
            fragment.start..fragment.end,
        );
        modes.pop();
        return Ok(());
    }

    if matches!(scanner.peek_char(), Some('\r' | '\n')) {
        return Err(vec![unexpected_lexeme_error(
            scanner,
            current_lexeme_span(scanner),
        )]);
    }

    modes.pop();
    push_token(
        tokens,
        Token::Ident(String::new()),
        scanner.pos..scanner.pos,
    );
    Ok(())
}

fn lex_verbatim(
    scanner: &mut Scanner<'_>,
    modes: &mut Vec<Mode>,
    tokens: &mut Vec<SpannedToken>,
    herald: &str,
    token_start: usize,
) -> ParseResult<()> {
    if let Some(offset) = scanner.rest().find(herald) {
        let body_start = scanner.pos;
        let body_end = scanner.pos + offset;
        let token_end = body_end + herald.len();
        let text = trim_verbatim(&scanner.input[body_start..body_end]);
        scanner.pos = token_end;
        modes.pop();
        push_token(tokens, Token::Verbatim(text), token_start..token_end);
        Ok(())
    } else {
        Err(vec![lexer_error(
            token_start,
            token_start..scanner.input.len(),
            "unterminated verbatim",
        )])
    }
}

pub fn tokenize(input: &str) -> ParseResult<Vec<SpannedToken>> {
    let mut scanner = Scanner::new(input);
    let mut modes = vec![Mode::Main];
    let mut tokens = Vec::new();

    while !scanner.is_eof() {
        match modes.last().cloned().expect("mode stack is never empty") {
            Mode::Main => lex_main(&mut scanner, &mut modes, &mut tokens),
            Mode::IdentInit { backslash_start } => {
                lex_ident_init(&mut scanner, &mut modes, &mut tokens, backslash_start)?;
            }
            Mode::IdentFragments => {
                lex_ident_fragments(&mut scanner, &mut modes, &mut tokens)?;
            }
            Mode::Verbatim {
                herald,
                token_start,
            } => {
                lex_verbatim(&mut scanner, &mut modes, &mut tokens, &herald, token_start)?;
            }
        }
    }

    if let Some(mode) = modes.last() {
        match mode {
            Mode::Main => Ok(tokens),
            Mode::IdentInit { .. } => {
                push_token(
                    &mut tokens,
                    Token::Ident(String::new()),
                    scanner.pos..scanner.pos,
                );
                Ok(tokens)
            }
            Mode::IdentFragments => {
                push_token(
                    &mut tokens,
                    Token::Ident(String::new()),
                    scanner.pos..scanner.pos,
                );
                Ok(tokens)
            }
            Mode::Verbatim { token_start, .. } => Err(vec![lexer_error(
                *token_start,
                *token_start..scanner.pos,
                "unterminated verbatim",
            )]),
        }
    } else {
        Ok(tokens)
    }
}
