// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Lexer for Forester markup language.
//!
//! The Rust parser ultimately needs to mirror the OCaml lexer state machine,
//! where `\\` switches the lexer into an identifier-oriented mode instead of
//! emitting a standalone backslash token. This file implements that mode-aware
//! token stream contract for the parser-facing tokens.

use crate::error::{ParseError, ParseResult};
use std::fmt;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    LParen,
    RParen,
    HashLBrace,
    HashHashLBrace,
    Slash,
    Tick,
    AtSign,
    Hash,
    DxEntailed,
    KwScope,
    KwPut,
    KwDefault,
    KwGet,
    KwImport,
    KwExport,
    KwNamespace,
    KwOpen,
    KwDef,
    KwAlloc,
    KwLet,
    KwFun,
    KwSubtree,
    KwObject,
    KwPatch,
    KwCall,
    KwDatalog,
    XmlIdent(Option<String>, String),
    DeclXmlns(String),
    Ident(String),
    HashIdent(String),
    DxVar(String),
    Text(String),
    Whitespace(String),
    Verbatim(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LSquare => write!(f, "["),
            Token::RSquare => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::HashLBrace => write!(f, "#{{"),
            Token::HashHashLBrace => write!(f, "##{{"),
            Token::Slash => write!(f, "/"),
            Token::Tick => write!(f, "'"),
            Token::AtSign => write!(f, "@"),
            Token::Hash => write!(f, "#"),
            Token::DxEntailed => write!(f, "-:"),
            Token::KwScope => write!(f, "scope"),
            Token::KwPut => write!(f, "put"),
            Token::KwDefault => write!(f, "put?"),
            Token::KwGet => write!(f, "get"),
            Token::KwImport => write!(f, "import"),
            Token::KwExport => write!(f, "export"),
            Token::KwNamespace => write!(f, "namespace"),
            Token::KwOpen => write!(f, "open"),
            Token::KwDef => write!(f, "def"),
            Token::KwAlloc => write!(f, "alloc"),
            Token::KwLet => write!(f, "let"),
            Token::KwFun => write!(f, "fun"),
            Token::KwSubtree => write!(f, "subtree"),
            Token::KwObject => write!(f, "object"),
            Token::KwPatch => write!(f, "patch"),
            Token::KwCall => write!(f, "call"),
            Token::KwDatalog => write!(f, "datalog"),
            Token::XmlIdent(prefix, name) => match prefix {
                Some(prefix) => write!(f, "xml-ident:{prefix}:{name}"),
                None => write!(f, "xml-ident:{name}"),
            },
            Token::DeclXmlns(prefix) => write!(f, "xmlns:{prefix}"),
            Token::Ident(s) => write!(f, "ident:{s}"),
            Token::HashIdent(s) => write!(f, "#{s}"),
            Token::DxVar(s) => write!(f, "?{s}"),
            Token::Text(s) => write!(f, "text:{s}"),
            Token::Whitespace(s) => write!(f, "ws:{s:?}"),
            Token::Verbatim(s) => write!(f, "verbatim:{s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Range<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Mode {
    Main,
    IdentInit { backslash_start: usize },
    IdentFragments,
    Verbatim { herald: String, token_start: usize },
}

struct Scanner<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn rest(&self) -> &'a str {
        &self.input[self.pos..]
    }

    fn starts_with(&self, needle: &str) -> bool {
        self.rest().starts_with(needle)
    }

    fn peek_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    fn advance_bytes(&mut self, len: usize) {
        self.pos += len;
    }

    fn consume_while<F>(&mut self, predicate: F) -> Range<usize>
    where
        F: Fn(char) -> bool,
    {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.pos += ch.len_utf8();
        }
        start..self.pos
    }

    fn take_char_span(&mut self) -> Option<(char, Range<usize>)> {
        let start = self.pos;
        let ch = self.advance_char()?;
        Some((ch, start..self.pos))
    }

    fn slice(&self, range: Range<usize>) -> &'a str {
        &self.input[range]
    }
}

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
        return Err(vec![lexer_error(
            backslash_start,
            backslash_start..scanner.pos,
            "expected command or identifier after backslash",
        )]);
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
    if fragment.start != fragment.end {
        let span_start = fragment.start;
        let ident = scanner.slice(fragment).to_string();
        if scanner.peek_char() == Some('/') {
            push_token(tokens, Token::Ident(ident), span_start..scanner.pos);
            let slash_start = scanner.pos;
            scanner.advance_char();
            push_token(tokens, Token::Slash, slash_start..scanner.pos);
            modes.pop();
            modes.push(Mode::IdentFragments);
        } else {
            modes.pop();
            push_token(tokens, Token::Ident(ident), span_start..scanner.pos);
        }
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

    Err(vec![lexer_error(
        backslash_start,
        scanner.pos..scanner.pos.saturating_add(ch.len_utf8()),
        format!("invalid character after backslash: {ch:?}"),
    )])
}

fn lex_ident_fragments(
    scanner: &mut Scanner<'_>,
    modes: &mut Vec<Mode>,
    tokens: &mut Vec<SpannedToken>,
) -> ParseResult<()> {
    let start = scanner.pos;
    let fragment = scanner.consume_while(is_simple_name_char);
    if fragment.start == fragment.end {
        let span = start..scanner.pos.saturating_add(1).min(scanner.input.len());
        return Err(vec![lexer_error(
            start,
            span,
            "expected identifier fragment after slash",
        )]);
    }

    let ident = scanner.slice(fragment.clone()).to_string();
    push_token(tokens, Token::Ident(ident), fragment.start..fragment.end);

    if scanner.peek_char() == Some('/') {
        let slash_start = scanner.pos;
        scanner.advance_char();
        push_token(tokens, Token::Slash, slash_start..scanner.pos);
    } else {
        modes.pop();
    }

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
            Mode::IdentInit { backslash_start } => Err(vec![lexer_error(
                *backslash_start,
                *backslash_start..scanner.pos,
                "expected command or identifier after backslash",
            )]),
            Mode::IdentFragments => Err(vec![lexer_error(
                scanner.pos,
                scanner.pos..scanner.pos,
                "expected identifier fragment after slash",
            )]),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = tokenize("\\title{Hello}").unwrap();
        assert!(tokens.len() >= 3);
        assert_eq!(tokens[0].token, Token::Ident("title".to_string()));
    }

    #[test]
    fn test_keywords_are_contextual() {
        let plain = tokenize("scope import def").unwrap();
        assert!(plain
            .iter()
            .all(|token| matches!(token.token, Token::Text(_) | Token::Whitespace(_))));

        let command = tokenize("\\scope{\\def\\name{body}}").unwrap();
        assert!(command.iter().any(|token| token.token == Token::KwScope));
        assert!(command.iter().any(|token| token.token == Token::KwDef));
    }

    #[test]
    fn test_ident_fragments_are_only_after_backslash() {
        let tokens = tokenize("\\alpha/beta gamma/delta").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::Ident("alpha".to_string()),
                Token::Slash,
                Token::Ident("beta".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Text("gamma/delta".to_string()),
            ]
        );
    }

    #[test]
    fn test_comment() {
        let tokens = tokenize("% this is a comment").unwrap();
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_comment_consumes_newline_and_indent() {
        let tokens = tokenize("alpha% comment\n  beta").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::Text("alpha".to_string()),
                Token::Text("beta".to_string()),
            ]
        );
    }

    #[test]
    fn test_comment_leaves_following_blank_line() {
        let tokens = tokenize("% comment\n\nbeta").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::Whitespace("\n".to_string()),
                Token::Text("beta".to_string()),
            ]
        );
    }

    #[test]
    fn test_whitespace_tokens_preserve_newlines() {
        let tokens = tokenize("alpha  \n\tbeta").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::Text("alpha".to_string()),
                Token::Whitespace("  ".to_string()),
                Token::Whitespace("\n".to_string()),
                Token::Whitespace("\t".to_string()),
                Token::Text("beta".to_string()),
            ]
        );
    }

    #[test]
    fn test_special_names_after_backslash() {
        let tokens = tokenize("\\% \\{ \\_ \\| \\ ").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::Text("%".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Ident("{".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Ident("_".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Ident("|".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::Ident(" ".to_string()),
            ]
        );
    }

    #[test]
    fn test_inline_verbatim() {
        let tokens = tokenize("\\verbfoo|  hello world  foo").unwrap();
        assert_eq!(
            tokens,
            vec![SpannedToken {
                token: Token::Verbatim("  hello world".to_string()),
                span: 0..27,
            }]
        );
    }

    #[test]
    fn test_block_verbatim() {
        let tokens = tokenize("\\startverb\nhello\n\\stopverb").unwrap();
        assert_eq!(
            tokens,
            vec![SpannedToken {
                token: Token::Verbatim("hello".to_string()),
                span: 0..26,
            }]
        );
    }

    #[test]
    fn test_unterminated_verbatim_errors() {
        let errors = tokenize("\\startverb\nhello").unwrap_err();
        assert_eq!(errors[0].to_string(), "unterminated verbatim");
    }

    #[test]
    fn test_datalog_token_forms() {
        let tokens = tokenize("?- ?foo -: # #topic @ '").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::DxVar("-".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::DxVar("foo".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::DxEntailed,
                Token::Whitespace(" ".to_string()),
                Token::Hash,
                Token::Whitespace(" ".to_string()),
                Token::HashIdent("topic".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::AtSign,
                Token::Whitespace(" ".to_string()),
                Token::Tick,
            ]
        );
    }

    #[test]
    fn test_bare_question_is_dx_var_empty() {
        let tokens = tokenize("?").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![Token::DxVar(String::new())]
        );
    }

    #[test]
    fn test_xml_ident_tokens() {
        let tokens = tokenize("\\<svg:path> \\<article>").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![
                Token::XmlIdent(Some("svg".to_string()), "path".to_string()),
                Token::Whitespace(" ".to_string()),
                Token::XmlIdent(None, "article".to_string()),
            ]
        );
    }

    #[test]
    fn test_xmlns_token() {
        let tokens = tokenize("\\xmlns:svg").unwrap();
        assert_eq!(
            tokens
                .iter()
                .map(|token| token.token.clone())
                .collect::<Vec<_>>(),
            vec![Token::DeclXmlns("svg".to_string())]
        );
    }
}
