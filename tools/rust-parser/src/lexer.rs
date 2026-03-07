// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Lexer for Forester markup language using Logos

use logos::{Lexer, Logos, Skip};
use std::fmt;

/// Helper to track newlines for position tracking
fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.line_start = lex.span().end;
    Skip
}

/// Extra state for the lexer (line tracking)
#[derive(Debug, Clone, Default)]
pub struct LexerExtras {
    pub line: usize,
    pub line_start: usize,
}

impl LexerExtras {
    pub fn new() -> Self {
        Self {
            line: 1,
            line_start: 0,
        }
    }
}

/// Token type for Forester lexer
#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(extras = LexerExtras)]
#[logos(skip r"[ \t]+")] // Skip horizontal whitespace
pub enum Token {
    // === Newlines (tracked for position) ===
    #[regex(r"\n|\r\n|\r", newline_callback)]
    Newline,

    // === Delimiters ===
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LSquare,

    #[token("]")]
    RSquare,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    // === Math mode ===
    #[token("#{")]
    HashLBrace,

    #[token("##{")]
    HashHashLBrace,

    // === Special tokens ===
    #[token("/")]
    Slash,

    #[token("'")]
    Tick,

    #[token("@")]
    AtSign,

    #[token("#")]
    Hash,

    #[token("-:")]
    DxEntailed,

    // === Backslash commands ===
    #[token("\\")]
    Backslash,

    // === Escaped literal text ===
    #[regex(r"\\[%#{}\[\]\\]", |lex| lex.slice()[1..].to_string())]
    EscapedText(String),

    // === Keywords (recognized after backslash in parser) ===
    #[token("scope")]
    KwScope,

    #[token("put")]
    KwPut,

    #[token("put?")]
    KwDefault,

    #[token("get")]
    KwGet,

    #[token("import")]
    KwImport,

    #[token("export")]
    KwExport,

    #[token("namespace")]
    KwNamespace,

    #[token("open")]
    KwOpen,

    #[token("def")]
    KwDef,

    #[token("alloc")]
    KwAlloc,

    #[token("let")]
    KwLet,

    #[token("fun")]
    KwFun,

    #[token("subtree")]
    KwSubtree,

    #[token("object")]
    KwObject,

    #[token("patch")]
    KwPatch,

    #[token("call")]
    KwCall,

    #[token("datalog")]
    KwDatalog,

    #[token("verb")]
    KwVerb,

    #[token("startverb")]
    KwStartverb,

    #[token("stopverb")]
    KwStopverb,

    // === Identifiers ===
    #[regex(r"[a-zA-Z][a-zA-Z0-9\-]*", priority = 3, callback = |lex| lex.slice().to_string())]
    Ident(String),

    // === Hash identifiers (#name) ===
    #[regex(r"#[a-zA-Z][a-zA-Z0-9\-_]*", |lex| lex.slice()[1..].to_string())]
    HashIdent(String),

    // === Datalog variables (?name) ===
    #[regex(r"\?[a-zA-Z][a-zA-Z0-9\-_]*", |lex| lex.slice()[1..].to_string())]
    DxVar(String),

    // === XML element identifiers (\<name> or \<prefix:name>) ===
    // Will be handled in parser after backslash
    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token(":")]
    Colon,

    // === Comments ===
    #[regex(r"%[^\r\n]*", |lex| lex.slice()[1..].to_string())]
    Comment(String),

    // === Text content - everything else ===
    #[regex(r"[^%#\\{}\[\]()\r\n \t'@/<>:\-~]+", priority = 1, callback = |lex| lex.slice().to_string())]
    Text(String),

    // === Tilde for lazy bindings ===
    #[token("~")]
    Tilde,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Newline => write!(f, "newline"),
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
            Token::Backslash => write!(f, "\\"),
            Token::EscapedText(s) => write!(f, "escaped:{}", s),
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
            Token::KwVerb => write!(f, "verb"),
            Token::KwStartverb => write!(f, "startverb"),
            Token::KwStopverb => write!(f, "stopverb"),
            Token::Ident(s) => write!(f, "ident:{}", s),
            Token::HashIdent(s) => write!(f, "#{}", s),
            Token::DxVar(s) => write!(f, "?{}", s),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Colon => write!(f, ":"),
            Token::Comment(s) => write!(f, "%{}", s),
            Token::Text(s) => write!(f, "text:{}", s),
            Token::Tilde => write!(f, "~"),
        }
    }
}

/// Spanned token with position information
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: std::ops::Range<usize>,
}

/// Tokenize input and collect all tokens with spans
pub fn tokenize(input: &str) -> Vec<SpannedToken> {
    let mut lexer = Token::lexer_with_extras(input, LexerExtras::new());
    let mut tokens = Vec::new();

    while let Some(result) = lexer.next() {
        match result {
            Ok(token) => {
                // Skip newlines in token stream (they're tracked in extras)
                if token != Token::Newline {
                    tokens.push(SpannedToken {
                        token,
                        span: lexer.span(),
                    });
                }
            }
            Err(_) => {
                // Unknown token - emit as text
                tokens.push(SpannedToken {
                    token: Token::Text(lexer.slice().to_string()),
                    span: lexer.span(),
                });
            }
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = tokenize("\\title{Hello}");
        assert!(tokens.len() >= 4);
        assert_eq!(tokens[0].token, Token::Backslash);
    }

    #[test]
    fn test_math_mode() {
        let tokens = tokenize("#{x^2}");
        assert_eq!(tokens[0].token, Token::HashLBrace);
    }

    #[test]
    fn test_comment() {
        let tokens = tokenize("% this is a comment");
        assert!(matches!(tokens[0].token, Token::Comment(_)));
    }

    #[test]
    fn test_escaped_punctuation() {
        let tokens = tokenize(r"\% \{ \} \[ \] \# \\");
        assert!(matches!(tokens[0].token, Token::EscapedText(ref s) if s == "%"));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "{")));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "}")));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "[")));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "]")));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "#")));
        assert!(tokens
            .iter()
            .any(|token| matches!(token.token, Token::EscapedText(ref s) if s == "\\")));
    }
}
