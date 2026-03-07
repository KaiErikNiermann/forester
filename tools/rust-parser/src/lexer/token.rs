// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

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
            Token::Ident(value) => write!(f, "ident:{value}"),
            Token::HashIdent(value) => write!(f, "#{value}"),
            Token::DxVar(value) => write!(f, "?{value}"),
            Token::Text(value) => write!(f, "text:{value}"),
            Token::Whitespace(value) => write!(f, "ws:{value:?}"),
            Token::Verbatim(value) => write!(f, "verbatim:{value}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Range<usize>,
}
