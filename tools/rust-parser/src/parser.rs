// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Parser for Forester markup language using Chumsky

use crate::ast::*;
use crate::error::{ExpectedTokens, ParseError, ParseResult};
use crate::lexer::{tokenize, Token};
use chumsky::prelude::*;
use std::cell::RefCell;
use std::ops::Range;

thread_local! {
    // Chumsky only gives the parser closures byte ranges, so stash the current
    // source text while a parse is active to build accurate line/column spans.
    static CURRENT_PARSE_SOURCE: RefCell<Option<String>> = const { RefCell::new(None) };
}

struct ParseSourceGuard;

impl Drop for ParseSourceGuard {
    fn drop(&mut self) {
        CURRENT_PARSE_SOURCE.with(|source| {
            source.borrow_mut().take();
        });
    }
}

/// Convert lexer span to AST span
fn make_span(input: &str, range: std::ops::Range<usize>) -> Span {
    let start = compute_position(input, range.start);
    let end = compute_position(input, range.end);
    Span::new(start, end)
}

/// Compute line/column from offset
fn compute_position(input: &str, offset: usize) -> Position {
    let mut line = 1;
    let mut col = 1;
    let mut _line_start = 0;

    for (i, ch) in input.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            _line_start = i + 1;
        } else {
            col += 1;
        }
    }

    Position::new(offset, line, col)
}

/// Token stream type for chumsky
#[allow(dead_code)]
type TokenStream<'a> = &'a [(Token, std::ops::Range<usize>)];

/// Parser input type
#[allow(dead_code)]
type ParserInput<'a> = chumsky::stream::Stream<
    'a,
    Token,
    std::ops::Range<usize>,
    std::iter::Map<
        std::slice::Iter<'a, (Token, std::ops::Range<usize>)>,
        fn(&'a (Token, std::ops::Range<usize>)) -> (Token, std::ops::Range<usize>),
    >,
>;

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
    open_span: std::ops::Range<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseMode {
    Strict,
    Recovery,
}

impl ParseMode {
    fn enables_recovery(self) -> bool {
        matches!(self, Self::Recovery)
    }
}

#[derive(Debug, Clone)]
pub struct RecoveryResult<T> {
    pub output: Option<T>,
    pub errors: Vec<ParseError>,
}

fn recovered_error_node(message: impl Into<String>, span: Range<usize>) -> Located<Node> {
    Located::new(
        Node::Error {
            message: message.into(),
        },
        Some(make_span_from_range(span)),
    )
}

fn recovered_error_nodes(message: impl Into<String>, span: Range<usize>) -> Nodes {
    vec![recovered_error_node(message, span)]
}

fn recover_nested_delimiters<P, O, F, const N: usize>(
    parser: P,
    mode: ParseMode,
    start: Token,
    end: Token,
    others: [(Token, Token); N],
    fallback: F,
) -> BoxedParser<'static, Token, O, Simple<Token>>
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone + 'static,
    O: 'static,
    F: Fn(Range<usize>) -> O + Clone + 'static,
{
    if mode.enables_recovery() {
        parser
            .recover_with(nested_delimiters(start, end, others, fallback))
            .boxed()
    } else {
        parser.boxed()
    }
}

fn recover_until_closing<P, O, F>(
    parser: P,
    mode: ParseMode,
    closing: Token,
    fallback: F,
) -> BoxedParser<'static, Token, O, Simple<Token>>
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone + 'static,
    O: 'static,
    F: Fn(Range<usize>) -> O + Clone + 'static,
{
    if !mode.enables_recovery() {
        return parser.boxed();
    }

    let boundary = filter({
        let closing = closing.clone();
        move |token: &Token| token == &closing
    })
    .rewind()
    .ignored()
    .or_not();

    parser
        .recover_with(skip_parser(
            filter({
                let closing = closing.clone();
                move |token: &Token| token != &closing
            })
            .repeated()
            .at_least(1)
            .map_with_span(move |_: Vec<Token>, span| fallback(span))
            .then_ignore(boundary),
        ))
        .boxed()
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
    tokens: &[(Token, std::ops::Range<usize>)],
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
        chumsky::error::SimpleReason::Unclosed {
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

fn parse_with_mode(input: &str, mode: ParseMode) -> RecoveryResult<Document> {
    CURRENT_PARSE_SOURCE.with(|source| {
        *source.borrow_mut() = Some(input.to_string());
    });
    let _guard = ParseSourceGuard;

    // Tokenize
    let spanned_tokens = match tokenize(input) {
        Ok(tokens) => tokens,
        Err(errors) => {
            return RecoveryResult {
                output: None,
                errors,
            };
        }
    };

    // Convert to format chumsky expects
    let tokens: Vec<(Token, std::ops::Range<usize>)> = spanned_tokens
        .into_iter()
        .map(|st| (st.token, st.span))
        .collect();

    // Create stream
    let len = input.len();
    let stream = chumsky::Stream::from_iter(len..len + 1, tokens.clone().into_iter());

    let parser = document_parser(mode);
    let (nodes, errors) = match mode {
        ParseMode::Strict => match parser.parse(stream) {
            Ok(nodes) => (Some(nodes), Vec::new()),
            Err(errors) => (None, errors),
        },
        ParseMode::Recovery => parser.parse_recovery(stream),
    };

    let parse_errors = errors
        .into_iter()
        .map(|error| convert_chumsky_error(error, &tokens))
        .collect();

    RecoveryResult {
        output: nodes.map(Document::new),
        errors: parse_errors,
    }
}

/// Main parse function - strict parity-preserving entry point.
pub fn parse(input: &str) -> ParseResult<Document> {
    let result = parse_with_mode(input, ParseMode::Strict);
    match (result.output, result.errors) {
        (Some(document), errors) if errors.is_empty() => Ok(document),
        (_, errors) => Err(errors),
    }
}

/// Recovery-mode parse entry point that attempts to continue after delimiter/body errors.
pub fn parse_recovery(input: &str) -> RecoveryResult<Document> {
    parse_with_mode(input, ParseMode::Recovery)
}

fn default_recovery_message(context: &str) -> String {
    format!("recovered malformed {context}")
}

fn recover_textual_nodes<P>(
    parser: P,
    mode: ParseMode,
    closing: Token,
    context: &'static str,
) -> BoxedParser<'static, Token, Nodes, Simple<Token>>
where
    P: Parser<Token, Nodes, Error = Simple<Token>> + Clone + 'static,
{
    recover_until_closing(parser, mode, closing, move |span| {
        recovered_error_nodes(default_recovery_message(context), span)
    })
}

fn recover_text_arg<P>(
    parser: P,
    mode: ParseMode,
) -> BoxedParser<'static, Token, String, Simple<Token>>
where
    P: Parser<Token, String, Error = Simple<Token>> + Clone + 'static,
{
    recover_until_closing(parser, mode, Token::RBrace, |_| String::new())
}

fn recover_braced_output<P, O, F>(
    parser: P,
    mode: ParseMode,
    fallback: F,
) -> BoxedParser<'static, Token, O, Simple<Token>>
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone + 'static,
    O: 'static,
    F: Fn(Range<usize>) -> O + Clone + 'static,
{
    recover_nested_delimiters(
        parser,
        mode,
        Token::LBrace,
        Token::RBrace,
        [
            (Token::LSquare, Token::RSquare),
            (Token::LParen, Token::RParen),
        ],
        fallback,
    )
}

fn recover_to_empty<P, O>(
    parser: P,
    mode: ParseMode,
    closing: Token,
) -> BoxedParser<'static, Token, Vec<O>, Simple<Token>>
where
    P: Parser<Token, Vec<O>, Error = Simple<Token>> + Clone + 'static,
    O: 'static,
{
    recover_until_closing(parser, mode, closing, |_| Vec::new())
}

fn recover_group_node<P, const N: usize>(
    parser: P,
    mode: ParseMode,
    start: Token,
    end: Token,
    others: [(Token, Token); N],
    context: &'static str,
) -> BoxedParser<'static, Token, Located<Node>, Simple<Token>>
where
    P: Parser<Token, Located<Node>, Error = Simple<Token>> + Clone + 'static,
{
    recover_nested_delimiters(parser, mode, start, end, others, move |span| {
        recovered_error_node(default_recovery_message(context), span)
    })
}

/// Convert a chumsky Simple error to our ParseError type
fn convert_chumsky_error(
    e: Simple<Token>,
    tokens: &[(Token, std::ops::Range<usize>)],
) -> ParseError {
    if let Some(error) = delimiter_error_for(&e, tokens) {
        return error;
    }

    let span = e.span();

    // Get what was expected
    let expected: Vec<String> = e
        .expected()
        .filter_map(|tok| tok.as_ref().map(|t| format!("'{}'", t)))
        .collect();

    let expected_tokens = ExpectedTokens::new(expected);

    // Get what was found
    let found_str = match e.found() {
        Some(tok) => format!("'{}'", tok),
        None => "end of input".to_string(),
    };

    // Determine error type based on reason
    match e.reason() {
        chumsky::error::SimpleReason::Unexpected => {
            if e.found().is_none() {
                ParseError::UnexpectedEof {
                    expected: expected_tokens,
                    span,
                }
            } else {
                ParseError::UnexpectedToken {
                    expected: expected_tokens,
                    found: found_str,
                    span,
                }
            }
        }
        chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
            ParseError::UnclosedDelimiter {
                delim: quote_delimiter(&delimiter.to_string()),
                expected_close: "'end of input'".to_string(),
                open_span: span.clone(),
                eof_span: e.span(),
            }
        }
        chumsky::error::SimpleReason::Custom(msg) => ParseError::Custom {
            message: msg.clone(),
            span,
        },
    }
}

/// Parse a file
pub fn parse_file(path: &std::path::Path) -> ParseResult<Document> {
    let content = std::fs::read_to_string(path).map_err(|e| {
        vec![ParseError::Custom {
            message: format!("Failed to read file: {}", e),
            span: 0..0,
        }]
    })?;

    let mut doc = parse(&content)?;
    doc.source_path = Some(path.to_string_lossy().to_string());
    Ok(doc)
}

fn ws_or<P, O>(parser: P) -> impl Parser<Token, Vec<O>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    choice((
        select! { Token::Whitespace(_) => Vec::<O>::new() },
        parser.map(|node| vec![node]),
    ))
}

fn ws_list<P, O>(parser: P) -> impl Parser<Token, Vec<O>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    ws_or(parser)
        .repeated()
        .map(|chunks| chunks.into_iter().flatten().collect())
}

fn import_parser() -> impl Parser<Token, Located<Node>, Error = Simple<Token>> + Clone {
    let wstext = select! {
        Token::Text(s) => s,
        Token::Whitespace(s) => s,
    }
    .repeated()
    .map(|parts| parts.join(""));

    just(Token::KwImport)
        .ignore_then(wstext.delimited_by(just(Token::LBrace), just(Token::RBrace)))
        .map_with_span(|target, span| {
            Located::new(
                Node::Import {
                    visibility: Visibility::Private,
                    target,
                },
                Some(make_span_from_range(span)),
            )
        })
}

/// Document parser - parses a sequence of nodes
fn document_parser(mode: ParseMode) -> impl Parser<Token, Nodes, Error = Simple<Token>> + Clone {
    ws_list(choice((import_parser(), parser(mode)))).then_ignore(end())
}

/// Single non-import node parser
fn parser(mode: ParseMode) -> impl Parser<Token, Located<Node>, Error = Simple<Token>> + Clone {
    recursive(|head_node| {
        let plain_text = select! {
            Token::Text(s) => Node::text(s),
            Token::Whitespace(s) => Node::text(s),
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let head_node1_fallback = select! {
            Token::AtSign => Node::text("@"),
            Token::Tick => Node::text("'"),
            Token::Hash => Node::text("#"),
            Token::DxEntailed => Node::text("-:"),
            Token::DxVar(s) => Node::text(format!("?{s}")),
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let head_node1 = choice((head_node.clone(), head_node1_fallback)).boxed();
        let textual_node = choice((plain_text, head_node1.clone())).boxed();
        let textual_expr = textual_node.clone().repeated();
        let code_expr = ws_list(head_node1.clone()).boxed();
        let subtree_body = ws_list(head_node.clone()).boxed();

        let wstext = select! {
            Token::Text(s) => s,
            Token::Whitespace(s) => s,
        }
        .repeated()
        .map(|parts| parts.join(""));

        let txt_arg = recover_braced_output(
            recover_text_arg(wstext, mode).delimited_by(just(Token::LBrace), just(Token::RBrace)),
            mode,
            |_| String::new(),
        );

        let verbatim = select! {
            Token::Verbatim(s) => Node::verbatim(s),
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let braces_arg = recover_braced_output(
            recover_textual_nodes(textual_expr.clone(), mode, Token::RBrace, "argument body")
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            mode,
            |span| recovered_error_nodes(default_recovery_message("argument body"), span),
        );
        let arg = choice((
            select! { Token::Verbatim(s) => s }.map_with_span(|content, span| {
                vec![Located::new(
                    Node::verbatim(content),
                    Some(make_span_from_range(span)),
                )]
            }),
            braces_arg,
        ))
        .boxed();

        let hash_ident = select! {
            Token::HashIdent(s) => Node::HashIdent { name: s },
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let xml_ident = select! {
            Token::XmlIdent(prefix, name) => Node::XmlIdent { prefix, name },
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let braces = recover_group_node(
            recover_textual_nodes(
                textual_expr.clone(),
                mode,
                Token::RBrace,
                "braced group body",
            )
            .clone()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::braces(body), Some(make_span_from_range(span)))
            }),
            mode,
            Token::LBrace,
            Token::RBrace,
            [
                (Token::LSquare, Token::RSquare),
                (Token::LParen, Token::RParen),
            ],
            "braced group",
        );

        let squares = recover_group_node(
            recover_textual_nodes(
                textual_expr.clone(),
                mode,
                Token::RSquare,
                "square group body",
            )
            .clone()
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map_with_span(|body, span| {
                Located::new(Node::squares(body), Some(make_span_from_range(span)))
            }),
            mode,
            Token::LSquare,
            Token::RSquare,
            [
                (Token::LBrace, Token::RBrace),
                (Token::LParen, Token::RParen),
            ],
            "square group",
        );

        let parens = recover_group_node(
            recover_textual_nodes(
                textual_expr.clone(),
                mode,
                Token::RParen,
                "parenthesized group body",
            )
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with_span(|body, span| {
                Located::new(Node::parens(body), Some(make_span_from_range(span)))
            }),
            mode,
            Token::LParen,
            Token::RParen,
            [
                (Token::LBrace, Token::RBrace),
                (Token::LSquare, Token::RSquare),
            ],
            "parenthesized group",
        );

        let inline_math = recover_group_node(
            recover_textual_nodes(
                textual_expr.clone(),
                mode,
                Token::RBrace,
                "inline math body",
            )
            .clone()
            .delimited_by(just(Token::HashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::inline_math(body), Some(make_span_from_range(span)))
            }),
            mode,
            Token::HashLBrace,
            Token::RBrace,
            [
                (Token::LSquare, Token::RSquare),
                (Token::LParen, Token::RParen),
            ],
            "inline math",
        );

        let display_math = recover_group_node(
            recover_textual_nodes(
                textual_expr.clone(),
                mode,
                Token::RBrace,
                "display math body",
            )
            .clone()
            .delimited_by(just(Token::HashHashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::display_math(body), Some(make_span_from_range(span)))
            }),
            mode,
            Token::HashHashLBrace,
            Token::RBrace,
            [
                (Token::LSquare, Token::RSquare),
                (Token::LParen, Token::RParen),
            ],
            "display math",
        );

        let ident_path = select! { Token::Ident(s) => s }
            .separated_by(just(Token::Slash))
            .at_least(1);
        let ident_node = ident_path.clone().map_with_span(|path, span| {
            Located::new(Node::Ident { path }, Some(make_span_from_range(span)))
        });

        let binding = select! { Token::Text(s) => s }
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map(|name| {
                let (info, name) = if let Some(rest) = name.strip_prefix('~') {
                    (BindingInfo::Lazy, rest.to_string())
                } else {
                    (BindingInfo::Strict, name)
                };
                (info, name)
            });
        let bvar = select! { Token::Text(name) => name };
        let method_name = bvar.delimited_by(just(Token::LSquare), just(Token::RSquare));
        let method_decl = method_name
            .clone()
            .then_ignore(select! { Token::Whitespace(_) => () }.repeated())
            .then(arg.clone())
            .boxed();
        let object_self = bvar.delimited_by(just(Token::LSquare), just(Token::RSquare));

        let bindings = binding.repeated();
        let fun_spec = ident_path.clone().then(bindings.clone()).then(arg.clone());
        let code_block = recover_braced_output(
            recover_textual_nodes(code_expr.clone(), mode, Token::RBrace, "code expression")
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            mode,
            |span| recovered_error_nodes(default_recovery_message("code expression"), span),
        );
        let method_block = recover_braced_output(
            recover_to_empty(ws_list(method_decl.clone()), mode, Token::RBrace)
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            mode,
            |_| Vec::new(),
        );

        let def_cmd = just(Token::KwDef)
            .ignore_then(fun_spec.clone())
            .map_with_span(|((path, bindings), body), span| {
                Located::new(
                    Node::Def {
                        path,
                        bindings,
                        body,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        let alloc_cmd = just(Token::KwAlloc)
            .ignore_then(ident_path.clone())
            .map_with_span(|path, span| {
                Located::new(Node::Alloc { path }, Some(make_span_from_range(span)))
            });

        let export_cmd = just(Token::KwExport)
            .ignore_then(txt_arg.clone())
            .map_with_span(|target, span| {
                Located::new(
                    Node::Import {
                        visibility: Visibility::Public,
                        target,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        let namespace_cmd = just(Token::KwNamespace)
            .ignore_then(ident_path.clone())
            .then(code_block.clone())
            .map_with_span(|(path, body), span| {
                Located::new(
                    Node::Namespace { path, body },
                    Some(make_span_from_range(span)),
                )
            });

        let fun_cmd = just(Token::KwFun)
            .ignore_then(bindings.clone())
            .then(arg.clone())
            .map_with_span(|(bindings, body), span| {
                Located::new(
                    Node::Fun { bindings, body },
                    Some(make_span_from_range(span)),
                )
            });

        let let_cmd = just(Token::KwLet).ignore_then(fun_spec).map_with_span(
            |((path, bindings), body), span| {
                Located::new(
                    Node::Let {
                        path,
                        bindings,
                        body,
                    },
                    Some(make_span_from_range(span)),
                )
            },
        );

        let open_cmd = just(Token::KwOpen)
            .ignore_then(ident_path.clone())
            .map_with_span(|path, span| {
                Located::new(Node::Open { path }, Some(make_span_from_range(span)))
            });

        let scope_cmd =
            just(Token::KwScope)
                .ignore_then(arg.clone())
                .map_with_span(|body, span| {
                    Located::new(Node::Scope { body }, Some(make_span_from_range(span)))
                });

        let put_cmd = just(Token::KwPut)
            .ignore_then(ident_path.clone())
            .then(arg.clone())
            .map_with_span(|(path, body), span| {
                Located::new(Node::Put { path, body }, Some(make_span_from_range(span)))
            });

        let default_cmd = just(Token::KwDefault)
            .ignore_then(ident_path.clone())
            .then(arg.clone())
            .map_with_span(|(path, body), span| {
                Located::new(
                    Node::Default { path, body },
                    Some(make_span_from_range(span)),
                )
            });

        let get_cmd = just(Token::KwGet)
            .ignore_then(ident_path.clone())
            .map_with_span(|path, span| {
                Located::new(Node::Get { path }, Some(make_span_from_range(span)))
            });

        let decl_xmlns_cmd = select! { Token::DeclXmlns(prefix) => prefix }
            .then(txt_arg.clone())
            .map_with_span(|(prefix, uri), span| {
                Located::new(
                    Node::DeclXmlns { prefix, uri },
                    Some(make_span_from_range(span)),
                )
            });

        let subtree_cmd = just(Token::KwSubtree)
            .ignore_then(
                wstext
                    .delimited_by(just(Token::LSquare), just(Token::RSquare))
                    .or_not(),
            )
            .then(recover_braced_output(
                recover_textual_nodes(subtree_body, mode, Token::RBrace, "subtree body")
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
                mode,
                |span| recovered_error_nodes(default_recovery_message("subtree body"), span),
            ))
            .map_with_span(|(addr, body), span| {
                Located::new(
                    Node::Subtree { addr, body },
                    Some(make_span_from_range(span)),
                )
            });

        let object_cmd = just(Token::KwObject)
            .ignore_then(object_self.clone().or_not())
            .then(method_block.clone())
            .map_with_span(|(self_name, methods), span| {
                Located::new(
                    Node::Object {
                        def: ObjectDef { self_name, methods },
                    },
                    Some(make_span_from_range(span)),
                )
            })
            .boxed();

        let patch_bindings = choice((
            object_self
                .clone()
                .then(object_self.clone())
                .map(|(self_name, super_name)| (Some(self_name), Some(super_name))),
            object_self.clone().map(|self_name| (Some(self_name), None)),
            empty().to((None::<String>, None::<String>)),
        ))
        .boxed();

        let patch_cmd = just(Token::KwPatch)
            .ignore_then(code_block.clone())
            .then(patch_bindings)
            .then(method_block.clone())
            .map_with_span(|((obj, (self_name, super_name)), methods), span| {
                Located::new(
                    Node::Patch {
                        def: PatchDef {
                            obj,
                            self_name,
                            super_name,
                            methods,
                        },
                    },
                    Some(make_span_from_range(span)),
                )
            })
            .boxed();

        let call_cmd = just(Token::KwCall)
            .ignore_then(code_block.clone())
            .then(txt_arg.clone())
            .map_with_span(|(target, method), span| {
                Located::new(
                    Node::Call { target, method },
                    Some(make_span_from_range(span)),
                )
            })
            .boxed();

        let dx_rel = choice((
            ident_node.clone().map(|node| vec![node]),
            just(Token::Tick).ignore_then(arg.clone()),
        ))
        .boxed();

        let dx_term_node = choice((
            select! { Token::DxVar(name) => Node::DxVar { name } },
            just(Token::Tick)
                .ignore_then(arg.clone())
                .map(|body| Node::DxConstContent { body }),
            just(Token::AtSign)
                .ignore_then(arg.clone())
                .map(|body| Node::DxConstUri { body }),
        ))
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))))
        .boxed();

        let dx_term = dx_term_node.clone().map(|node| vec![node]);

        let dx_prop_node = dx_rel
            .clone()
            .then(ws_list(dx_term.clone()))
            .map(|(relation, args)| Node::DxProp { relation, args })
            .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))))
            .boxed();

        let dx_prop = dx_prop_node.clone().map(|node| vec![node]);
        let dx_premise = dx_prop
            .clone()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .boxed();
        let dx_negative_premises = just(Token::Hash)
            .ignore_then(ws_list(dx_premise.clone()))
            .boxed();

        let dx_query_var = select! { Token::DxVar(name) => name };
        let dx_interstitial_ws = select! { Token::Whitespace(_) => () }.repeated();

        let dx_sequent_node = choice((
            dx_prop
                .clone()
                .then_ignore(just(Token::DxEntailed))
                .then(ws_list(dx_premise.clone()))
                .map(|(conclusion, premises)| Node::DxSequent {
                    conclusion,
                    premises,
                }),
            dx_query_var
                .then_ignore(dx_interstitial_ws)
                .then_ignore(just(Token::DxEntailed))
                .then(ws_list(dx_premise.clone()))
                .then(dx_negative_premises.or_not())
                .map(|((var, positives), negatives)| Node::DxQuery {
                    var,
                    positives,
                    negatives: negatives.unwrap_or_default(),
                }),
        ))
        .boxed();

        let datalog_cmd = just(Token::KwDatalog)
            .ignore_then(
                select! { Token::Whitespace(_) => () }
                    .repeated()
                    .ignore_then(recover_until_closing(
                        dx_sequent_node,
                        mode,
                        Token::RBrace,
                        |_| Node::Error {
                            message: default_recovery_message("datalog body"),
                        },
                    ))
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))))
            .boxed();

        let generic_cmd = ident_node;

        let command_like = choice((
            def_cmd,
            alloc_cmd,
            export_cmd,
            namespace_cmd,
            subtree_cmd,
            fun_cmd,
            let_cmd,
            object_cmd,
            patch_cmd,
            call_cmd,
            scope_cmd,
            put_cmd,
            default_cmd,
            get_cmd,
            open_cmd,
            xml_ident,
            decl_xmlns_cmd,
            datalog_cmd,
            generic_cmd,
        ))
        .boxed();

        choice((
            command_like,
            hash_ident,
            verbatim,
            inline_math,
            display_math,
            braces,
            squares,
            parens,
        ))
    })
}

fn make_span_from_range(range: std::ops::Range<usize>) -> Span {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_title() {
        let result = parse("\\title{Hello World}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["title".to_string()])
        );
        assert!(matches!(
            &doc.nodes[1].value,
            Node::Group {
                delim: Delim::Braces,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_p() {
        let result = parse("\\p{This is a paragraph.}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()])
        );
        assert!(matches!(
            &doc.nodes[1].value,
            Node::Group {
                delim: Delim::Braces,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_combined() {
        let input = r#"
\title{My Document}
\p{First paragraph.}
\p{Second paragraph with #{math}.}
"#;
        let result = parse(input);
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 6);
    }

    #[test]
    fn test_parse_import() {
        let result = parse("\\import{foundation}");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_import_with_whitespace_target() {
        let result = parse("\\import{foundation / intro}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(
            matches!(&doc.nodes[0].value, Node::Import { target, .. } if target == "foundation / intro")
        );
    }

    #[test]
    fn test_nested_import_is_rejected() {
        let result = parse("\\p{before \\import{foundation} after}");
        assert!(result.is_err());
    }

    #[test]
    fn test_nested_export_is_allowed() {
        let result = parse("\\p{before \\export{foundation} after}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
    }

    #[test]
    fn test_parse_def() {
        let result = parse("\\def\\myMacro[x]{Content with #x}");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_def_fun_spec_with_path_and_binders() {
        let result = parse("\\def\\alpha/beta[~x][y]{z}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Def { path, bindings, body }
                if path == &vec!["alpha".to_string(), "beta".to_string()]
                && bindings == &vec![
                    (BindingInfo::Lazy, "x".to_string()),
                    (BindingInfo::Strict, "y".to_string())
                ]
                && matches!(body.as_slice(), [Located { value: Node::Text { content }, .. }] if content == "z")
        ));
    }

    #[test]
    fn test_parse_let_fun_spec_with_path_and_binders() {
        let result = parse("\\let\\alpha/beta[~x][y]{z}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Let { path, bindings, body }
                if path == &vec!["alpha".to_string(), "beta".to_string()]
                && bindings == &vec![
                    (BindingInfo::Lazy, "x".to_string()),
                    (BindingInfo::Strict, "y".to_string())
                ]
                && matches!(body.as_slice(), [Located { value: Node::Text { content }, .. }] if content == "z")
        ));
    }

    #[test]
    fn test_binder_rejects_backslash_command_content() {
        let result = parse("\\def\\foo[\\bar]{z}");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_unclosed_brace() {
        let input = "\\title{Hello";
        let result = parse(input);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(matches!(
            &errors[0],
            ParseError::UnclosedDelimiter {
                delim,
                expected_close,
                open_span,
                eof_span,
            } if delim == "'{'" && expected_close == "'}'" && open_span.start < eof_span.start
        ));
        let report = errors[0].report("test.tree", input);
        assert!(report.contains("opened here"));
        assert!(report.contains("expected '}' before end of input"));
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let input = "\\title}";
        let result = parse(input);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(matches!(
            &errors[0],
            ParseError::UnexpectedClosingDelimiter { found_close, .. }
                if found_close == "'}'"
        ));
        let report = errors[0].report("test.tree", input);
        assert!(report.to_lowercase().contains("error"));
    }

    #[test]
    fn test_parse_recovery_reports_multiple_group_body_errors() {
        let input = "\\p{]}\n\\p{)}\n\\p{ok}";
        let recovery = parse_recovery(input);

        assert!(recovery.output.is_some());
        assert!(recovery.errors.len() >= 2);

        let doc = recovery.output.expect("recovered document");
        assert_eq!(doc.nodes.len(), 6);

        assert!(matches!(&doc.nodes[1].value, Node::Error { .. }));
        assert!(matches!(&doc.nodes[3].value, Node::Error { .. }));

        let Node::Group {
            body: third_body, ..
        } = &doc.nodes[5].value
        else {
            panic!("expected third group");
        };
        assert!(matches!(
            third_body.as_slice(),
            [Located {
                value: Node::Text { content },
                ..
            }] if content == "ok"
        ));
    }

    #[test]
    fn test_parse_recovery_keeps_following_nodes_after_code_block_error() {
        let input = "\\namespace\\alpha{]}\n\\p{tail}";
        let recovery = parse_recovery(input);

        assert!(recovery.output.is_some());
        assert!(!recovery.errors.is_empty());

        let doc = recovery.output.expect("recovered document");
        assert_eq!(doc.nodes.len(), 3);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Namespace { path, body }
                if path == &vec!["alpha".to_string()]
                && matches!(
                    body.as_slice(),
                    [Located {
                        value: Node::Error { .. },
                        ..
                    }]
                )
        ));
        assert!(
            matches!(&doc.nodes[1].value, Node::Ident { path } if path == &vec!["p".to_string()])
        );
    }

    #[test]
    fn test_parse_recovery_recovers_nested_delimiter_mismatch() {
        let input = "\\p{([)]}\n\\p{tail}";
        let recovery = parse_recovery(input);

        assert!(recovery.output.is_some());
        assert!(!recovery.errors.is_empty());

        let doc = recovery.output.expect("recovered document");
        assert_eq!(doc.nodes.len(), 4);

        assert!(matches!(
            &doc.nodes[1].value,
            Node::Error { .. } | Node::Group { .. }
        ));
        assert!(
            matches!(&doc.nodes[2].value, Node::Ident { path } if path == &vec!["p".to_string()])
        );
    }

    #[test]
    fn test_mismatched_delimiter_tracks_opening_delimiter() {
        let input = "(abc]";
        let errors = parse(input).expect_err("mismatched delimiter should fail");
        assert!(matches!(
            &errors[0],
            ParseError::MismatchedDelimiter {
                open_delim,
                expected_close,
                found_close,
                open_span,
                found_span,
            } if open_delim == "'('" && expected_close == "')'" && found_close == "']'" && open_span.start == 0 && found_span.start == 4
        ));
        let report = errors[0].report("test.tree", input);
        assert!(report.contains("'(' opened here"));
        assert!(report.contains("found ']' here, but ')' was required"));
    }

    #[test]
    fn test_nested_mismatched_delimiter_reports_innermost_opening() {
        let input = "([)]";
        let errors = parse(input).expect_err("nested mismatched delimiter should fail");
        assert!(matches!(
            &errors[0],
            ParseError::MismatchedDelimiter {
                open_delim,
                expected_close,
                found_close,
                open_span,
                found_span,
            } if open_delim == "'['" && expected_close == "']'" && found_close == "')'" && open_span.start == 1 && found_span.start == 2
        ));
    }

    #[test]
    fn test_unexpected_closing_delimiter_is_classified_explicitly() {
        let input = "]";
        let errors = parse(input).expect_err("stray closing delimiter should fail");
        assert!(matches!(
            &errors[0],
            ParseError::UnexpectedClosingDelimiter { found_close, span }
                if found_close == "']'" && span.start == 0 && span.end == 1
        ));
        let report = errors[0].report("test.tree", input);
        assert!(report.contains("does not match any currently open delimiter"));
    }

    #[test]
    fn test_unclosed_inline_math_reports_hash_opening() {
        let input = "#{abc";
        let errors = parse(input).expect_err("unterminated inline math should fail");
        assert!(matches!(
            &errors[0],
            ParseError::UnclosedDelimiter {
                delim,
                expected_close,
                ..
            } if delim == "'#{'" && expected_close == "'}'"
        ));
    }

    #[test]
    fn test_error_display() {
        let input = "\\def{missing}";
        let result = parse(input);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        // Generate ariadne report
        for err in &errors {
            let report = err.report("test.tree", input);
            println!("Error report:\n{}", report);
            assert!(!report.is_empty());
        }
    }

    #[test]
    fn test_parse_escaped_punctuation_in_text() {
        let input = r"\p{Escaped \% \{ \} \[ \] \# \\ punctuation.}";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_link_targets_with_paths_and_urls() {
        let input = r"\p{See \ref{alpha/beta}, \link{alpha/beta}{Internal}, and \link{https://openai.com}{OpenAI}.}";
        let result = parse(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_plain_keywords_remain_text_in_textual_expr() {
        let result = parse(r"\p{scope import def}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected braced group");
        };
        assert_eq!(body.len(), 5);
        assert!(matches!(&body[0].value, Node::Text { content } if content == "scope"));
        assert!(matches!(&body[1].value, Node::Text { content } if content == " "));
        assert!(matches!(&body[2].value, Node::Text { content } if content == "import"));
        assert!(matches!(&body[3].value, Node::Text { content } if content == " "));
        assert!(matches!(&body[4].value, Node::Text { content } if content == "def"));
    }

    #[test]
    fn test_top_level_plain_text_is_rejected() {
        let result = parse("scope import def");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_generic_ident_path_without_backslash_token() {
        let result = parse(r"\alpha/beta");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["alpha".to_string(), "beta".to_string()])
        );
    }

    #[test]
    fn test_parse_verbatim_node() {
        let result = parse("\\startverb\nhello\n\\stopverb");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(&doc.nodes[0].value, Node::Verbatim { content } if content == "hello"));
    }

    #[test]
    fn test_head_node1_tokens_fall_back_to_text() {
        let result = parse(r"\p{?- ?foo -: # @ '}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected braced group");
        };
        assert_eq!(body.len(), 11);
        assert!(matches!(&body[0].value, Node::Text { content } if content == "?-"));
        assert!(matches!(&body[2].value, Node::Text { content } if content == "?foo"));
        assert!(matches!(&body[4].value, Node::Text { content } if content == "-:"));
        assert!(matches!(&body[6].value, Node::Text { content } if content == "#"));
        assert!(matches!(&body[8].value, Node::Text { content } if content == "@"));
        assert!(matches!(&body[10].value, Node::Text { content } if content == "'"));
    }

    #[test]
    fn test_bare_question_falls_back_to_text() {
        let result = parse(r"\p{?}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected braced group");
        };
        assert_eq!(body.len(), 1);
        assert!(matches!(&body[0].value, Node::Text { content } if content == "?" ));
    }

    #[test]
    fn test_lexer_error_surfaces_with_ocaml_like_message() {
        let result = parse("\\!");
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors[0].to_string(), "syntax error, unexpected \"!\"");
        assert_eq!(errors[0].span(), 1..2);
    }

    #[test]
    fn test_parse_xml_ident_node() {
        let result = parse("\\<svg:path>");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(
            matches!(&doc.nodes[0].value, Node::XmlIdent { prefix, name } if prefix.as_deref() == Some("svg") && name == "path")
        );
    }

    #[test]
    fn test_parse_decl_xmlns_node() {
        let result = parse("\\xmlns:svg{https://example.com/svg}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(
            matches!(&doc.nodes[0].value, Node::DeclXmlns { prefix, uri } if prefix == "svg" && uri == "https://example.com/svg")
        );
    }

    #[test]
    fn test_top_level_whitespace_is_dropped() {
        let result = parse(" \n\t\\title{Hello}\n");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["title".to_string()])
        );
        assert!(matches!(
            &doc.nodes[1].value,
            Node::Group {
                delim: Delim::Braces,
                ..
            }
        ));
    }

    #[test]
    fn test_textual_whitespace_is_preserved() {
        let result = parse("\\p{alpha  \n\tbeta}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()])
        );
        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected braced argument group");
        };
        assert_eq!(body.len(), 5);
        assert!(matches!(&body[0].value, Node::Text { content } if content == "alpha"));
        assert!(matches!(&body[1].value, Node::Text { content } if content == "  "));
        assert!(matches!(&body[2].value, Node::Text { content } if content == "\n"));
        assert!(matches!(&body[3].value, Node::Text { content } if content == "\t"));
        assert!(matches!(&body[4].value, Node::Text { content } if content == "beta"));
    }

    #[test]
    fn test_comments_are_consumed_by_lexer() {
        let result = parse("% comment\n\\title{Hello}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
    }

    #[test]
    fn test_comment_swallowing_preserves_following_blank_line() {
        let result = parse("\\p{alpha% comment\n\nbeta}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert!(
            matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()])
        );
        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected braced argument group");
        };
        assert!(matches!(&body[0].value, Node::Text { content } if content == "alpha"));
        assert!(matches!(&body[1].value, Node::Text { content } if content == "\n"));
        assert!(matches!(&body[2].value, Node::Text { content } if content == "beta"));
    }

    #[test]
    fn test_parse_alloc_open_and_get_nodes() {
        let result = parse("\\alloc\\alpha/beta \\open\\gamma \\get\\delta/epsilon");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 3);
        assert!(
            matches!(&doc.nodes[0].value, Node::Alloc { path } if path == &vec!["alpha".to_string(), "beta".to_string()])
        );
        assert!(
            matches!(&doc.nodes[1].value, Node::Open { path } if path == &vec!["gamma".to_string()])
        );
        assert!(
            matches!(&doc.nodes[2].value, Node::Get { path } if path == &vec!["delta".to_string(), "epsilon".to_string()])
        );
    }

    #[test]
    fn test_parse_put_and_default_with_verbatim_args() {
        let result = parse(
            "\\put\\alpha/beta\\verbEND|payloadEND \\put?\\gamma\\startverb\nfallback\n\\stopverb",
        );
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Put { path, body }
                if path == &vec!["alpha".to_string(), "beta".to_string()]
                && matches!(body.as_slice(), [Located { value: Node::Verbatim { content }, .. }] if content == "payload")
        ));
        assert!(matches!(
            &doc.nodes[1].value,
            Node::Default { path, body }
                if path == &vec!["gamma".to_string()]
                && matches!(body.as_slice(), [Located { value: Node::Verbatim { content }, .. }] if content == "fallback")
        ));
    }

    #[test]
    fn test_parse_fun_and_scope_verbatim_args() {
        let result = parse("\\fun[~x][y]{body} \\scope\\verbEND|hiddenEND");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 2);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Fun { bindings, body }
                if bindings == &vec![
                    (BindingInfo::Lazy, "x".to_string()),
                    (BindingInfo::Strict, "y".to_string())
                ]
                && matches!(body.as_slice(), [Located { value: Node::Text { content }, .. }] if content == "body")
        ));
        assert!(matches!(
            &doc.nodes[1].value,
            Node::Scope { body }
                if matches!(body.as_slice(), [Located { value: Node::Verbatim { content }, .. }] if content == "hidden")
        ));
    }

    #[test]
    fn test_namespace_uses_code_expr_and_drops_interstitial_whitespace() {
        let result = parse("\\namespace\\alpha/beta{\\title{A} \\p{B}}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Namespace { path, body }
                if path == &vec!["alpha".to_string(), "beta".to_string()]
                && body.len() == 4
                && matches!(&body[0].value, Node::Ident { path } if path == &vec!["title".to_string()])
                && matches!(&body[2].value, Node::Ident { path } if path == &vec!["p".to_string()])
        ));
    }

    #[test]
    fn test_namespace_rejects_raw_text_body() {
        let result = parse("\\namespace\\alpha{plain}");
        assert!(result.is_err());
    }

    #[test]
    fn test_subtree_accepts_wstext_address_and_head_nodes_only_body() {
        let result = parse("\\subtree[foundation / intro]{\\title{A}\\p{B}}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Subtree { addr, body }
                if addr.as_deref() == Some("foundation / intro") && body.len() == 4
        ));
    }

    #[test]
    fn test_subtree_rejects_raw_text_body() {
        let result = parse("\\subtree{plain text}");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_datalog_sequent_with_uri_term() {
        let result =
            parse("\\datalog{\\rel/links-to ?X @{https://example.com} -: {\\rel/is-node ?X}}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::DxSequent { conclusion, premises }
                if premises.len() == 1
                && matches!(
                    conclusion.as_slice(),
                    [Located {
                        value: Node::DxProp { relation, args },
                        ..
                    }] if matches!(
                        relation.as_slice(),
                        [Located { value: Node::Ident { path }, .. }]
                            if path == &vec!["rel".to_string(), "links-to".to_string()]
                    ) && args.len() == 2
                        && matches!(args[0].as_slice(), [Located { value: Node::DxVar { name }, .. }] if name == "X")
                        && matches!(args[1].as_slice(), [Located { value: Node::DxConstUri { body }, .. }] if matches!(body.as_slice(), [Located { value: Node::Text { content }, .. }] if content == "https://example.com"))
                )
        ));
    }

    #[test]
    fn test_parse_datalog_query_with_negative_premises() {
        let result = parse(
            "\\datalog{\n  ?related -: {\\rel/links-to @{example} ?related} # {\\rel/hidden ?related}\n}",
        );
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::DxQuery {
                var,
                positives,
                negatives
            } if var == "related" && positives.len() == 1 && negatives.len() == 1
        ));
    }

    #[test]
    fn test_parse_datalog_content_constant_term() {
        let result = parse("\\datalog{?X -: {\\rel/has-tag ?X '{tag}}}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::DxQuery { positives, .. }
                if matches!(
                    positives[0].as_slice(),
                    [Located {
                        value: Node::DxProp { args, .. },
                        ..
                    }] if args.len() == 2
                        && matches!(args[1].as_slice(), [Located { value: Node::DxConstContent { body }, .. }] if matches!(body.as_slice(), [Located { value: Node::Text { content }, .. }] if content == "tag"))
                )
        ));
    }

    #[test]
    fn test_parse_object_with_self_and_verbatim_method() {
        let result = parse("\\object[self]{[render]{\\p{Body}} [raw]\\verbEND|literalEND}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Object { def }
                if def.self_name.as_deref() == Some("self")
                && def.methods.len() == 2
                && def.methods[0].0 == "render"
                && matches!(&def.methods[0].1[..], [Located { value: Node::Ident { path }, .. }, Located { value: Node::Group { .. }, .. }] if path == &vec!["p".to_string()])
                && def.methods[1].0 == "raw"
                && matches!(&def.methods[1].1[..], [Located { value: Node::Verbatim { content }, .. }] if content == "literal")
        ));
    }

    #[test]
    fn test_parse_patch_with_self_super_and_methods() {
        let result = parse("\\patch{\\object{[render]{base}}}[self][super]{[render]{override}}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Patch { def }
                if def.self_name.as_deref() == Some("self")
                && def.super_name.as_deref() == Some("super")
                && def.obj.len() == 1
                && matches!(&def.obj[0].value, Node::Object { .. })
                && def.methods.len() == 1
                && def.methods[0].0 == "render"
        ));
    }

    #[test]
    fn test_parse_call_with_code_expr_target_and_txt_arg_method() {
        let result = parse("\\call{\\ref{alpha/beta}}{render html}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(
            &doc.nodes[0].value,
            Node::Call { target, method }
                if method == "render html"
                && target.len() == 2
                && matches!(&target[0].value, Node::Ident { path } if path == &vec!["ref".to_string()])
                && matches!(&target[1].value, Node::Group { .. })
        ));
    }

    #[test]
    fn test_node_spans_track_multiline_offsets() {
        let input = "\\title{Hello}\n\\p{World}";
        let doc = parse(input).expect("parser should succeed");

        let title_span = doc.nodes[0].span.as_ref().expect("missing title span");
        assert_eq!(title_span.start.offset, 1);
        assert_eq!(title_span.start.line, 1);
        assert_eq!(title_span.start.column, 2);
        assert_eq!(title_span.end.offset, 6);
        assert_eq!(title_span.end.line, 1);
        assert_eq!(title_span.end.column, 7);

        let title_group_span = doc.nodes[1]
            .span
            .as_ref()
            .expect("missing title group span");
        assert_eq!(title_group_span.start.offset, 6);
        assert_eq!(title_group_span.start.line, 1);
        assert_eq!(title_group_span.start.column, 7);
        assert_eq!(title_group_span.end.offset, 13);
        assert_eq!(title_group_span.end.line, 1);
        assert_eq!(title_group_span.end.column, 14);

        let Node::Group { body, .. } = &doc.nodes[1].value else {
            panic!("expected title group");
        };
        let hello_span = body[0].span.as_ref().expect("missing text span");
        assert_eq!(hello_span.start.offset, 7);
        assert_eq!(hello_span.start.line, 1);
        assert_eq!(hello_span.start.column, 8);
        assert_eq!(hello_span.end.offset, 12);
        assert_eq!(hello_span.end.line, 1);
        assert_eq!(hello_span.end.column, 13);

        let paragraph_span = doc.nodes[2].span.as_ref().expect("missing paragraph span");
        assert_eq!(paragraph_span.start.offset, 15);
        assert_eq!(paragraph_span.start.line, 2);
        assert_eq!(paragraph_span.start.column, 2);
        assert_eq!(paragraph_span.end.offset, 16);
        assert_eq!(paragraph_span.end.line, 2);
        assert_eq!(paragraph_span.end.column, 3);
    }
}
