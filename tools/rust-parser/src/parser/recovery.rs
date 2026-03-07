// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use super::support::make_span_from_range;
use super::ParseMode;
use crate::ast::*;
use crate::lexer::Token;
use chumsky::prelude::*;
use std::ops::Range;

pub(super) fn recovered_error_node(
    message: impl Into<String>,
    span: Range<usize>,
) -> Located<Node> {
    Located::new(
        Node::Error {
            message: message.into(),
        },
        Some(make_span_from_range(span)),
    )
}

pub(super) fn recovered_error_nodes(message: impl Into<String>, span: Range<usize>) -> Nodes {
    vec![recovered_error_node(message, span)]
}

pub(super) fn recover_nested_delimiters<P, O, F, const N: usize>(
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

pub(super) fn recover_until_closing<P, O, F>(
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

pub(super) fn recover_through_closing<P, O, F>(
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

    let closing_direct = just(closing.clone()).map_with_span({
        let fallback = fallback.clone();
        move |_: Token, span| fallback(span)
    });

    let closing_after_skip = filter({
        let closing = closing.clone();
        move |token: &Token| token != &closing
    })
    .repeated()
    .at_least(1)
    .then_ignore(just(closing).or_not())
    .map_with_span(move |_: Vec<Token>, span| fallback(span));

    parser
        .recover_with(skip_parser(choice((closing_direct, closing_after_skip))))
        .boxed()
}

pub(super) fn default_recovery_message(context: &str) -> String {
    format!("recovered malformed {context}")
}

pub(super) fn recover_textual_nodes<P>(
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

pub(super) fn recover_text_arg<P>(
    parser: P,
    mode: ParseMode,
) -> BoxedParser<'static, Token, String, Simple<Token>>
where
    P: Parser<Token, String, Error = Simple<Token>> + Clone + 'static,
{
    recover_until_closing(parser, mode, Token::RBrace, |_| String::new())
}

pub(super) fn recover_braced_output<P, O, F>(
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

pub(super) fn recover_to_empty<P, O>(
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

pub(super) fn recover_group_node<P, const N: usize>(
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
