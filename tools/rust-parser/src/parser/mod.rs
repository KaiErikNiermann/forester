// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Parser for Forester markup language using Chumsky

mod recovery;
mod support;

#[cfg(test)]
mod tests;

use crate::ast::*;
use crate::error::{ParseError, ParseResult};
use crate::lexer::{tokenize, Token};
use chumsky::prelude::*;
use recovery::{
    default_recovery_message, recover_braced_output, recover_group_node, recover_text_arg,
    recover_textual_nodes, recover_to_empty, recover_until_closing, recovered_error_nodes,
};
use schemars::JsonSchema;
use serde::Serialize;
use std::str::FromStr;
use support::{convert_chumsky_error, make_span_from_range, set_current_parse_source};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, JsonSchema, Default)]
#[serde(rename_all = "snake_case")]
pub enum ParseMode {
    #[default]
    Strict,
    Recovery,
}

impl ParseMode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Strict => "strict",
            Self::Recovery => "recovery",
        }
    }

    pub(super) fn enables_recovery(self) -> bool {
        matches!(self, Self::Recovery)
    }
}

impl FromStr for ParseMode {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "strict" => Ok(Self::Strict),
            "recovery" => Ok(Self::Recovery),
            _ => Err(format!(
                "Unknown parse mode '{value}'; expected 'strict' or 'recovery'"
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecoveryResult<T> {
    pub output: Option<T>,
    pub errors: Vec<ParseError>,
}

pub fn parse_with_mode(input: &str, mode: ParseMode) -> RecoveryResult<Document> {
    let _guard = set_current_parse_source(input);

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
fn ws_or<P, O>(parser: P) -> impl Parser<Token, Vec<O>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    choice((
        select! { Token::Whitespace(s) if is_code_whitespace(&s) => Vec::<O>::new() },
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

fn is_code_whitespace(s: &str) -> bool {
    s.chars().all(|ch| matches!(ch, ' ' | '\n' | '\r'))
}

fn top_level_ws_or<P, O>(parser: P) -> impl Parser<Token, Vec<O>, Error = Simple<Token>> + Clone
where
    P: Parser<Token, O, Error = Simple<Token>> + Clone,
{
    choice((
        select! { Token::Whitespace(s) if is_code_whitespace(&s) => Vec::<O>::new() },
        parser.map(|node| vec![node]),
    ))
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
    top_level_ws_or(choice((import_parser(), parser(mode))))
        .repeated()
        .map(|chunks| chunks.into_iter().flatten().collect())
        .then_ignore(end())
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
