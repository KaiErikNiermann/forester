// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Parser for Forester markup language using Chumsky

use crate::ast::*;
use crate::error::{ParseError, ParseResult};
use crate::lexer::{tokenize, Token};
use chumsky::prelude::*;

/// Convert lexer span to AST span
#[allow(dead_code)]
fn make_span(input: &str, range: std::ops::Range<usize>) -> Span {
    let start = compute_position(input, range.start);
    let end = compute_position(input, range.end);
    Span::new(start, end)
}

/// Compute line/column from offset
#[allow(dead_code)]
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

/// Main parse function - entry point
pub fn parse(input: &str) -> ParseResult<Document> {
    // Tokenize
    let spanned_tokens = tokenize(input)?;

    // Convert to format chumsky expects
    let tokens: Vec<(Token, std::ops::Range<usize>)> = spanned_tokens
        .into_iter()
        .map(|st| (st.token, st.span))
        .collect();

    // Create stream
    let len = input.len();
    let stream = chumsky::Stream::from_iter(len..len + 1, tokens.into_iter());

    // Parse
    let parser = document_parser();
    match parser.parse(stream) {
        Ok(nodes) => Ok(Document::new(nodes)),
        Err(errors) => {
            let parse_errors: Vec<ParseError> =
                errors.into_iter().map(convert_chumsky_error).collect();
            Err(parse_errors)
        }
    }
}

/// Convert a chumsky Simple error to our ParseError type
fn convert_chumsky_error(e: Simple<Token>) -> ParseError {
    let span = e.span();

    // Get what was expected
    let expected: Vec<String> = e
        .expected()
        .filter_map(|tok| tok.as_ref().map(|t| format!("'{}'", t)))
        .collect();

    let expected_str = if expected.is_empty() {
        "end of input".to_string()
    } else if expected.len() == 1 {
        expected[0].clone()
    } else {
        let last = expected.last().unwrap().clone();
        let rest: Vec<_> = expected[..expected.len() - 1].to_vec();
        format!("{} or {}", rest.join(", "), last)
    };

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
                    expected: expected_str,
                    span,
                }
            } else {
                ParseError::UnexpectedToken {
                    expected: expected_str,
                    found: found_str,
                    span,
                }
            }
        }
        chumsky::error::SimpleReason::Unclosed {
            span: open_span,
            delimiter,
        } => ParseError::UnclosedDelimiter {
            delim: format!("{}", delimiter),
            open_span: open_span.clone(),
        },
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
fn document_parser() -> impl Parser<Token, Nodes, Error = Simple<Token>> + Clone {
    ws_list(choice((import_parser(), parser()))).then_ignore(end())
}

/// Single non-import node parser
fn parser() -> impl Parser<Token, Located<Node>, Error = Simple<Token>> + Clone {
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

        let head_node1 = choice((head_node.clone(), head_node1_fallback));
        let textual_node = choice((plain_text, head_node1.clone()));
        let textual_expr = textual_node.clone().repeated();
        let code_expr = ws_list(head_node1.clone());
        let subtree_body =
            ws_list(head_node.clone()).delimited_by(just(Token::LBrace), just(Token::RBrace));

        let wstext = select! {
            Token::Text(s) => s,
            Token::Whitespace(s) => s,
        }
        .repeated()
        .map(|parts| parts.join(""));

        let txt_arg = wstext.delimited_by(just(Token::LBrace), just(Token::RBrace));

        let verbatim = select! {
            Token::Verbatim(s) => Node::verbatim(s),
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let braces_arg = textual_expr
            .clone()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));
        let arg = choice((
            select! { Token::Verbatim(s) => s }.map_with_span(|content, span| {
                vec![Located::new(
                    Node::verbatim(content),
                    Some(make_span_from_range(span)),
                )]
            }),
            braces_arg,
        ));

        let hash_ident = select! {
            Token::HashIdent(s) => Node::HashIdent { name: s },
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let xml_ident = select! {
            Token::XmlIdent(prefix, name) => Node::XmlIdent { prefix, name },
        }
        .map_with_span(|node, span| Located::new(node, Some(make_span_from_range(span))));

        let braces = textual_expr
            .clone()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::braces(body), Some(make_span_from_range(span)))
            });

        let squares = textual_expr
            .clone()
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map_with_span(|body, span| {
                Located::new(Node::squares(body), Some(make_span_from_range(span)))
            });

        let parens = textual_expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with_span(|body, span| {
                Located::new(Node::parens(body), Some(make_span_from_range(span)))
            });

        let inline_math = textual_expr
            .clone()
            .delimited_by(just(Token::HashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::inline_math(body), Some(make_span_from_range(span)))
            });

        let display_math = textual_expr
            .clone()
            .delimited_by(just(Token::HashHashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::display_math(body), Some(make_span_from_range(span)))
            });

        let ident_path = select! { Token::Ident(s) => s }
            .separated_by(just(Token::Slash))
            .at_least(1);

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

        let bindings = binding.repeated();
        let fun_spec = ident_path.clone().then(bindings.clone()).then(arg.clone());

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
            .then(
                code_expr
                    .clone()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
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
            .then(txt_arg)
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
            .then(subtree_body)
            .map_with_span(|(addr, body), span| {
                Located::new(
                    Node::Subtree { addr, body },
                    Some(make_span_from_range(span)),
                )
            });

        let generic_cmd = ident_path.map_with_span(|path, span| {
            Located::new(Node::Ident { path }, Some(make_span_from_range(span)))
        });

        choice((
            def_cmd,
            alloc_cmd,
            export_cmd,
            namespace_cmd,
            subtree_cmd,
            fun_cmd,
            let_cmd,
            scope_cmd,
            put_cmd,
            default_cmd,
            get_cmd,
            open_cmd,
            xml_ident,
            decl_xmlns_cmd,
            generic_cmd,
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

/// Helper to create span from range (placeholder)
fn make_span_from_range(range: std::ops::Range<usize>) -> Span {
    Span::new(
        Position::new(range.start, 1, range.start + 1),
        Position::new(range.end, 1, range.end + 1),
    )
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
        // Check that error reporting works (case-insensitive check)
        let report = errors[0].report("test.tree", input);
        assert!(report.to_lowercase().contains("error"));
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let input = "\\title}";
        let result = parse(input);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        let report = errors[0].report("test.tree", input);
        assert!(report.to_lowercase().contains("error"));
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
}
