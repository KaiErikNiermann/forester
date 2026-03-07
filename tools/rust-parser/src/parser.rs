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

/// Document parser - parses a sequence of nodes
fn document_parser() -> impl Parser<Token, Nodes, Error = Simple<Token>> + Clone {
    choice((select! { Token::Whitespace(_) => None }, parser().map(Some)))
        .repeated()
        .then_ignore(end())
        .map(|nodes| nodes.into_iter().flatten().collect())
}

/// Single node parser
fn parser() -> impl Parser<Token, Located<Node>, Error = Simple<Token>> + Clone {
    recursive(|node| {
        let _nodes = node.clone().repeated();

        // Text nodes in textual positions.
        let text = select! {
            Token::Text(s) => Node::text(s),
            Token::Whitespace(s) => Node::text(s),
            Token::AtSign => Node::text("@"),
            Token::Tick => Node::text("'"),
            Token::Hash => Node::text("#"),
            Token::DxEntailed => Node::text("-:"),
            Token::DxVar(s) => Node::text(format!("?{s}")),
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        // Hash identifier (#name)
        let hash_ident = select! {
            Token::HashIdent(s) => Node::HashIdent { name: s },
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        let xml_ident = select! {
            Token::XmlIdent(prefix, name) => Node::XmlIdent { prefix, name },
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        // Verbatim body already resolved by the lexer.
        let verbatim = select! {
            Token::Verbatim(s) => Node::verbatim(s),
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        // Braced group {content}
        let braces = node
            .clone()
            .repeated()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::braces(body), Some(make_span_from_range(span)))
            });

        // Square bracketed group [content]
        let squares = node
            .clone()
            .repeated()
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map_with_span(|body, span| {
                Located::new(Node::squares(body), Some(make_span_from_range(span)))
            });

        // Parenthesized group (content)
        let parens = node
            .clone()
            .repeated()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with_span(|body, span| {
                Located::new(Node::parens(body), Some(make_span_from_range(span)))
            });

        // Inline math #{content}
        let inline_math = node
            .clone()
            .repeated()
            .delimited_by(just(Token::HashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::inline_math(body), Some(make_span_from_range(span)))
            });

        // Display math ##{content}
        let display_math = node
            .clone()
            .repeated()
            .delimited_by(just(Token::HashHashLBrace), just(Token::RBrace))
            .map_with_span(|body, span| {
                Located::new(Node::display_math(body), Some(make_span_from_range(span)))
            });

        // === Backslash commands ===

        // Parse identifier path after backslash: \foo or \foo/bar/baz
        let ident_path = select! { Token::Ident(s) => s }
            .separated_by(just(Token::Slash))
            .at_least(1);

        // Braced argument helper
        let braced_arg = node
            .clone()
            .repeated()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));

        let wstext = select! {
            Token::Text(s) => s,
            Token::Ident(s) => s,
            Token::Whitespace(s) => s,
        }
        .repeated()
        .map(|parts| parts.join(""));

        // Square bracketed binding: [name] or [~name]
        let binding = select! { Token::Ident(s) => s, Token::Text(s) => s }
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

        // \def\name[bindings]{body}
        let def_cmd = just(Token::KwDef)
            .ignore_then(ident_path.clone())
            .then(bindings.clone())
            .then(braced_arg.clone())
            .map_with_span(|((path, binds), body), span| {
                Located::new(
                    Node::Def {
                        path,
                        bindings: binds,
                        body,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \let\name[bindings]{body}
        let let_cmd = just(Token::KwLet)
            .ignore_then(ident_path.clone())
            .then(bindings.clone())
            .then(braced_arg.clone())
            .map_with_span(|((path, binds), body), span| {
                Located::new(
                    Node::Let {
                        path,
                        bindings: binds,
                        body,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \import{target}
        let import_cmd = just(Token::KwImport)
            .ignore_then(wstext.delimited_by(just(Token::LBrace), just(Token::RBrace)))
            .map_with_span(|parts, span| {
                Located::new(
                    Node::Import {
                        visibility: Visibility::Private,
                        target: parts,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \export{target}
        let export_cmd = just(Token::KwExport)
            .ignore_then(wstext.delimited_by(just(Token::LBrace), just(Token::RBrace)))
            .map_with_span(|parts, span| {
                Located::new(
                    Node::Import {
                        visibility: Visibility::Public,
                        target: parts,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        let decl_xmlns_cmd = select! { Token::DeclXmlns(prefix) => prefix }
            .then(wstext.delimited_by(just(Token::LBrace), just(Token::RBrace)))
            .map_with_span(|(prefix, uri), span| {
                Located::new(
                    Node::DeclXmlns { prefix, uri },
                    Some(make_span_from_range(span)),
                )
            });

        // \subtree[addr]{body} or \subtree{body}
        let subtree_cmd = just(Token::KwSubtree)
            .ignore_then(
                wstext
                    .delimited_by(just(Token::LSquare), just(Token::RSquare))
                    .or_not(),
            )
            .then(braced_arg.clone())
            .map_with_span(|(addr, body), span| {
                Located::new(
                    Node::Subtree { addr, body },
                    Some(make_span_from_range(span)),
                )
            });

        // \scope{body}
        let scope_cmd = just(Token::KwScope)
            .ignore_then(braced_arg.clone())
            .map_with_span(|body, span| {
                Located::new(Node::Scope { body }, Some(make_span_from_range(span)))
            });

        // Generic command identifier: \ident becomes IDENT(/IDENT)* at the
        // lexer level, so the parser starts directly from the identifier path.
        let generic_cmd = ident_path.clone().map_with_span(|path, span| {
            Located::new(Node::Ident { path }, Some(make_span_from_range(span)))
        });

        // Combine all parsers
        choice((
            // Specific commands first (more specific patterns)
            def_cmd,
            let_cmd,
            import_cmd,
            export_cmd,
            decl_xmlns_cmd,
            subtree_cmd,
            scope_cmd,
            generic_cmd,
            xml_ident,
            // Groups
            braces,
            squares,
            parens,
            inline_math,
            display_math,
            verbatim,
            // Simple tokens
            text,
            hash_ident,
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
    fn test_parse_def() {
        let result = parse("\\def\\myMacro[x]{Content with #x}");
        assert!(result.is_ok());
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
    fn test_plain_keywords_remain_text() {
        let result = parse("scope import def");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 3);
        assert!(matches!(&doc.nodes[0].value, Node::Text { content } if content == "scope"));
        assert!(matches!(&doc.nodes[1].value, Node::Text { content } if content == "import"));
        assert!(matches!(&doc.nodes[2].value, Node::Text { content } if content == "def"));
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
        let result = parse("?- ?foo -: # @ '");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 6);
        assert!(matches!(&doc.nodes[0].value, Node::Text { content } if content == "?-"));
        assert!(matches!(&doc.nodes[1].value, Node::Text { content } if content == "?foo"));
        assert!(matches!(&doc.nodes[2].value, Node::Text { content } if content == "-:"));
        assert!(matches!(&doc.nodes[3].value, Node::Text { content } if content == "#"));
        assert!(matches!(&doc.nodes[4].value, Node::Text { content } if content == "@"));
        assert!(matches!(&doc.nodes[5].value, Node::Text { content } if content == "'"));
    }

    #[test]
    fn test_bare_question_falls_back_to_text() {
        let result = parse("?");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert_eq!(doc.nodes.len(), 1);
        assert!(matches!(&doc.nodes[0].value, Node::Text { content } if content == "?"));
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
}
