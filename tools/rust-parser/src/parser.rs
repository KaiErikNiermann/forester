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
    let spanned_tokens = tokenize(input);

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
    parser().repeated().then_ignore(end())
}

/// Single node parser
fn parser() -> impl Parser<Token, Located<Node>, Error = Simple<Token>> + Clone {
    recursive(|node| {
        let _nodes = node.clone().repeated();

        // Text node - both Text and Ident tokens can be plain text
        let text = select! {
            Token::Text(s) => Node::text(s),
            Token::Ident(s) => Node::text(s),
            Token::Slash => Node::text("/"),
            Token::Colon => Node::text(":"),
            Token::AtSign => Node::text("@"),
            Token::Tick => Node::text("'"),
            Token::Tilde => Node::text("~"),
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        // Comment node
        let comment = select! {
            Token::Comment(s) => Node::Comment { content: s },
        }
        .map_with_span(|n, span| Located::new(n, Some(make_span_from_range(span))));

        // Hash identifier (#name)
        let hash_ident = select! {
            Token::HashIdent(s) => Node::HashIdent { name: s },
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

        // Square bracketed binding: [name] or [~name]
        let binding = just(Token::Tilde)
            .or_not()
            .then(select! { Token::Ident(s) => s, Token::Text(s) => s })
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map(|(tilde, name)| {
                let info = if tilde.is_some() {
                    BindingInfo::Lazy
                } else {
                    BindingInfo::Strict
                };
                (info, name)
            });

        let bindings = binding.repeated();

        // \title{content} - basic command test
        // Returns the identifier node; argument is consumed but returned as part of command
        let title_cmd = just(Token::Backslash)
            .ignore_then(just(Token::Ident("title".to_string())))
            .ignore_then(braced_arg.clone())
            .map_with_span(|body, span: std::ops::Range<usize>| {
                // Create a group containing the title identifier and its braced argument
                Located::new(
                    Node::Group {
                        delim: Delim::Braces,
                        body: vec![
                            Located::new(
                                Node::Ident {
                                    path: vec!["title".to_string()],
                                },
                                Some(make_span_from_range(span.clone())),
                            ),
                            Located::new(
                                Node::braces(body),
                                Some(make_span_from_range(span.clone())),
                            ),
                        ],
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \p{content} - paragraph command test
        let p_cmd = just(Token::Backslash)
            .ignore_then(just(Token::Ident("p".to_string())))
            .ignore_then(braced_arg.clone())
            .map_with_span(|body, span: std::ops::Range<usize>| {
                // Create a group containing the p identifier and its braced argument
                Located::new(
                    Node::Group {
                        delim: Delim::Braces,
                        body: vec![
                            Located::new(
                                Node::Ident {
                                    path: vec!["p".to_string()],
                                },
                                Some(make_span_from_range(span.clone())),
                            ),
                            Located::new(
                                Node::braces(body),
                                Some(make_span_from_range(span.clone())),
                            ),
                        ],
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \def\name[bindings]{body}
        let def_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwDef))
            .ignore_then(just(Token::Backslash))
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
        let let_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwLet))
            .ignore_then(just(Token::Backslash))
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
        let import_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwImport))
            .ignore_then(
                select! { Token::Text(s) => s, Token::Ident(s) => s }
                    .repeated()
                    .at_least(1)
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with_span(|parts, span| {
                let target = parts.join("");
                Located::new(
                    Node::Import {
                        visibility: Visibility::Private,
                        target,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \export{target}
        let export_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwExport))
            .ignore_then(
                select! { Token::Text(s) => s, Token::Ident(s) => s }
                    .repeated()
                    .at_least(1)
                    .delimited_by(just(Token::LBrace), just(Token::RBrace)),
            )
            .map_with_span(|parts, span| {
                let target = parts.join("");
                Located::new(
                    Node::Import {
                        visibility: Visibility::Public,
                        target,
                    },
                    Some(make_span_from_range(span)),
                )
            });

        // \subtree[addr]{body} or \subtree{body}
        let subtree_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwSubtree))
            .ignore_then(
                select! { Token::Ident(s) => s, Token::Text(s) => s }
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
        let scope_cmd = just(Token::Backslash)
            .ignore_then(just(Token::KwScope))
            .ignore_then(braced_arg.clone())
            .map_with_span(|body, span| {
                Located::new(Node::Scope { body }, Some(make_span_from_range(span)))
            });

        // Generic backslash command: \ident
        let generic_cmd = just(Token::Backslash)
            .ignore_then(ident_path.clone())
            .map_with_span(|path, span| {
                Located::new(Node::Ident { path }, Some(make_span_from_range(span)))
            });

        // Escaped characters: \{, \}, etc.
        let escaped = just(Token::Backslash)
            .ignore_then(select! {
                Token::LBrace => "{".to_string(),
                Token::RBrace => "}".to_string(),
                Token::LSquare => "[".to_string(),
                Token::RSquare => "]".to_string(),
                Token::Hash => "#".to_string(),
            })
            .or(select! {
                Token::EscapedText(s) => s,
            })
            .map_with_span(|s, span| Located::new(Node::text(s), Some(make_span_from_range(span))));

        // Combine all parsers
        choice((
            // Specific commands first (more specific patterns)
            def_cmd,
            let_cmd,
            import_cmd,
            export_cmd,
            subtree_cmd,
            scope_cmd,
            // Then try title and p as special cases
            title_cmd,
            p_cmd,
            escaped,
            generic_cmd,
            // Groups
            braces,
            squares,
            parens,
            inline_math,
            display_math,
            // Simple tokens
            text,
            comment,
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
        assert!(!doc.nodes.is_empty());
    }

    #[test]
    fn test_parse_p() {
        let result = parse("\\p{This is a paragraph.}");
        assert!(result.is_ok());
        let doc = result.unwrap();
        assert!(!doc.nodes.is_empty());
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
    }

    #[test]
    fn test_parse_import() {
        let result = parse("\\import{foundation}");
        assert!(result.is_ok());
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
}
