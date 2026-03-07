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
    assert!(matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()]));
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
    assert!(matches!(&doc.nodes[1].value, Node::Ident { path } if path == &vec!["p".to_string()]));
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
    assert!(matches!(&doc.nodes[2].value, Node::Ident { path } if path == &vec!["p".to_string()]));
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
fn test_parse_bare_backslash_as_empty_ident() {
    let result = parse("\\");
    assert!(result.is_ok());
    let doc = result.unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert!(matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec![String::new()]));
}

#[test]
fn test_parse_get_with_trailing_empty_ident_fragment() {
    let result = parse("\\get\\a/");
    assert!(result.is_ok());
    let doc = result.unwrap();
    assert_eq!(doc.nodes.len(), 1);
    assert!(
        matches!(&doc.nodes[0].value, Node::Get { path } if path == &vec!["a".to_string(), String::new()])
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
    let result = parse(" \n\\title{Hello}\n");
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
fn test_top_level_tab_is_rejected() {
    let result = parse("\t");
    assert!(result.is_err());
}

#[test]
fn test_textual_whitespace_is_preserved() {
    let result = parse("\\p{alpha  \n\tbeta}");
    assert!(result.is_ok());
    let doc = result.unwrap();
    assert!(matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()]));
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
    assert!(matches!(&doc.nodes[0].value, Node::Ident { path } if path == &vec!["p".to_string()]));
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
    let result = parse("\\datalog{\\rel/links-to ?X @{https://example.com} -: {\\rel/is-node ?X}}");
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
