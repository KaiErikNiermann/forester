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
fn test_bare_backslash_yields_empty_ident() {
    let tokens = tokenize("\\").unwrap();
    assert_eq!(
        tokens
            .iter()
            .map(|token| token.token.clone())
            .collect::<Vec<_>>(),
        vec![Token::Ident(String::new())]
    );
}

#[test]
fn test_ident_fragments_allow_empty_trailing_segment() {
    let tokens = tokenize("\\alpha/").unwrap();
    assert_eq!(
        tokens
            .iter()
            .map(|token| token.token.clone())
            .collect::<Vec<_>>(),
        vec![
            Token::Ident("alpha".to_string()),
            Token::Slash,
            Token::Ident(String::new()),
        ]
    );
}

#[test]
fn test_ident_fragments_allow_empty_interior_segment() {
    let tokens = tokenize("\\alpha//beta").unwrap();
    assert_eq!(
        tokens
            .iter()
            .map(|token| token.token.clone())
            .collect::<Vec<_>>(),
        vec![
            Token::Ident("alpha".to_string()),
            Token::Slash,
            Token::Ident(String::new()),
            Token::Slash,
            Token::Ident("beta".to_string()),
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
    let tokens = tokenize("\\% \\\\ \\, \\\" \\` \\_ \\; \\# \\{ \\} \\[ \\] \\| \\ ").unwrap();
    assert_eq!(
        tokens
            .iter()
            .map(|token| token.token.clone())
            .collect::<Vec<_>>(),
        vec![
            Token::Text("%".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("\\".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident(",".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("\"".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("`".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("_".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident(";".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("#".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("{".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("}".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("[".to_string()),
            Token::Whitespace(" ".to_string()),
            Token::Ident("]".to_string()),
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
fn test_inline_verbatim_trims_trailing_newlines_and_whitespace() {
    let tokens = tokenize("\\verbEND|line one\nline two \n\nEND").unwrap();
    assert_eq!(
        tokens,
        vec![SpannedToken {
            token: Token::Verbatim("line one\nline two".to_string()),
            span: 0..32,
        }]
    );
}

#[test]
fn test_inline_verbatim_waits_for_full_herald_match() {
    let tokens = tokenize("\\verbEND|bodyEN bodyEND").unwrap();
    assert_eq!(
        tokens,
        vec![SpannedToken {
            token: Token::Verbatim("bodyEN body".to_string()),
            span: 0..23,
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
fn test_ident_init_newline_error_uses_offending_lexeme() {
    let errors = tokenize("\\\n").unwrap_err();
    assert_eq!(errors[0].to_string(), "syntax error, unexpected \"\\n\"");
    assert_eq!(errors[0].span(), 1..2);
}

#[test]
fn test_ident_init_invalid_char_error_uses_offending_lexeme() {
    let errors = tokenize("\\!").unwrap_err();
    assert_eq!(errors[0].to_string(), "syntax error, unexpected \"!\"");
    assert_eq!(errors[0].span(), 1..2);
}

#[test]
fn test_ident_fragments_allow_empty_segment_before_space() {
    let tokens = tokenize("\\foo/ ").unwrap();
    assert_eq!(
        tokens
            .iter()
            .map(|token| token.token.clone())
            .collect::<Vec<_>>(),
        vec![
            Token::Ident("foo".to_string()),
            Token::Slash,
            Token::Ident(String::new()),
            Token::Whitespace(" ".to_string()),
        ]
    );
}

#[test]
fn test_ident_fragment_newline_error_uses_offending_lexeme() {
    let errors = tokenize("\\foo/\n").unwrap_err();
    assert_eq!(errors[0].to_string(), "syntax error, unexpected \"\\n\"");
    assert_eq!(errors[0].span(), 5..6);
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
