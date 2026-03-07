// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::error::{ExpectedTokens, ParseError, ParseErrorKind};

fn sample_source() -> &'static str {
    "alpha\nbeta\ngamma\n"
}

#[test]
fn parse_error_details_cover_every_variant() {
    let cases = vec![
        (
            ParseError::UnexpectedToken {
                expected: ExpectedTokens::new(vec!["'import'".to_string(), "'export'".to_string()]),
                found: "'text:alpha'".to_string(),
                span: 0..5,
            },
            ParseErrorKind::UnexpectedToken,
            Some("'text:alpha'"),
            Some("plain text is only valid inside textual bodies"),
            "Unexpected token",
        ),
        (
            ParseError::UnexpectedEof {
                expected: ExpectedTokens::new(vec!["'{'".to_string(), "'/'".to_string()]),
                span: 17..17,
            },
            ParseErrorKind::UnexpectedEof,
            None,
            Some("slash-separated identifier paths may continue with '/'"),
            "Unexpected end of input",
        ),
        (
            ParseError::UnclosedDelimiter {
                delim: "(".to_string(),
                expected_close: ")".to_string(),
                open_span: 0..1,
                eof_span: 17..17,
            },
            ParseErrorKind::UnclosedDelimiter,
            None,
            Some("close ( with )"),
            "Unclosed delimiter",
        ),
        (
            ParseError::MismatchedDelimiter {
                open_delim: "(".to_string(),
                expected_close: ")".to_string(),
                found_close: "]".to_string(),
                open_span: 0..1,
                found_span: 5..6,
            },
            ParseErrorKind::MismatchedDelimiter,
            Some("]"),
            Some("replace ] with )"),
            "Mismatched delimiter",
        ),
        (
            ParseError::UnexpectedClosingDelimiter {
                found_close: "]".to_string(),
                span: 5..6,
            },
            ParseErrorKind::UnexpectedClosingDelimiter,
            Some("]"),
            Some("remove the closing delimiter"),
            "Unexpected closing delimiter",
        ),
        (
            ParseError::InvalidEscape {
                char: '!',
                span: 6..8,
            },
            ParseErrorKind::InvalidEscape,
            Some("\\!"),
            Some("valid escaped names after a backslash"),
            "Invalid escape sequence",
        ),
        (
            ParseError::LexerError {
                position: 6,
                span: 6..7,
            },
            ParseErrorKind::LexerError,
            None,
            Some("inspect surrounding escapes, verbatim heralds, or XML command syntax"),
            "Lexer error",
        ),
        (
            ParseError::Custom {
                message: "unterminated verbatim".to_string(),
                span: 6..12,
            },
            ParseErrorKind::Custom,
            None,
            Some("block verbatim closes only at \\stopverb"),
            "unterminated verbatim",
        ),
    ];

    for (error, expected_kind, expected_found, expected_note, expected_display_fragment) in cases {
        let details = error.details();
        let display = error.to_string();

        assert_eq!(details.kind, expected_kind);
        assert_eq!(details.found.as_deref(), expected_found);
        assert!(
            !details.labels.is_empty(),
            "{} should include at least one label",
            display
        );
        assert!(
            error.span().start <= error.span().end,
            "span should be ordered for {display}"
        );
        assert!(
            display.contains(expected_display_fragment),
            "display string {:?} should contain {:?}",
            display,
            expected_display_fragment
        );

        if let Some(note) = expected_note {
            assert!(
                details
                    .notes
                    .iter()
                    .any(|candidate| candidate.contains(note)),
                "{} should mention {:?}, got {:?}",
                display,
                note,
                details.notes
            );
        }

        let report = error.report("error-variant-matrix.tree", sample_source());
        assert!(
            report.contains("Error:"),
            "report should include header: {report}"
        );
        assert!(
            report.contains(expected_display_fragment),
            "report should include {:?}: {report}",
            expected_display_fragment
        );
    }
}
