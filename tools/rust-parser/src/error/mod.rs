// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Error types for the Forester parser

mod details;
mod parse_error;

pub use details::{
    ExpectedTokens, ParseErrorDetails, ParseErrorKind, ParseErrorLabel, ParseErrorLabelKind,
};
pub use parse_error::{ParseError, ParseResult};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unexpected_token_reports_plain_text_hint() {
        let errors = crate::parse("hello").expect_err("plain text should fail");
        let details = errors[0].details();

        assert_eq!(details.kind, ParseErrorKind::UnexpectedToken);
        assert!(details
            .notes
            .iter()
            .any(|note| note.contains("plain text is only valid inside textual bodies")));
        assert!(!details
            .notes
            .iter()
            .any(|note| note.contains("braced argument or body")));
    }

    #[test]
    fn unexpected_eof_reports_braced_body_hint() {
        let errors =
            crate::parse("\\namespace\\foo").expect_err("namespace without body should fail");
        let details = errors[0].details();

        assert_eq!(details.kind, ParseErrorKind::UnexpectedEof);
        assert!(details
            .notes
            .iter()
            .any(|note| note.contains("braced argument or body")));
        assert!(details
            .notes
            .iter()
            .any(|note| note.contains("slash-separated identifier paths")));
    }

    #[test]
    fn custom_verbatim_errors_report_closing_hints() {
        let errors = crate::lexer::tokenize("\\startverb\nhello")
            .expect_err("unterminated verbatim should fail");
        let details = errors[0].details();

        assert_eq!(details.kind, ParseErrorKind::Custom);
        assert!(details
            .notes
            .iter()
            .any(|note| note.contains("inline verbatim closes")));
        assert!(details.notes.iter().any(|note| note.contains("\\stopverb")));
    }
}
