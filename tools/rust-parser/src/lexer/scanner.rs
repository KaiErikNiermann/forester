// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Mode {
    Main,
    IdentInit { backslash_start: usize },
    IdentFragments,
    Verbatim { herald: String, token_start: usize },
}

pub(super) struct Scanner<'a> {
    pub(super) input: &'a str,
    pub(super) pos: usize,
}

impl<'a> Scanner<'a> {
    pub(super) fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    pub(super) fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    pub(super) fn rest(&self) -> &'a str {
        &self.input[self.pos..]
    }

    pub(super) fn starts_with(&self, needle: &str) -> bool {
        self.rest().starts_with(needle)
    }

    pub(super) fn peek_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    pub(super) fn advance_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    pub(super) fn advance_bytes(&mut self, len: usize) {
        self.pos += len;
    }

    pub(super) fn consume_while<F>(&mut self, predicate: F) -> Range<usize>
    where
        F: Fn(char) -> bool,
    {
        let start = self.pos;
        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.pos += ch.len_utf8();
        }
        start..self.pos
    }

    pub(super) fn take_char_span(&mut self) -> Option<(char, Range<usize>)> {
        let start = self.pos;
        let ch = self.advance_char()?;
        Some((ch, start..self.pos))
    }

    pub(super) fn slice(&self, range: Range<usize>) -> &'a str {
        &self.input[range]
    }
}
