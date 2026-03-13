// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

// chumsky's `select!` macro generates closures that return large `Result::Err`
// variants (`Simple<Token>` ≈ 184 bytes). This is inherent to the library's
// error type and not something we can shrink at the call site.
#![allow(clippy::result_large_err)]

//! Forester Rust Parser - Alternative parser implementation
//!
//! This crate provides a Rust-based parser for the Forester markup language,
//! designed to integrate with the OCaml frontend via FFI (JSON serialization).

pub mod ast;
pub mod error;
pub mod ffi;
pub mod json;
pub mod lexer;
pub mod parser;

pub use ast::*;
pub use error::ParseError;
pub use lexer::Token;
pub use parser::{parse, parse_recovery, parse_with_mode, ParseMode, RecoveryResult};
