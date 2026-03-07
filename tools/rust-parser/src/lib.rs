// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

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
pub use parser::parse;
