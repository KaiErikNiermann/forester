// SPDX-FileCopyrightText: 2026 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

use forester_rust_parser::parse;
use std::hint::black_box;

fn usage() -> ! {
    eprintln!("Usage: forester-rust-parser-benchmark <tree-file>");
    std::process::exit(2);
}

fn main() {
    let mut args = std::env::args().skip(1);
    let path = match (args.next(), args.next()) {
        (Some(path), None) => path,
        _ => usage(),
    };

    let input = std::fs::read_to_string(&path).unwrap_or_else(|error| {
        eprintln!("Failed to read benchmark input {path}: {error}");
        std::process::exit(1);
    });

    match parse(&input) {
        Ok(document) => {
            black_box(document.nodes.len());
        }
        Err(errors) => {
            for error in errors {
                eprintln!("{error}");
            }
            std::process::exit(1);
        }
    }
}
