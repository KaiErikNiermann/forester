// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Forester Rust Parser - CLI tool for testing

use std::io::{self, Read};
use forester_rust_parser::parse;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.get(1).map(|s| s.as_str()) {
        Some("--help") | Some("-h") => {
            println!("Forester Rust Parser");
            println!();
            println!("Usage:");
            println!("  forester-rust-parser [file]     Parse a file");
            println!("  forester-rust-parser -          Read from stdin");
            println!("  forester-rust-parser --json     Output AST as JSON");
            println!("  forester-rust-parser --version  Show version");
            println!("  forester-rust-parser --help     Show this help");
        }
        Some("--version") | Some("-V") => {
            println!("forester-rust-parser {}", env!("CARGO_PKG_VERSION"));
        }
        Some("--json") => {
            // JSON mode: read from stdin or file
            let input = if let Some(path) = args.get(2) {
                if path == "-" {
                    let mut buf = String::new();
                    io::stdin().read_to_string(&mut buf).expect("Failed to read stdin");
                    buf
                } else {
                    std::fs::read_to_string(path).expect("Failed to read file")
                }
            } else {
                let mut buf = String::new();
                io::stdin().read_to_string(&mut buf).expect("Failed to read stdin");
                buf
            };

            match parse(&input) {
                Ok(doc) => {
                    let json = serde_json::to_string_pretty(&doc).unwrap();
                    println!("{}", json);
                }
                Err(errors) => {
                    eprintln!("Parse errors:");
                    for err in &errors {
                        eprintln!("{}", err.report("<stdin>", &input));
                    }
                    std::process::exit(1);
                }
            }
        }
        Some("-") => {
            // Read from stdin
            let mut input = String::new();
            io::stdin().read_to_string(&mut input).expect("Failed to read stdin");
            process_input(&input, "<stdin>");
        }
        Some(path) if !path.starts_with('-') => {
            // Parse file
            let input = std::fs::read_to_string(path).expect("Failed to read file");
            process_input(&input, path);
        }
        _ => {
            // Interactive mode or no args
            println!("Forester Rust Parser - Interactive Mode");
            println!("Enter Forester markup (Ctrl+D to finish):");
            println!();

            let mut input = String::new();
            io::stdin().read_to_string(&mut input).expect("Failed to read stdin");

            if !input.is_empty() {
                process_input(&input, "<stdin>");
            }
        }
    }
}

fn process_input(input: &str, filename: &str) {
    match parse(input) {
        Ok(doc) => {
            println!("✓ Parse successful!");
            println!();
            println!("AST ({} top-level nodes):", doc.nodes.len());
            println!("{:#?}", doc);
        }
        Err(errors) => {
            eprintln!("✗ Parse failed with {} error(s):", errors.len());
            eprintln!();
            for err in &errors {
                eprintln!("{}", err.report(filename, input));
            }
            std::process::exit(1);
        }
    }
}
