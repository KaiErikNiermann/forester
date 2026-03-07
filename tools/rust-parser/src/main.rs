// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Forester Rust Parser - CLI tool for testing

use forester_rust_parser::json::{schema_pretty_string, ParseResult};
use forester_rust_parser::{parse_with_mode, ParseMode, RecoveryResult};
use std::io::{self, Read};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OutputFormat {
    Human,
    Json,
    JsonSchema,
    Version,
    Help,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CliOptions {
    mode: ParseMode,
    format: OutputFormat,
    input_path: Option<String>,
}

fn parse_cli_args(args: &[String]) -> Result<CliOptions, String> {
    let mut options = CliOptions {
        mode: ParseMode::Strict,
        format: OutputFormat::Human,
        input_path: None,
    };

    for arg in args.iter().skip(1) {
        match arg.as_str() {
            "--help" | "-h" => options.format = OutputFormat::Help,
            "--version" | "-V" => options.format = OutputFormat::Version,
            "--json-schema" => options.format = OutputFormat::JsonSchema,
            "--json" => options.format = OutputFormat::Json,
            "--strict" => options.mode = ParseMode::Strict,
            "--recovery" => options.mode = ParseMode::Recovery,
            _ if arg.starts_with('-') && arg != "-" => {
                return Err(format!("Unknown flag: {arg}"));
            }
            _ => {
                if options.input_path.is_some() {
                    return Err("Only one input path is supported".to_string());
                }
                options.input_path = Some(arg.clone());
            }
        }
    }

    Ok(options)
}

fn print_help() {
    println!("Forester Rust Parser");
    println!();
    println!("Usage:");
    println!("  forester-rust-parser [--strict|--recovery] [file]");
    println!("  forester-rust-parser [--strict|--recovery] -");
    println!("  forester-rust-parser --json [--strict|--recovery] [file|-]");
    println!("  forester-rust-parser --json-schema");
    println!("  forester-rust-parser --version");
    println!("  forester-rust-parser --help");
}

fn read_input(input_path: Option<&str>) -> (String, String) {
    match input_path {
        Some("-") | None => {
            let mut buf = String::new();
            io::stdin()
                .read_to_string(&mut buf)
                .expect("Failed to read stdin");
            (buf, "<stdin>".to_string())
        }
        Some(path) => (
            std::fs::read_to_string(path).expect("Failed to read file"),
            path.to_string(),
        ),
    }
}

fn json_result(input: &str, filename: &str, mode: ParseMode) -> ParseResult {
    ParseResult::from_recovery_result(parse_with_mode(input, mode), mode, filename, input)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let options = match parse_cli_args(&args) {
        Ok(options) => options,
        Err(message) => {
            eprintln!("{message}");
            std::process::exit(2);
        }
    };

    match options.format {
        OutputFormat::Help => print_help(),
        OutputFormat::Version => {
            println!("forester-rust-parser {}", env!("CARGO_PKG_VERSION"));
        }
        OutputFormat::JsonSchema => {
            print!("{}", schema_pretty_string());
        }
        OutputFormat::Json => {
            let (input, filename) = read_input(options.input_path.as_deref());
            let result = json_result(&input, &filename, options.mode);
            let json = serde_json::to_string_pretty(&result).unwrap();
            println!("{json}");
        }
        OutputFormat::Human => {
            if options.input_path.is_none() {
                println!("Forester Rust Parser - Interactive Mode");
                println!("Enter Forester markup (Ctrl+D to finish):");
                println!();
            }

            let (input, filename) = read_input(options.input_path.as_deref());
            if input.is_empty() {
                return;
            }
            let exit_code = process_input(&input, &filename, options.mode);
            if exit_code != 0 {
                std::process::exit(exit_code);
            }
        }
    }
}

fn process_input(input: &str, filename: &str, mode: ParseMode) -> i32 {
    match parse_with_mode(input, mode) {
        RecoveryResult {
            output: Some(doc),
            errors,
        } if errors.is_empty() => {
            println!("✓ Parse successful!");
            println!();
            println!("AST ({} top-level nodes):", doc.nodes.len());
            println!("{:#?}", doc);
            0
        }
        RecoveryResult {
            output: Some(doc),
            errors,
        } => {
            eprintln!(
                "✗ Parse recovered with {} error(s) in {} mode:",
                errors.len(),
                mode.as_str()
            );
            eprintln!();
            for err in &errors {
                eprintln!("{}", err.report(filename, input));
            }
            eprintln!();
            eprintln!("Recovered AST ({} top-level nodes):", doc.nodes.len());
            eprintln!("{:#?}", doc);
            1
        }
        RecoveryResult {
            output: None,
            errors,
        } => {
            eprintln!("✗ Parse failed with {} error(s):", errors.len());
            eprintln!();
            for err in &errors {
                eprintln!("{}", err.report(filename, input));
            }
            1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(values: &[&str]) -> Vec<String> {
        values.iter().map(|value| value.to_string()).collect()
    }

    #[test]
    fn parse_cli_args_defaults_to_strict_human_mode() {
        let options =
            parse_cli_args(&args(&["forester-rust-parser", "input.tree"])).expect("parse args");

        assert_eq!(options.mode, ParseMode::Strict);
        assert_eq!(options.format, OutputFormat::Human);
        assert_eq!(options.input_path.as_deref(), Some("input.tree"));
    }

    #[test]
    fn parse_cli_args_accepts_recovery_json_mode() {
        let options = parse_cli_args(&args(&[
            "forester-rust-parser",
            "--json",
            "--recovery",
            "-",
        ]))
        .expect("parse args");

        assert_eq!(options.mode, ParseMode::Recovery);
        assert_eq!(options.format, OutputFormat::Json);
        assert_eq!(options.input_path.as_deref(), Some("-"));
    }

    #[test]
    fn parse_cli_args_rejects_unknown_flags() {
        let error = parse_cli_args(&args(&["forester-rust-parser", "--bogus"]))
            .expect_err("unknown flag should fail");

        assert!(error.contains("--bogus"));
    }
}
