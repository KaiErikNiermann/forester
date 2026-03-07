#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

resolve_rust_parser_path() {
  if [[ -n "${FORESTER_RUST_PARSER_PATH:-}" ]]; then
    printf '%s\n' "$FORESTER_RUST_PARSER_PATH"
    return
  fi

  local debug_bin="$repo_root/tools/rust-parser/target/debug/forester-rust-parser"
  local release_bin="$repo_root/tools/rust-parser/target/release/forester-rust-parser"

  if [[ -x "$debug_bin" ]]; then
    printf '%s\n' "$debug_bin"
    return
  fi

  if [[ -x "$release_bin" ]]; then
    printf '%s\n' "$release_bin"
    return
  fi

  cargo build --manifest-path "$repo_root/tools/rust-parser/Cargo.toml"
  printf '%s\n' "$debug_bin"
}

rust_parser_path="$(resolve_rust_parser_path)"

if [[ ! -x "$rust_parser_path" ]]; then
  printf 'Rust parser binary is not executable: %s\n' "$rust_parser_path" >&2
  exit 1
fi

export FORESTER_RUST_PARSER_PATH="$rust_parser_path"
export FORESTER_RUST_PARSER_REQUIRE_BINARY="${FORESTER_RUST_PARSER_REQUIRE_BINARY:-1}"

cd "$repo_root"
opam exec -- dune runtest "$@"
