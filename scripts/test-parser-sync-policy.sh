#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd -- "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
check_script="$repo_root/scripts/check-parser-sync.sh"

run_case() {
  local expected_status="$1"
  local name="$2"
  shift 2

  local files_list
  files_list="$(mktemp)"
  trap 'rm -f "$files_list"' RETURN
  printf '%s\n' "$@" > "$files_list"

  local output
  if output="$("$check_script" --files-list "$files_list" 2>&1)"; then
    local status=0
    if [[ "$expected_status" -ne 0 ]]; then
      printf 'parser-sync test %s unexpectedly succeeded\n%s\n' "$name" "$output" >&2
      exit 1
    fi
  else
    local status=$?
    if [[ "$expected_status" -eq 0 || "$status" -ne "$expected_status" ]]; then
      printf 'parser-sync test %s failed with status %d\n%s\n' "$name" "$status" "$output" >&2
      exit 1
    fi
  fi

  rm -f "$files_list"
  trap - RETURN
}

run_case 0 "no-parser-change" \
  "docs/rust-parser/source-of-truth.md"

run_case 1 "missing-rust-and-tests" \
  "lib/parser/Lexer.mll"

run_case 1 "missing-tests" \
  "lib/parser/Lexer.mll" \
  "tools/rust-parser/src/lexer.rs"

run_case 0 "complete-sync-change" \
  "lib/parser/Lexer.mll" \
  "tools/rust-parser/src/lexer.rs" \
  "tools/rust-parser/tests/fixture_corpus.rs"

printf 'parser-sync policy tests passed.\n'
