#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd -- "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  ./scripts/check-parser-sync.sh [base_ref [head_ref]]
  ./scripts/check-parser-sync.sh --files-list <path>

Policy:
  If OCaml parser grammar/token sources change, the same change set must also
  include:
    1. Rust parser implementation updates under tools/rust-parser/src/
    2. Parser parity coverage updates under lib/parser/test/ or
       tools/rust-parser/tests/
EOF
}

is_valid_commit() {
  local commitish="$1"
  [[ -n "$commitish" ]] && [[ "$commitish" != 0000000000000000000000000000000000000000 ]] \
    && git -C "$repo_root" rev-parse --verify "${commitish}^{commit}" >/dev/null 2>&1
}

resolve_base_ref() {
  if [[ $# -ge 1 ]] && [[ -n "$1" ]]; then
    printf '%s\n' "$1"
    return
  fi

  if is_valid_commit "${FORESTER_PARSER_SYNC_BASE_REF:-}"; then
    printf '%s\n' "${FORESTER_PARSER_SYNC_BASE_REF}"
    return
  fi

  if is_valid_commit "${GITHUB_EVENT_BEFORE:-}"; then
    printf '%s\n' "${GITHUB_EVENT_BEFORE}"
    return
  fi

  local default_branch="${FORESTER_DEFAULT_BRANCH:-main}"
  if git -C "$repo_root" rev-parse --verify "origin/$default_branch" >/dev/null 2>&1; then
    git -C "$repo_root" merge-base HEAD "origin/$default_branch"
    return
  fi

  if git -C "$repo_root" rev-parse --verify HEAD^ >/dev/null 2>&1; then
    git -C "$repo_root" rev-parse HEAD^
    return
  fi

  printf 'Unable to determine a parser-sync base ref. Pass one explicitly.\n' >&2
  exit 2
}

resolve_head_ref() {
  if [[ $# -ge 2 ]] && [[ -n "$2" ]]; then
    printf '%s\n' "$2"
  elif [[ -n "${FORESTER_PARSER_SYNC_HEAD_REF:-}" ]]; then
    printf '%s\n' "${FORESTER_PARSER_SYNC_HEAD_REF}"
  else
    printf 'HEAD\n'
  fi
}

collect_changed_files() {
  if [[ $# -ge 1 ]] && [[ "$1" == "--files-list" ]]; then
    if [[ $# -ne 2 ]]; then
      usage >&2
      exit 2
    fi
    cat -- "$2"
    return
  fi

  if [[ $# -gt 2 ]]; then
    usage >&2
    exit 2
  fi

  local base_ref
  local head_ref
  base_ref="$(resolve_base_ref "$@")"
  head_ref="$(resolve_head_ref "$@")"

  git -C "$repo_root" diff --name-only --diff-filter=ACMRTUXB "$base_ref" "$head_ref"
}

is_ocaml_parser_source() {
  case "$1" in
    lib/parser/Lexer.mll|lib/parser/Grammar.mly|lib/parser/Parse.ml|lib/parser/Parse.mli)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

is_rust_parser_impl() {
  case "$1" in
    tools/rust-parser/src/*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

is_parity_coverage() {
  case "$1" in
    lib/parser/test/*|tools/rust-parser/tests/*|tools/rust-parser/proptest-regressions/*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

is_parser_docs() {
  case "$1" in
    docs/rust-parser/*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

print_section() {
  local title="$1"
  shift
  printf '%s\n' "$title"
  if [[ $# -eq 0 ]]; then
    printf '  (none)\n'
    return
  fi

  local path
  for path in "$@"; do
    printf '  - %s\n' "$path"
  done
}

main() {
  mapfile -t changed_files < <(collect_changed_files "$@")

  local ocaml_parser_changes=()
  local rust_impl_changes=()
  local parity_coverage_changes=()
  local parser_doc_changes=()

  local path
  for path in "${changed_files[@]}"; do
    [[ -n "$path" ]] || continue
    if is_ocaml_parser_source "$path"; then
      ocaml_parser_changes+=("$path")
    fi
    if is_rust_parser_impl "$path"; then
      rust_impl_changes+=("$path")
    fi
    if is_parity_coverage "$path"; then
      parity_coverage_changes+=("$path")
    fi
    if is_parser_docs "$path"; then
      parser_doc_changes+=("$path")
    fi
  done

  if [[ ${#ocaml_parser_changes[@]} -eq 0 ]]; then
    printf 'parser-sync: no OCaml parser grammar/token sources changed; skipping policy check.\n'
    exit 0
  fi

  local failure=0
  if [[ ${#rust_impl_changes[@]} -eq 0 ]]; then
    printf 'parser-sync: missing Rust parser implementation updates for OCaml parser changes.\n' >&2
    failure=1
  fi
  if [[ ${#parity_coverage_changes[@]} -eq 0 ]]; then
    printf 'parser-sync: missing parser parity coverage updates for OCaml parser changes.\n' >&2
    failure=1
  fi

  if [[ $failure -ne 0 ]]; then
    print_section "OCaml parser changes:" "${ocaml_parser_changes[@]}" >&2
    print_section "Rust parser implementation changes:" "${rust_impl_changes[@]}" >&2
    print_section "Parity coverage changes:" "${parity_coverage_changes[@]}" >&2
    print_section "Parser docs changes (recommended):" "${parser_doc_changes[@]}" >&2
    printf '%s\n' \
      "Update the same change set with matching files under tools/rust-parser/src/ and parser parity coverage under lib/parser/test/ or tools/rust-parser/tests/." >&2
    exit 1
  fi

  print_section "parser-sync: OCaml parser changes:" "${ocaml_parser_changes[@]}"
  print_section "parser-sync: Rust parser implementation changes:" "${rust_impl_changes[@]}"
  print_section "parser-sync: Parity coverage changes:" "${parity_coverage_changes[@]}"
  if [[ ${#parser_doc_changes[@]} -eq 0 ]]; then
    printf 'parser-sync: no docs/rust-parser/ updates detected; this is allowed but review whether source-of-truth docs should change.\n'
  else
    print_section "parser-sync: Parser docs changes:" "${parser_doc_changes[@]}"
  fi
}

main "$@"
