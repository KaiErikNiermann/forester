#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd cabal "Install cabal to build the Markdown converter for corpus validation."
require_cmd date "GNU date is required to timestamp the corpus report."
require_cmd opam "Install opam to build the OCaml parser validator for corpus validation."
require_cmd python3 "Install Python 3 so diagnostic JSON can be summarized."

manifest_path="$repo_root/tools/pandoc-converter/corpus/realworld-manifest.txt"
report_path=""
tmp_dir="$(mktemp -d)"

cleanup() {
  rm -rf "$tmp_dir"
}

trap cleanup EXIT

usage() {
  cat <<'USAGE'
Usage:
  check-markdown-realworld-corpus.sh [--report PATH]

Options:
  --report PATH  Write the validation report to PATH.
USAGE
}

parse_args() {
  while (($# > 0)); do
    case "$1" in
      --report)
        if (($# < 2)); then
          printf 'Missing value for --report.\n' >&2
          exit 1
        fi
        report_path="$2"
        shift 2
        ;;
      --help|-h)
        usage
        exit 0
        ;;
      *)
        printf 'Unknown option: %s\n\n' "$1" >&2
        usage >&2
        exit 1
        ;;
    esac
  done
}

load_corpus_entries() {
  while IFS= read -r entry; do
    if [[ ! -f "$repo_root/$entry" ]]; then
      printf 'Corpus manifest entry does not exist: %s\n' "$entry" >&2
      exit 1
    fi
    printf '%s\n' "$entry"
  done < <(read_manifest_entries "$manifest_path")
}

resolve_converter_path() {
  if [[ -n "${FORESTER_PANDOC_PATH:-}" && -x "${FORESTER_PANDOC_PATH}" ]]; then
    printf '%s\n' "$FORESTER_PANDOC_PATH"
    return 0
  fi

  (
    cd "$repo_root/tools/pandoc-converter"
    cabal --project-file=cabal.project build forester-pandoc:exe:forester-pandoc >/dev/null
    cabal --project-file=cabal.project list-bin forester-pandoc:exe:forester-pandoc
  )
}

resolve_ocaml_validator_path() {
  if [[ -n "${FORESTER_OCAML_VALIDATOR_PATH:-}" && -x "${FORESTER_OCAML_VALIDATOR_PATH}" ]]; then
    printf '%s\n' "$FORESTER_OCAML_VALIDATOR_PATH"
    return 0
  fi

  (
    cd "$repo_root"
    opam exec -- dune build lib/compiler/test/Validate_forester_cli.exe >/dev/null
  )
  printf '%s\n' "$repo_root/_build/default/lib/compiler/test/Validate_forester_cli.exe"
}

diagnostic_summary() {
  local diagnostics_path="$1"
  python3 "$repo_root/scripts/json_diagnostic_summary.py" "$diagnostics_path"
}

append_report() {
  printf '%s\n' "$1" >> "$report_path"
}

run_case() {
  local converter_path="$1"
  local ocaml_validator_path="$2"
  local relative_path="$3"
  local input_path="$repo_root/$relative_path"
  local stem
  stem="$(printf '%s' "$relative_path" | tr '/.' '__')"
  local converted_path="$tmp_dir/${stem}.forester"
  local diagnostics_path="$tmp_dir/${stem}.diagnostics.json"
  local validator_output_path="$tmp_dir/${stem}.validator.txt"
  local input_bytes output_bytes diagnostics_count diagnostics_summary

  input_bytes="$(wc -c < "$input_path" | tr -d '[:space:]')"

  if ! "$converter_path" \
    markdown-to-forester \
    --diagnostics-format json \
    "$input_path" \
    >"$converted_path" \
    2>"$diagnostics_path"; then
    append_report "## $relative_path"
    append_report ""
    append_report "- status: converter-failed"
    append_report "- input_bytes: $input_bytes"
    append_report "- diagnostics_json:"
    append_report '```json'
    if [[ -s "$diagnostics_path" ]]; then
      cat "$diagnostics_path" >> "$report_path"
    else
      printf '[]\n' >> "$report_path"
    fi
    append_report '```'
    append_report ""
    return 1
  fi

  if [[ ! -s "$converted_path" ]]; then
    append_report "## $relative_path"
    append_report ""
    append_report "- status: empty-output"
    append_report "- input_bytes: $input_bytes"
    append_report ""
    return 1
  fi

  if ! "$ocaml_validator_path" "$converted_path" > /dev/null 2>"$validator_output_path"; then
    append_report "## $relative_path"
    append_report ""
    append_report "- status: ocaml-parser-invalid"
    append_report "- input_bytes: $input_bytes"
    append_report "- output_bytes: $(wc -c < "$converted_path" | tr -d '[:space:]')"
    append_report "- diagnostics_json:"
    append_report '```json'
    if [[ -s "$diagnostics_path" ]]; then
      cat "$diagnostics_path" >> "$report_path"
    else
      printf '[]\n' >> "$report_path"
    fi
    append_report '```'
    append_report "- ocaml_validator_output:"
    append_report '```text'
    cat "$validator_output_path" >> "$report_path"
    append_report '```'
    append_report ""
    return 1
  fi

  output_bytes="$(wc -c < "$converted_path" | tr -d '[:space:]')"
  IFS=$'\t' read -r diagnostics_count diagnostics_summary < <(diagnostic_summary "$diagnostics_path")

  append_report "## $relative_path"
  append_report ""
  append_report "- status: ok"
  append_report "- input_bytes: $input_bytes"
  append_report "- output_bytes: $output_bytes"
  append_report "- diagnostics_count: $diagnostics_count"
  append_report "- diagnostic_codes: $diagnostics_summary"
  append_report ""

  printf '%s\t%s\t%s\t%s\n' "$relative_path" "$input_bytes" "$output_bytes" "$diagnostics_count"
}

main() {
  parse_args "$@"

  if [[ ! -f "$manifest_path" ]]; then
    printf 'Missing corpus manifest: %s\n' "$manifest_path" >&2
    exit 1
  fi

  if [[ -z "$report_path" ]]; then
    report_path="$tmp_dir/markdown-realworld-corpus-report.md"
  fi

  ensure_parent_dir "$report_path"
  : > "$report_path"

  local converter_path ocaml_validator_path
  local timestamp total_inputs total_input_bytes total_output_bytes total_diagnostics files_with_diagnostics failures
  local rows=()

  converter_path="$(resolve_converter_path)"
  ocaml_validator_path="$(resolve_ocaml_validator_path)"
  if [[ ! -x "$ocaml_validator_path" ]]; then
    printf 'OCaml validator binary is not executable: %s\n' "$ocaml_validator_path" >&2
    exit 1
  fi
  timestamp="$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  total_inputs=0
  total_input_bytes=0
  total_output_bytes=0
  total_diagnostics=0
  files_with_diagnostics=0
  failures=0

  append_report "# Markdown Real-World Corpus Report"
  append_report ""
  append_report "- generated_at_utc: $timestamp"
  append_report "- converter: $converter_path"
  append_report "- ocaml_validator: $ocaml_validator_path"
  append_report "- manifest: ${manifest_path#$repo_root/}"
  append_report ""

  while IFS= read -r relative_path; do
    local row input_bytes output_bytes diagnostics_count
    if row="$(run_case "$converter_path" "$ocaml_validator_path" "$relative_path")"; then
      IFS=$'\t' read -r _ input_bytes output_bytes diagnostics_count <<< "$row"
      rows+=("$row")
      total_input_bytes=$((total_input_bytes + input_bytes))
      total_output_bytes=$((total_output_bytes + output_bytes))
      total_diagnostics=$((total_diagnostics + diagnostics_count))
      if ((diagnostics_count > 0)); then
        files_with_diagnostics=$((files_with_diagnostics + 1))
      fi
    else
      failures=$((failures + 1))
    fi
    total_inputs=$((total_inputs + 1))
  done < <(load_corpus_entries)

  append_report "## Summary"
  append_report ""
  append_report "| metric | value |"
  append_report "| --- | ---: |"
  append_report "| files_checked | $total_inputs |"
  append_report "| files_failed | $failures |"
  append_report "| total_input_bytes | $total_input_bytes |"
  append_report "| total_output_bytes | $total_output_bytes |"
  append_report "| total_diagnostics | $total_diagnostics |"
  append_report "| files_with_diagnostics | $files_with_diagnostics |"
  append_report ""

  if ((${#rows[@]} > 0)); then
    append_report "## Passing Files"
    append_report ""
    append_report "| path | input_bytes | output_bytes | diagnostics_count |"
    append_report "| --- | ---: | ---: | ---: |"
    local row path input_bytes output_bytes diagnostics_count
    for row in "${rows[@]}"; do
      IFS=$'\t' read -r path input_bytes output_bytes diagnostics_count <<< "$row"
      append_report "| $path | $input_bytes | $output_bytes | $diagnostics_count |"
    done
    append_report ""
  fi

  cat "$report_path"

  if ((failures > 0)); then
    printf 'Markdown real-world corpus validation found %d failing file(s).\n' "$failures" >&2
    exit 1
  fi
}

main "$@"
