#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd cabal "Install cabal to benchmark the Markdown converter."
require_cmd date "GNU date is required to capture benchmark timing."

fixtures_root="$repo_root/tools/pandoc-converter/fixtures/markdown"
manifest_path="$fixtures_root/manifest.txt"
tmp_dir="$(mktemp -d)"
actual_output="$(mktemp)"

cleanup() {
  rm -rf "$tmp_dir"
  rm -f "$actual_output"
}

trap cleanup EXIT

trim_fixture_stem() {
  printf '%s' "$1" | sed -e 's/#.*$//' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'
}

load_fixture_stems() {
  while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
    local stem
    stem="$(trim_fixture_stem "$raw_line")"
    if [[ -n "$stem" ]]; then
      printf '%s\n' "$stem"
    fi
  done < "$manifest_path"
}

build_converter() {
  (
    cd "$repo_root/tools/pandoc-converter"
    cabal --project-file=cabal.project build forester-pandoc:exe:forester-pandoc >/dev/null
    cabal --project-file=cabal.project list-bin forester-pandoc:exe:forester-pandoc
  )
}

generate_mixed_corpus() {
  local output_path="$1"
  : > "$output_path"
  local iteration
  for iteration in $(seq 1 18); do
    while IFS= read -r stem; do
      cat "$fixtures_root/$stem.md" >> "$output_path"
      printf '\n\n<!-- benchmark iteration %s fixture %s -->\n\n' "$iteration" "$stem" >> "$output_path"
    done < <(load_fixture_stems)
  done
}

generate_tables_corpus() {
  local output_path="$1"
  : > "$output_path"
  local iteration
  for iteration in $(seq 1 220); do
    cat "$fixtures_root/tables/basic.md" >> "$output_path"
    printf '\n\n' >> "$output_path"
  done
}

benchmark_case() {
  local converter_bin="$1"
  local case_name="$2"
  local input_path="$3"
  local threshold_ms="$4"

  "$converter_bin" markdown-to-forester --diagnostics-format none "$input_path" > /dev/null 2>/dev/null

  local start_ms end_ms elapsed_ms input_bytes
  start_ms="$(date +%s%3N)"
  "$converter_bin" markdown-to-forester --diagnostics-format none "$input_path" > "$actual_output" 2>/dev/null
  end_ms="$(date +%s%3N)"
  elapsed_ms="$((end_ms - start_ms))"
  input_bytes="$(wc -c < "$input_path")"

  if [[ ! -s "$actual_output" ]]; then
    printf 'Benchmark case %s produced empty output.\n' "$case_name" >&2
    exit 1
  fi

  printf 'benchmark case=%s bytes=%s elapsed_ms=%s threshold_ms=%s\n' \
    "$case_name" \
    "$input_bytes" \
    "$elapsed_ms" \
    "$threshold_ms"

  if (( elapsed_ms > threshold_ms )); then
    printf 'Benchmark case %s exceeded threshold: %s ms > %s ms\n' \
      "$case_name" \
      "$elapsed_ms" \
      "$threshold_ms" \
      >&2
    exit 1
  fi
}

main() {
  local converter_bin mixed_corpus tables_corpus
  local mixed_threshold_ms="${FORESTER_PANDOC_BENCH_MAX_MS_MIXED:-2500}"
  local tables_threshold_ms="${FORESTER_PANDOC_BENCH_MAX_MS_TABLES:-3000}"

  converter_bin="$(build_converter)"
  mixed_corpus="$tmp_dir/mixed-large.md"
  tables_corpus="$tmp_dir/tables-large.md"

  generate_mixed_corpus "$mixed_corpus"
  generate_tables_corpus "$tables_corpus"

  benchmark_case "$converter_bin" "mixed-large" "$mixed_corpus" "$mixed_threshold_ms"
  benchmark_case "$converter_bin" "tables-large" "$tables_corpus" "$tables_threshold_ms"
}

main "$@"
