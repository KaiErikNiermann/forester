#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

thresholds_path="$repo_root/tools/rust-parser/benchmarks/thresholds.json"

output_path=""
fail_on_regression=0

usage() {
  cat <<'EOF'
Usage: ./scripts/benchmark-parser-performance.sh [--output path] [--fail-on-regression]
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --output)
      output_path="$2"
      shift 2
      ;;
    --fail-on-regression)
      fail_on_regression=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

resolve_rust_benchmark_binary() {
  local release_bin="$repo_root/tools/rust-parser/target/release/benchmark"

  cargo build --manifest-path "$repo_root/tools/rust-parser/Cargo.toml" --release --bin benchmark >/dev/null
  printf '%s\n' "$release_bin"
}

resolve_ocaml_benchmark_binary() {
  local exe="$repo_root/_build/default/lib/parser/test/Parser_benchmark.exe"
  require_cmd opam "Install opam so the OCaml benchmark executable can be built."
  opam exec -- dune build lib/parser/test/Parser_benchmark.exe >/dev/null
  printf '%s\n' "$exe"
}

make_corpora() {
  local temp_dir="$1"
  local combined="$temp_dir/positive-fixtures.tree"
  local amplified="$temp_dir/positive-amplified.tree"
  local target_bytes="$2"

  : > "$combined"
  local fixture
  for fixture in "$repo_root"/tools/rust-parser/tests/fixtures/positive/*.tree; do
    case "$(basename "$fixture")" in
      # These fixtures are intentionally only valid when the file ends
      # immediately after the final token, so concatenating them with a
      # newline would change their semantics.
      bare-backslash.tree|empty-ident-fragment.tree)
        continue
        ;;
    esac
    cat "$fixture" >> "$combined"
    printf '\n' >> "$combined"
  done

  cp "$combined" "$amplified"
  while [[ $(wc -c < "$amplified") -lt "$target_bytes" ]]; do
    cat "$combined" >> "$amplified"
  done
}

make_header_binder_corpora() {
  local temp_dir="$1"
  local combined="$temp_dir/header-binders-fixtures.tree"
  local amplified="$temp_dir/header-binders-amplified.tree"
  local target_bytes="$2"
  local fixture="$repo_root/tools/rust-parser/tests/fixtures/positive/header-binders.tree"

  cat "$fixture" > "$combined"
  printf '\n' >> "$combined"

  cp "$combined" "$amplified"
  while [[ $(wc -c < "$amplified") -lt "$target_bytes" ]]; do
    cat "$combined" >> "$amplified"
  done
}

run_measurement() {
  local parser_name="$1"
  local binary="$2"
  local corpus_name="$3"
  local corpus_path="$4"
  local runs="$5"
  local temp_dir="$6"
  local metrics_path="$7"
  python3 - "$binary" "$corpus_path" "$runs" "$metrics_path" <<'PY'
import os
import subprocess
import sys
import time

binary, corpus_path, runs, metrics_path = sys.argv[1:]

with open(metrics_path, "w", encoding="utf-8") as handle:
    for _ in range(int(runs)):
        start = time.perf_counter()
        process = subprocess.Popen(
            [binary, corpus_path],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            text=True,
        )
        _, status, rusage = os.wait4(process.pid, 0)
        elapsed = time.perf_counter() - start
        stderr = process.stderr.read() if process.stderr is not None else ""
        returncode = os.waitstatus_to_exitcode(status)
        if returncode != 0:
            raise SystemExit(
                f"benchmark command failed for {binary} {corpus_path}: {stderr.strip()}"
            )
        handle.write(f"{elapsed}\t{rusage.ru_maxrss}\n")
PY
}
require_cmd cargo "Install Rust and cargo to build the Rust benchmark binary."
require_cmd python3 "Install Python 3 so benchmark JSON can be summarized."

rust_benchmark_bin="$(resolve_rust_benchmark_binary)"
ocaml_benchmark_bin="$(resolve_ocaml_benchmark_binary)"

temp_dir="$(mktemp -d)"
trap 'rm -rf "$temp_dir"' EXIT

mapfile -t threshold_config < <(
  python3 "$repo_root/scripts/benchmark_parser_summary.py" threshold-config "$thresholds_path"
)

runs="${threshold_config[0]}"
amplified_target_bytes="${threshold_config[1]}"

make_corpora "$temp_dir" "$amplified_target_bytes"
make_header_binder_corpora "$temp_dir" "$amplified_target_bytes"

small_corpus="$temp_dir/positive-fixtures.tree"
large_corpus="$temp_dir/positive-amplified.tree"
header_small_corpus="$temp_dir/header-binders-fixtures.tree"
header_large_corpus="$temp_dir/header-binders-amplified.tree"

rust_small_metrics="$temp_dir/rust-positive-fixtures.tsv"
ocaml_small_metrics="$temp_dir/ocaml-positive-fixtures.tsv"
rust_large_metrics="$temp_dir/rust-positive-amplified.tsv"
ocaml_large_metrics="$temp_dir/ocaml-positive-amplified.tsv"
rust_header_small_metrics="$temp_dir/rust-header-binders-fixtures.tsv"
ocaml_header_small_metrics="$temp_dir/ocaml-header-binders-fixtures.tsv"
rust_header_large_metrics="$temp_dir/rust-header-binders-amplified.tsv"
ocaml_header_large_metrics="$temp_dir/ocaml-header-binders-amplified.tsv"

run_measurement rust "$rust_benchmark_bin" positive-fixtures "$small_corpus" "$runs" "$temp_dir" "$rust_small_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" positive-fixtures "$small_corpus" "$runs" "$temp_dir" "$ocaml_small_metrics"
run_measurement rust "$rust_benchmark_bin" positive-amplified "$large_corpus" "$runs" "$temp_dir" "$rust_large_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" positive-amplified "$large_corpus" "$runs" "$temp_dir" "$ocaml_large_metrics"
run_measurement rust "$rust_benchmark_bin" header-binders-fixtures "$header_small_corpus" "$runs" "$temp_dir" "$rust_header_small_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" header-binders-fixtures "$header_small_corpus" "$runs" "$temp_dir" "$ocaml_header_small_metrics"
run_measurement rust "$rust_benchmark_bin" header-binders-amplified "$header_large_corpus" "$runs" "$temp_dir" "$rust_header_large_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" header-binders-amplified "$header_large_corpus" "$runs" "$temp_dir" "$ocaml_header_large_metrics"

summary_path="$temp_dir/rust-parser-benchmarks.json"
python3 "$repo_root/scripts/benchmark_parser_summary.py" summarize \
  "$thresholds_path" \
  "positive-fixtures" \
  "$small_corpus" \
  "$rust_small_metrics" \
  "$ocaml_small_metrics" \
  "positive-amplified" \
  "$large_corpus" \
  "$rust_large_metrics" \
  "$ocaml_large_metrics" \
  "header-binders-fixtures" \
  "$header_small_corpus" \
  "$rust_header_small_metrics" \
  "$ocaml_header_small_metrics" \
  "header-binders-amplified" \
  "$header_large_corpus" \
  "$rust_header_large_metrics" \
  "$ocaml_header_large_metrics" \
  > "$summary_path"

if [[ -n "$output_path" ]]; then
  ensure_parent_dir "$output_path"
  cp "$summary_path" "$output_path"
else
  cat "$summary_path"
fi

python3 "$repo_root/scripts/benchmark_parser_summary.py" print-summary "$summary_path"

if [[ $fail_on_regression -eq 1 ]]; then
  python3 "$repo_root/scripts/benchmark_parser_summary.py" assert-ok "$summary_path"
fi
