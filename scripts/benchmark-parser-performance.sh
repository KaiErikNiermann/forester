#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd -- "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
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

require_cmd() {
  local command_name="$1"
  local install_hint="$2"
  if ! command -v "$command_name" >/dev/null 2>&1; then
    printf 'Missing required command: %s\n%s\n' "$command_name" "$install_hint" >&2
    exit 1
  fi
}

resolve_rust_benchmark_binary() {
  local debug_bin="$repo_root/tools/rust-parser/target/debug/benchmark"
  local release_bin="$repo_root/tools/rust-parser/target/release/benchmark"

  if [[ -x "$release_bin" ]]; then
    printf '%s\n' "$release_bin"
    return
  fi

  cargo build --manifest-path "$repo_root/tools/rust-parser/Cargo.toml" --release --bin benchmark >/dev/null
  printf '%s\n' "$release_bin"
}

resolve_ocaml_benchmark_binary() {
  local exe="$repo_root/_build/default/lib/parser/test/Parser_benchmark.exe"
  if [[ -x "$exe" ]]; then
    printf '%s\n' "$exe"
    return
  fi

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
      bare-backslash.tree)
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
  python3 - "$thresholds_path" <<'PY'
import json
import sys

with open(sys.argv[1], "r", encoding="utf-8") as handle:
    data = json.load(handle)

print(data["runs"])
print(data["amplified_target_bytes"])
PY
)

runs="${threshold_config[0]}"
amplified_target_bytes="${threshold_config[1]}"

make_corpora "$temp_dir" "$amplified_target_bytes"

small_corpus="$temp_dir/positive-fixtures.tree"
large_corpus="$temp_dir/positive-amplified.tree"

rust_small_metrics="$temp_dir/rust-positive-fixtures.tsv"
ocaml_small_metrics="$temp_dir/ocaml-positive-fixtures.tsv"
rust_large_metrics="$temp_dir/rust-positive-amplified.tsv"
ocaml_large_metrics="$temp_dir/ocaml-positive-amplified.tsv"

run_measurement rust "$rust_benchmark_bin" positive-fixtures "$small_corpus" "$runs" "$temp_dir" "$rust_small_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" positive-fixtures "$small_corpus" "$runs" "$temp_dir" "$ocaml_small_metrics"
run_measurement rust "$rust_benchmark_bin" positive-amplified "$large_corpus" "$runs" "$temp_dir" "$rust_large_metrics"
run_measurement ocaml "$ocaml_benchmark_bin" positive-amplified "$large_corpus" "$runs" "$temp_dir" "$ocaml_large_metrics"

json_output="$(
  python3 - \
    "$thresholds_path" \
    "$small_corpus" \
    "$large_corpus" \
    "$rust_small_metrics" \
    "$ocaml_small_metrics" \
    "$rust_large_metrics" \
    "$ocaml_large_metrics" <<'PY'
import json
import statistics
import sys
from pathlib import Path

thresholds_path, small_corpus, large_corpus, rust_small, ocaml_small, rust_large, ocaml_large = sys.argv[1:]

with open(thresholds_path, "r", encoding="utf-8") as handle:
    thresholds = json.load(handle)

def load_metrics(path: str) -> dict[str, float]:
    rows: list[tuple[float, int]] = []
    with open(path, "r", encoding="utf-8") as handle:
        for line in handle:
            elapsed, rss = line.strip().split("\t")
            rows.append((float(elapsed), int(rss)))
    elapsed_values = sorted(value for value, _ in rows)
    rss_values = sorted(value for _, value in rows)
    return {
        "runs": len(rows),
        "median_seconds": statistics.median(elapsed_values),
        "median_rss_kib": statistics.median(rss_values),
    }

def parser_metrics(corpus_path: str, metrics_path: str) -> dict[str, float]:
    metrics = load_metrics(metrics_path)
    size_bytes = Path(corpus_path).stat().st_size
    metrics["bytes"] = size_bytes
    metrics["throughput_bytes_per_second"] = size_bytes / metrics["median_seconds"]
    metrics["throughput_mib_per_second"] = metrics["throughput_bytes_per_second"] / (1024 * 1024)
    return metrics

def compare(corpus_name: str, rust_metrics: dict[str, float], ocaml_metrics: dict[str, float]) -> dict[str, object]:
    corpus_thresholds = thresholds["corpora"][corpus_name]
    throughput_ratio = rust_metrics["throughput_bytes_per_second"] / ocaml_metrics["throughput_bytes_per_second"]
    rss_ratio = rust_metrics["median_rss_kib"] / ocaml_metrics["median_rss_kib"]
    regression = (
        throughput_ratio < corpus_thresholds["min_rust_vs_ocaml_throughput_ratio"]
        or rss_ratio > corpus_thresholds["max_rust_vs_ocaml_rss_ratio"]
    )
    return {
        "name": corpus_name,
        "bytes": rust_metrics["bytes"],
        "thresholds": corpus_thresholds,
        "rust": rust_metrics,
        "ocaml": ocaml_metrics,
        "comparison": {
            "throughput_ratio": throughput_ratio,
            "rss_ratio": rss_ratio,
            "regression": regression,
        },
    }

small = compare(
    "positive-fixtures",
    parser_metrics(small_corpus, rust_small),
    parser_metrics(small_corpus, ocaml_small),
)
large = compare(
    "positive-amplified",
    parser_metrics(large_corpus, rust_large),
    parser_metrics(large_corpus, ocaml_large),
)

overall_regression = any(corpus["comparison"]["regression"] for corpus in [small, large])

print(
    json.dumps(
        {
            "status": "regression" if overall_regression else "ok",
            "thresholds": thresholds,
            "corpora": [small, large],
        },
        indent=2,
        sort_keys=True,
    )
)
PY
)"

if [[ -n "$output_path" ]]; then
  printf '%s\n' "$json_output" > "$output_path"
else
  printf '%s\n' "$json_output"
fi

python3 - "$json_output" <<'PY'
import json
import sys

data = json.loads(sys.argv[1])
for corpus in data["corpora"]:
    comparison = corpus["comparison"]
    print(
        f"{corpus['name']}: "
        f"rust={corpus['rust']['throughput_mib_per_second']:.2f} MiB/s, "
        f"ocaml={corpus['ocaml']['throughput_mib_per_second']:.2f} MiB/s, "
        f"throughput_ratio={comparison['throughput_ratio']:.2f}, "
        f"rss_ratio={comparison['rss_ratio']:.2f}, "
        f"status={'regression' if comparison['regression'] else 'ok'}"
    )
PY

if [[ $fail_on_regression -eq 1 ]]; then
  python3 - "$json_output" <<'PY'
import json
import sys

data = json.loads(sys.argv[1])
if data["status"] != "ok":
    sys.exit(1)
PY
fi
