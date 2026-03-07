#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

from __future__ import annotations

import argparse
import json
import statistics
from pathlib import Path
from typing import Any


def load_json(path: str) -> Any:
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def cmd_threshold_config(thresholds_path: str) -> int:
    data = load_json(thresholds_path)
    print(data["runs"])
    print(data["amplified_target_bytes"])
    return 0


def load_metrics(path: str) -> dict[str, float]:
    rows: list[tuple[float, int]] = []
    with open(path, "r", encoding="utf-8") as handle:
        for line in handle:
            elapsed, rss = line.strip().split("\t")
            rows.append((float(elapsed), int(rss)))
    elapsed_values = sorted(value for value, _ in rows)
    rss_values = sorted(value for _, value in rows)
    return {
        "runs": float(len(rows)),
        "median_seconds": statistics.median(elapsed_values),
        "median_rss_kib": float(statistics.median(rss_values)),
    }


def parser_metrics(corpus_path: str, metrics_path: str) -> dict[str, float]:
    metrics = load_metrics(metrics_path)
    size_bytes = Path(corpus_path).stat().st_size
    metrics["bytes"] = float(size_bytes)
    metrics["throughput_bytes_per_second"] = size_bytes / metrics["median_seconds"]
    metrics["throughput_mib_per_second"] = metrics["throughput_bytes_per_second"] / (1024 * 1024)
    return metrics


def compare(
    thresholds: dict[str, Any],
    corpus_name: str,
    corpus_path: str,
    rust_path: str,
    ocaml_path: str,
) -> dict[str, Any]:
    corpus_thresholds = thresholds["corpora"][corpus_name]
    rust_metrics = parser_metrics(corpus_path, rust_path)
    ocaml_metrics = parser_metrics(corpus_path, ocaml_path)
    throughput_ratio = rust_metrics["throughput_bytes_per_second"] / ocaml_metrics["throughput_bytes_per_second"]
    rss_ratio = rust_metrics["median_rss_kib"] / ocaml_metrics["median_rss_kib"]
    regression = (
        throughput_ratio < corpus_thresholds["min_rust_vs_ocaml_throughput_ratio"]
        or rss_ratio > corpus_thresholds["max_rust_vs_ocaml_rss_ratio"]
    )
    return {
        "name": corpus_name,
        "bytes": int(rust_metrics["bytes"]),
        "thresholds": corpus_thresholds,
        "rust": rust_metrics,
        "ocaml": ocaml_metrics,
        "comparison": {
            "throughput_ratio": throughput_ratio,
            "rss_ratio": rss_ratio,
            "regression": regression,
        },
    }


def cmd_summarize(thresholds_path: str, corpus_args: list[str]) -> int:
    thresholds = load_json(thresholds_path)
    if len(corpus_args) % 4 != 0:
        raise SystemExit("expected corpus arguments in groups of four")
    corpora = [
        compare(thresholds, corpus_args[i], corpus_args[i + 1], corpus_args[i + 2], corpus_args[i + 3])
        for i in range(0, len(corpus_args), 4)
    ]
    overall_regression = any(corpus["comparison"]["regression"] for corpus in corpora)
    print(
        json.dumps(
            {
                "status": "regression" if overall_regression else "ok",
                "thresholds": thresholds,
                "corpora": corpora,
            },
            indent=2,
            sort_keys=True,
        )
    )
    return 0


def cmd_print_summary(summary_path: str) -> int:
    data = load_json(summary_path)
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
    return 0


def cmd_assert_ok(summary_path: str) -> int:
    data = load_json(summary_path)
    return 0 if data["status"] == "ok" else 1


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    threshold_config = subparsers.add_parser("threshold-config")
    threshold_config.add_argument("thresholds_path")

    summarize = subparsers.add_parser("summarize")
    summarize.add_argument("thresholds_path")
    summarize.add_argument("corpus_args", nargs="+")

    print_summary = subparsers.add_parser("print-summary")
    print_summary.add_argument("summary_path")

    assert_ok = subparsers.add_parser("assert-ok")
    assert_ok.add_argument("summary_path")

    return parser.parse_args()


def main() -> int:
    args = parse_args()
    match args.command:
        case "threshold-config":
            return cmd_threshold_config(args.thresholds_path)
        case "summarize":
            return cmd_summarize(args.thresholds_path, args.corpus_args)
        case "print-summary":
            return cmd_print_summary(args.summary_path)
        case "assert-ok":
            return cmd_assert_ok(args.summary_path)
        case _:
            raise AssertionError(f"unexpected command: {args.command}")


if __name__ == "__main__":
    raise SystemExit(main())
