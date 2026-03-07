#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

from __future__ import annotations

import argparse
import json
import random
import subprocess
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Literal

ParseClass = Literal["ok", "error", "timeout", "crash"]


@dataclass
class ParseResult:
    klass: ParseClass
    detail: str
    duration_ms: float


@dataclass
class Divergence:
    case: str
    ocaml: ParseClass
    rust: ParseClass


def run_cmd(cmd: list[str], input_text: str, timeout_s: float) -> tuple[int, str, str, float, bool]:
    start = time.perf_counter()
    try:
        proc = subprocess.run(
            cmd,
            input=input_text,
            text=True,
            capture_output=True,
            timeout=timeout_s,
            check=False,
        )
        elapsed = (time.perf_counter() - start) * 1000
        return proc.returncode, proc.stdout, proc.stderr, elapsed, False
    except subprocess.TimeoutExpired as exc:
        elapsed = (time.perf_counter() - start) * 1000
        return 124, exc.stdout or "", exc.stderr or "", elapsed, True


class ParserHarness:
    def __init__(self, ocaml_bin: Path, rust_bin: Path, timeout_s: float) -> None:
        self.ocaml_bin = ocaml_bin
        self.rust_bin = rust_bin
        self.timeout_s = timeout_s

    def parse_ocaml(self, case: str) -> ParseResult:
        rc, out, err, ms, timed_out = run_cmd([str(self.ocaml_bin)], case, self.timeout_s)
        if timed_out:
            return ParseResult("timeout", "timeout", ms)
        out = out.strip()
        if rc == 0 and out == "ok":
            return ParseResult("ok", "ok", ms)
        if rc == 0 and out == "error":
            return ParseResult("error", "error", ms)
        return ParseResult("crash", (err or out).strip()[:300], ms)

    def parse_rust(self, case: str) -> ParseResult:
        rc, out, err, ms, timed_out = run_cmd([str(self.rust_bin), "--json", "-"], case, self.timeout_s)
        if timed_out:
            return ParseResult("timeout", "timeout", ms)
        if rc not in (0, 1):
            return ParseResult("crash", (err or out).strip()[:300], ms)
        try:
            payload = json.loads(out)
        except json.JSONDecodeError:
            return ParseResult("crash", (err or out).strip()[:300], ms)
        status = payload.get("status")
        if status == "ok":
            return ParseResult("ok", "ok", ms)
        if status == "error":
            return ParseResult("error", "error", ms)
        return ParseResult("crash", f"unknown-status:{status}", ms)


def make_valid_seeds() -> list[str]:
    return [
        "\\p{hello world}",
        "\\p{hello}",
        "\\open\\foo/bar",
        "\\scope{\\p{}}",
        "\\def\\myMacro[x][~y]{\\p{#x #y}}",
        "\\let\\tmp{\\p{local}}",
        "\\fun[x]{\\p{#x}}",
        "\\put\\foo{bar}",
        "\\put?\\foo{bar}",
        "\\get\\foo",
        "\\alloc\\foo",
        "\\namespace\\foo{\\def\\bar{baz}}",
        "\\subtree[example]{\\p{inside}}",
        "\\object[self]{[render]{\\p{ok}}}",
        "\\patch{\\get\\obj}[self][super]{[render]{\\p{patched}}}",
        "\\call{\\get\\obj}{render}",
        "\\import{foundation}",
        "\\export{foundation}",
        "\\xmlns:fr{http://www.forester-notes.org}",
        "\\<html>",
        "#{a + b = c}",
        "##{x^2 + y^2}",
        "\\verb<<|asdf<<",
        "\\startverb\nhello\n\\stopverb",
        "\\datalog{?X -: {\\rel/is-node ?X}}",
    ]


def make_invalid_seeds() -> list[str]:
    return [
        "\\p{",
        "\\def\\foo[x]{",
        "\\object[self]{[m]{x}",
        "\\patch{\\get\\o}[self]{",
        "\\call{\\get\\o}",
        "\\namespace{foo}{bar}",
        "#{unclosed",
        "##{unclosed",
        "\\verb<<|unterminated",
        "\\startverb\nmissing stop",
        "\\datalog{?X - {\\rel/is-node ?X}}",
        "\\datalog{?X -: {}}",
        "\\xmlns:{broken}",
        "\\<:bad>",
        "\\open",
    ]


def rand_word(rng: random.Random) -> str:
    syll = ["al", "be", "ra", "to", "ki", "zen", "phi", "mu", "tri"]
    return "".join(rng.choice(syll) for _ in range(rng.randint(1, 3)))


def generate_valid_case(rng: random.Random, depth: int) -> str:
    if depth <= 0:
        return rand_word(rng)
    choice = rng.randint(0, 8)
    if choice == 0:
        return rand_word(rng)
    if choice == 1:
        return "{" + generate_valid_case(rng, depth - 1) + "}"
    if choice == 2:
        return "[" + generate_valid_case(rng, depth - 1) + "]"
    if choice == 3:
        return "(" + generate_valid_case(rng, depth - 1) + ")"
    if choice == 4:
        return "#{" + generate_valid_case(rng, depth - 1) + "}"
    if choice == 5:
        return "\\p{" + generate_valid_case(rng, depth - 1) + "}"
    if choice == 6:
        return "\\scope{" + generate_valid_case(rng, depth - 1) + "}"
    if choice == 7:
        return "\\def\\" + rand_word(rng) + "[x]{" + generate_valid_case(rng, depth - 1) + "}"
    return "\\subtree[" + rand_word(rng) + "]{" + generate_valid_case(rng, depth - 1) + "}"


def mutate_case(rng: random.Random, s: str) -> str:
    if not s:
        return "}"
    op = rng.randint(0, 6)
    if op == 0:
        i = rng.randrange(len(s))
        return s[:i] + s[i + 1 :]
    if op == 1:
        i = rng.randrange(len(s))
        return s[:i] + rng.choice(["{", "}", "[", "]", "#", "\\", "?", "-:"]) + s[i:]
    if op == 2:
        return s.replace("{", "", 1)
    if op == 3:
        return s.replace("}", "", 1)
    if op == 4:
        return s + rng.choice(["}", "]", "\\", "-:", "#"])
    if op == 5:
        return s.replace("\\p", "\\def", 1)
    return s.replace(" ", "  ")


def metamorphic_cases(seeds: list[str]) -> list[tuple[str, str]]:
    out: list[tuple[str, str]] = []
    for s in seeds:
        out.append((s, " ".join(s.split())))
        out.append((s, s.replace("\n", " \n")))
        out.append((s, s + "\n"))
    return out


def deep_nesting_case(n: int) -> str:
    return "\\p{" + ("{" * n) + "x" + ("}" * n) + "}"


def long_verbatim_case(lines: int) -> str:
    body = "\n".join("line" + str(i) for i in range(lines))
    return "\\startverb\n" + body + "\n\\stopverb"


def classify_batch(h: ParserHarness, cases: list[str]) -> dict[str, int]:
    stats = {"ok": 0, "error": 0, "timeout": 0, "crash": 0}
    for case in cases:
        r = h.parse_ocaml(case)
        stats[r.klass] += 1
    return stats


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--generated", type=int, default=300)
    parser.add_argument("--mutations", type=int, default=300)
    parser.add_argument("--timeout", type=float, default=2.0)
    args = parser.parse_args()

    repo = Path(__file__).resolve().parents[2]
    ocaml_bin = repo / "_build/default/tools/grammar-soundness/ocaml_parse_check.exe"
    rust_bin = repo / "tools/rust-parser/target/debug/forester-rust-parser"
    h = ParserHarness(ocaml_bin=ocaml_bin, rust_bin=rust_bin, timeout_s=args.timeout)

    rng = random.Random(args.seed)

    valid = make_valid_seeds()
    invalid = make_invalid_seeds()
    generated = [generate_valid_case(rng, depth=rng.randint(2, 6)) for _ in range(args.generated)]
    mutations = [mutate_case(rng, rng.choice(valid + generated)) for _ in range(args.mutations)]

    # Baseline acceptance/rejection
    valid_stats = classify_batch(h, valid)
    invalid_stats = classify_batch(h, invalid)
    generated_stats = classify_batch(h, generated)
    mutation_stats = classify_batch(h, mutations)

    # Differential
    differential_pool = valid + invalid + generated + mutations
    divergences: list[Divergence] = []
    for case in differential_pool:
        oc = h.parse_ocaml(case)
        ru = h.parse_rust(case)
        if oc.klass != ru.klass:
            divergences.append(Divergence(case=case[:400], ocaml=oc.klass, rust=ru.klass))

    # Metamorphic
    metamorphic = metamorphic_cases(valid[:20])
    metamorphic_breaks: list[str] = []
    for original, transformed in metamorphic:
        o = h.parse_ocaml(original)
        t = h.parse_ocaml(transformed)
        if o.klass != t.klass:
            metamorphic_breaks.append(original[:200] + " => " + transformed[:200])

    # Performance/resource probes
    perf_cases = [deep_nesting_case(n) for n in [64, 128, 256, 512, 768, 1024]]
    perf_results = []
    for case in perf_cases:
        r = h.parse_ocaml(case)
        perf_results.append({"len": len(case), "class": r.klass, "ms": round(r.duration_ms, 3)})

    resource_cases = [long_verbatim_case(2000), long_verbatim_case(8000)]
    resource_results = []
    for case in resource_cases:
        r = h.parse_ocaml(case)
        resource_results.append({"len": len(case), "class": r.klass, "ms": round(r.duration_ms, 3)})

    result = {
        "seed": args.seed,
        "timeout_s": args.timeout,
        "counts": {
            "valid": len(valid),
            "invalid": len(invalid),
            "generated": len(generated),
            "mutations": len(mutations),
            "differential_pool": len(differential_pool),
            "metamorphic_pairs": len(metamorphic),
        },
        "ocaml": {
            "valid_stats": valid_stats,
            "invalid_stats": invalid_stats,
            "generated_stats": generated_stats,
            "mutation_stats": mutation_stats,
        },
        "differential": {
            "divergence_count": len(divergences),
            "sample": [asdict(d) for d in divergences[:20]],
        },
        "metamorphic": {
            "break_count": len(metamorphic_breaks),
            "sample": metamorphic_breaks[:20],
        },
        "performance": perf_results,
        "resource": resource_results,
    }

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    print(f"Wrote: {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
