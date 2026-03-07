# Rust Parser Developer Workflow

This document is the operational workflow for keeping the Rust parser and the OCaml parser in sync.

## Prerequisites

- Rust toolchain with `cargo`
- OCaml toolchain with `opam` and `dune`
- Repository root as current working directory

If the Rust parser binary is not already built, the OCaml-side parser-sync wrapper will build it automatically.

## Daily build and test loop

For normal Rust parser work, run the narrow loop first:

```bash
cargo fmt --manifest-path tools/rust-parser/Cargo.toml --all
cargo test --manifest-path tools/rust-parser/Cargo.toml
cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings
```

Before landing parser changes, run the OCaml-facing parity gate too:

```bash
./scripts/test-ocaml.sh lib/parser/test
```

Repository-level shortcuts:

```bash
just build-rust
just test-rust
just lint-rust
just test-parser-sync
```

`just test-parser-sync` is the main "do these parsers still agree" entrypoint. It resolves a Rust parser binary, exports `FORESTER_RUST_PARSER_PATH`, requires the binary by default via `FORESTER_RUST_PARSER_REQUIRE_BINARY=1`, and then runs the OCaml parser test suite slice under `lib/parser/test`.

## Key binaries and harnesses

- Rust CLI parser: `cargo run --manifest-path tools/rust-parser/Cargo.toml --bin forester-rust-parser -- --strict --json <file>`
- Rust recovery mode: `cargo run --manifest-path tools/rust-parser/Cargo.toml --bin forester-rust-parser -- --recovery --json <file>`
- OCaml oracle for AST comparisons: `_build/default/lib/parser/test/Parser_oracle.exe`
- OCaml/Rust parity suite: `_build/default/lib/parser/test/Test_rust_parser_parity.exe`
- OCaml JSON bridge conversion suite: `_build/default/lib/parser/test/Test_rust_parser_json_conversion.exe`
- OCaml subprocess/fallback suite: `_build/default/lib/parser/test/Test_rust_parser_process.exe`
- Benchmark harness: `./scripts/benchmark-parser-performance.sh --output /tmp/rust-parser-bench.json --fail-on-regression`

## Triage flow for AST diffs

When a fixture or fuzz case diverges, reduce it to a concrete input and compare both parser front doors directly.

1. Capture the input in a standalone `.tree` file.
2. Ask the OCaml oracle for normalized AST output:

```bash
_build/default/lib/parser/test/Parser_oracle.exe strict path/to/case.tree
```

3. Ask the Rust parser for the strict-mode JSON envelope:

```bash
cargo run --manifest-path tools/rust-parser/Cargo.toml \
  --bin forester-rust-parser -- --strict --json path/to/case.tree
```

4. Classify the difference before editing code:
   - lexer parity issue: tokenization/mode transition mismatch
   - grammar parity issue: both token streams are plausible, but AST shape diverges
   - diagnostics-only issue: acceptance/rejection matches, but error kind/span/report is weaker or drifted
   - fixture classification issue: the corpus entry is in the wrong positive/negative bucket
5. Fix the narrowest layer that explains the divergence.
6. Re-run `cargo test`, `cargo clippy`, and `./scripts/test-ocaml.sh lib/parser/test`.

## Fixture maintenance rules

Shared corpora live under `tools/rust-parser/tests/fixtures/`.

- `positive/*.tree`
  - must parse successfully in Rust strict mode
  - must also be accepted by the OCaml parity suite
  - should exercise real grammar coverage, not redundant happy paths
- `negative/*.tree`
  - must fail in Rust strict mode
  - should remain structurally invalid, not just unsupported by one implementation
  - if a case becomes valid after parity work, move it to `positive/` instead of weakening the test
- `error-golden/*.tree` and matching `.json`
  - lock the machine-readable error envelope and normalized human report text
  - update the `.json` only when the error contract changes intentionally
  - do not use error goldens as substitutes for parity fixtures

If a differential-fuzz regression uncovers a real parser gap, add one or more focused unit tests and then decide whether the minimized case belongs in `positive/`, `negative/`, or `error-golden/`.

## Differential fuzz workflow

The Rust-side differential harness compares Rust parser outcomes against the OCaml oracle.

```bash
cargo test --manifest-path tools/rust-parser/Cargo.toml --test differential_fuzz
```

If it fails:

1. inspect `tools/rust-parser/proptest-regressions/differential_fuzz.txt`
2. minimize or understand the generated case
3. add a direct unit or fixture regression
4. keep the saved regression file if it still captures a useful weak spot after the fix

## When to update docs and contracts

Update the checked-in docs or contract artifacts whenever parser behavior moves in a way that changes the supported developer workflow.

- JSON envelope or error shape changes: update `docs/rust-parser/json-contract.md` and `tools/rust-parser/json-schema/parse-result.schema.json`
- parity status changes: update `docs/rust-parser/parity-matrix.md` or `docs/rust-parser/mvp-acceptance.md` if the movement is substantive
- benchmark expectations change: update `tools/rust-parser/benchmarks/thresholds.json` and `tools/rust-parser/benchmarks/README.md`

## Recommended pre-merge gate

For parser work that touches lexer, parser, JSON bridge, or fixture corpora, the expected pre-merge gate is:

```bash
cargo fmt --manifest-path tools/rust-parser/Cargo.toml --all
cargo test --manifest-path tools/rust-parser/Cargo.toml
cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings
./scripts/test-ocaml.sh lib/parser/test
./scripts/benchmark-parser-performance.sh --output /tmp/rust-parser-bench.json --fail-on-regression
```
