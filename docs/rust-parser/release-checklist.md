# Rust Parser MVP Release Checklist

Use this checklist before calling the Rust parser ready for MVP use.

## Parity and correctness

- [ ] `docs/rust-parser/parity-matrix.md` is updated and no MVP-critical row is still `fail`
- [ ] `docs/rust-parser/mvp-acceptance.md` still matches the implemented parser surface
- [ ] shared positive fixture corpus passes in Rust strict mode
- [ ] shared negative fixture corpus fails in Rust strict mode
- [ ] OCaml-vs-Rust parity tests pass for the shared fixture corpus
- [ ] differential fuzzing is green with no untriaged fresh regression
- [ ] any intentionally accepted grammar drift is documented explicitly and approved

## Required verification commands

Run these from the repository root and attach or retain the output for the release candidate:

```bash
cargo fmt --manifest-path tools/rust-parser/Cargo.toml --all
cargo test --manifest-path tools/rust-parser/Cargo.toml
cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings
./scripts/test-ocaml.sh lib/parser/test
./scripts/benchmark-parser-performance.sh --output /tmp/rust-parser-bench.json --fail-on-regression
```

Checklist:

- [ ] formatting is clean
- [ ] Rust unit, integration, fixture, golden, and differential tests are green
- [ ] OCaml parser-sync suite is green with the Rust parser binary required
- [ ] benchmark script reports `"status": "ok"`

## Diagnostics quality

- [ ] delimiter mismatch, unexpected closer, unexpected EOF, and lexer-mode failures still produce structured diagnostics
- [ ] error golden snapshots in `tools/rust-parser/tests/fixtures/error-golden/` match the intended current contract
- [ ] `docs/rust-parser/json-contract.md` matches the emitted JSON envelope
- [ ] `tools/rust-parser/json-schema/parse-result.schema.json` matches the checked-in contract and tests prove that it does
- [ ] ariadne reports were spot-checked for at least one lexer failure and one delimiter failure

## Bridge and integration stability

- [ ] `lib/parser/Rust_parser.ml` strict mode still fails closed when the Rust binary is missing, times out, or emits invalid JSON
- [ ] `lib/parser/test/Test_rust_parser_json_conversion.ml` is green
- [ ] `lib/parser/test/Test_rust_parser_process.ml` is green
- [ ] `lib/parser/test/Test_rust_parser_parity.ml` is green
- [ ] CI still builds the Rust parser before the OCaml parser-sync path

## Fixtures and corpora

- [ ] every file in `tools/rust-parser/tests/fixtures/positive/` is intentionally positive
- [ ] every file in `tools/rust-parser/tests/fixtures/negative/` is intentionally negative
- [ ] newly discovered accepted cases were moved out of the negative corpus instead of forcing false failures
- [ ] newly discovered rejected cases have a focused unit test, fixture, or golden snapshot
- [ ] `tools/rust-parser/proptest-regressions/differential_fuzz.txt` contains only understood and useful regressions

## Performance and operational sanity

- [ ] benchmark thresholds in `tools/rust-parser/benchmarks/thresholds.json` still reflect an intentional bar
- [ ] no new pathological slowdown or RSS regression appears in the benchmark artifact
- [ ] the Rust parser binary can be built from a clean checkout with `cargo build --manifest-path tools/rust-parser/Cargo.toml`
- [ ] `./scripts/test-ocaml.sh` can resolve a Rust parser binary without manual path surgery on a normal developer checkout

## Documentation and sign-off

- [ ] `docs/rust-parser/developer-workflow.md` reflects the actual commands and harnesses in the repo
- [ ] `docs/rust-parser/source-of-truth.md` still points to the right OCaml and spec authorities
- [ ] any deferred post-MVP work is captured as explicit tasks rather than implicit tribal knowledge
- [ ] release owner records the exact commit SHA used for the release candidate
- [ ] release owner signs off that the Rust parser may be treated as MVP-ready for parity-gated use

## Explicit blockers

Do not mark MVP ready if any of these remain true:

- an MVP feature row in the parity matrix is still `fail`
- parser-sync CI is red or not exercising the Rust parser
- the OCaml bridge can silently drop into a misleading success path when the Rust parser fails
- structured diagnostics or JSON shape changed without golden/schema/doc updates
- fixture corpora contain known misclassified cases
