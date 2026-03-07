# Phase 1 Header-Binder Benchmark Report

This report captures the benchmark gate for phase-1 parenthesized header-binder sugar.

## Benchmarked Corpora

- `positive-fixtures`: the existing shared positive fixture corpus
- `positive-amplified`: the shared positive corpus repeated to the configured stress size
- `header-binders-fixtures`: the dedicated `tools/rust-parser/tests/fixtures/positive/header-binders.tree` corpus
- `header-binders-amplified`: the dedicated header-binder corpus repeated to the configured stress size

The concatenated general corpora deliberately exclude positive fixtures whose
validity depends on immediate end-of-file (`bare-backslash.tree` and
`empty-ident-fragment.tree`). Those fixtures remain part of the correctness
suite, but benchmarking them inside a newline-joined corpus would measure a
different input language than the one the parser tests exercise.

## Gate Intent

The benchmark gate is not trying to prove that the Rust parser is globally fast. It is trying to detect whether the phase-1 sugar path introduces a new regression relative to the existing parser baseline.

The dedicated header-binder corpora exist because the general positive corpus is too broad to isolate:

- extra lexer work around `(`, `)`, and comma-separated binder entries;
- parser branch selection between square and parenthesized headers;
- recovery-mode cost when malformed parenthesized headers are present.

## Pathological-Input Considerations

The main pathological shapes for this sugar wave are:

- very long parenthesized binder lists with many commas;
- repeated malformed headers that fail immediately before `)` and exercise recovery;
- large files that alternate between longhand `[]` and sugar `()` forms and therefore hit both header branches frequently.

The current implementation keeps these risks bounded by:

- treating `()` sugar as local header desugaring only;
- validating binder entries without introducing any new precedence or nesting rules;
- recovering through `)` explicitly so malformed headers do not poison the remainder of the document;
- preserving the existing longhand `[]` path unchanged.

## Verification Command

Run:

```bash
./scripts/benchmark-parser-performance.sh --output /tmp/rust-parser-bench.json --fail-on-regression
```

The JSON artifact should remain `\"status\": \"ok\"`, including the dedicated header-binder corpora, before phase-1 sugar is treated as rollout-ready.

## Current Baseline

Measured on this repository state with the command above:

| Corpus | Rust MiB/s | OCaml MiB/s | Throughput Ratio | RSS Ratio | Status |
| --- | ---: | ---: | ---: | ---: | --- |
| `positive-fixtures` | `0.55` | `0.42` | `1.32` | `1.01` | `ok` |
| `positive-amplified` | `0.08` | `3.80` | `0.02` | `1.00` | `ok` |
| `header-binders-fixtures` | `0.11` | `0.04` | `2.79` | `1.01` | `ok` |
| `header-binders-amplified` | `0.15` | `4.01` | `0.04` | `1.00` | `ok` |

The dedicated phase-1 sugar corpora stayed comfortably above their configured throughput floors:

- `header-binders-fixtures`: floor `0.50`, observed `2.79`
- `header-binders-amplified`: floor `0.02`, observed `0.04`
