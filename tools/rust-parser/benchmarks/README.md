# Rust Parser Benchmark Baselines

The parser benchmark harness compares the Rust parser against the OCaml parser on
four shared corpora derived from `tools/rust-parser/tests/fixtures/positive/`:

- `positive-fixtures`: one concatenated pass over the positive fixture corpus
- `positive-amplified`: the same corpus repeated until it reaches the configured
  target byte size in `thresholds.json`
- `header-binders-fixtures`: one concatenated pass over the dedicated
  `header-binders.tree` phase-1 sugar fixture
- `header-binders-amplified`: the header-binder fixture repeated until it
  reaches the configured target byte size

Thresholds are per-corpus because the current Rust parser has very different
performance characteristics across general versus sugar-specific inputs:

- the small corpus baseline expects Rust to stay close to OCaml throughput
- the amplified corpus baseline currently records that Rust is substantially
  slower than OCaml on repeated stress input
- the header-binder baselines isolate the cost of parsing and recovering around
  parenthesized header lists without unrelated grammar families diluting the signal

This is intentional for now. The benchmark task here is to make drift visible
and machine-readable, not to pretend the current large-corpus behavior is already
good. Once parser optimization work lands, raise the amplified throughput floor
instead of lowering it further.

The concatenated general corpora intentionally skip positive fixtures whose
acceptance depends on immediate end-of-file, currently:

- `bare-backslash.tree`
- `empty-ident-fragment.tree`

Those cases are still covered in normal parser tests. They are excluded here
because adding a newline between concatenated fixtures would otherwise change the
input semantics and produce a misleading benchmark failure.

CI writes a JSON artifact with the measured medians, throughput ratios, RSS
ratios, and regression status for each corpus.
