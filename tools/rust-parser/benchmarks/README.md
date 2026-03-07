# Rust Parser Benchmark Baselines

The parser benchmark harness compares the Rust parser against the OCaml parser on
two shared corpora derived from `tools/rust-parser/tests/fixtures/positive/`:

- `positive-fixtures`: one concatenated pass over the positive fixture corpus
- `positive-amplified`: the same corpus repeated until it reaches the configured
  target byte size in `thresholds.json`

Thresholds are per-corpus because the current Rust parser has very different
performance characteristics on tiny versus amplified inputs:

- the small corpus baseline expects Rust to stay close to OCaml throughput
- the amplified corpus baseline currently records that Rust is substantially
  slower than OCaml on repeated stress input

This is intentional for now. The benchmark task here is to make drift visible
and machine-readable, not to pretend the current large-corpus behavior is already
good. Once parser optimization work lands, raise the amplified throughput floor
instead of lowering it further.

CI writes a JSON artifact with the measured medians, throughput ratios, RSS
ratios, and regression status for each corpus.
