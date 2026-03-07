# Differential Fuzz Regressions

`proptest` persists minimized failing inputs for `tests/differential_fuzz.rs` in this directory.

Workflow:

1. Run `cargo test --manifest-path tools/rust-parser/Cargo.toml --test differential_fuzz`.
2. If `proptest` finds a Rust-vs-OCaml mismatch, it shrinks the input automatically.
3. The minimized seed is recorded here so the failure is reproducible.
4. Once the bug is fixed, promote the minimized case into the shared parser fixture corpus when it represents a real grammar edge case.
