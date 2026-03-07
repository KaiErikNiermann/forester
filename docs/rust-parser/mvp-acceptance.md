# Rust Parser MVP Acceptance Criteria

Goal: the Rust parser is acceptable as an MVP only when it matches OCaml parser behavior across the supported grammar surface while providing materially better diagnostics.

## Required parity guarantees

- AST parity:
  - For shared positive fixtures, Rust and OCaml produce equivalent AST shape after location normalization where needed.
  - No command-specific ad hoc AST shapes are introduced in Rust for syntax that OCaml parses generically.
- Acceptance/rejection parity:
  - Shared negative fixtures fail in both parsers for the same structural reason class.
  - Top-level-only import/export restrictions match OCaml behavior.
- Lexer parity:
  - Backslash/keyword dispatch, identifier fragments, escapes, comments, verbatim, XML, datalog specials, and whitespace token retention match OCaml semantics.
- Span parity:
  - Every Rust located node and every Rust parse error uses real byte/line/column information derived from the original source.

## Minimum feature surface for MVP

The following areas must be out of `fail` status in the parity matrix before MVP sign-off:

- generic command identifiers and path parsing
- whitespace and textual-expression handling
- comments
- inline and block verbatim
- imports/exports top-level restriction
- `def`, `let`, `fun`, `scope`, `put`, `put?`, `get`, `alloc`, `open`, `namespace`, `subtree`
- object system: `object`, `patch`, `call`
- XML token/parsing support
- datalog token and grammar support
- group and math delimiters
- accurate spans and structured diagnostics

## Diagnostics quality bar

Rust does not need to copy OCaml's exact wording, but it must be better in these concrete ways:

- structured expected/found reporting
- accurate spans on lexer-mode failures and delimiter mismatches
- stable machine-readable error payloads for OCaml bridge integration
- human-readable reports that identify the opening delimiter or mode transition when relevant

## Required verification gates

From the repository root, the MVP branch must pass all relevant gates for the implemented surface:

```bash
just test-rust
just lint-rust
just test-ocaml
cargo test --manifest-path tools/rust-parser/Cargo.toml
cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings
```

In addition, parity-specific verification must exist for:

- shared positive grammar fixtures
- shared negative grammar fixtures
- OCaml-vs-Rust AST differential checks
- representative parser-error fixtures

## Explicit non-criteria

These do not block MVP sign-off:

- optional runtime rollout or backend selection flags
- post-parity module splitting / cleanup refactors
- advanced recovery mode beyond the strict parity mode

## Exit condition

The Rust parser MVP is ready only when the parity matrix can honestly say:

- no remaining `fail` rows in the MVP feature surface
- any remaining `partial` rows are limited to clearly documented diagnostic improvements, not grammar mismatches
- OCaml bridge and CI treat Rust parity regressions as blocking
