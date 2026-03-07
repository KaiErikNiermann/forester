# Chumsky Upgrade Evaluation

Date: 2026-03-07
Status: defer upgrade; stay on `chumsky = "0.9"` for now

## Scope

Evaluate whether the Rust parser should move off `chumsky 0.9` to current upstream in order to gain better built-in error and recovery APIs.

## Inputs

- Current crate metadata from `cargo info chumsky@0.9.3`
- Current upstream crate metadata from `cargo info chumsky`
- Current upstream guide examples for `extra::Err<Rich<...>>`, `parse(...).into_output_errors()`, and `recover_with(...)`
- Disposable compile probe against `chumsky = "1.0.0-alpha.8"`

## Current parser coupling to Chumsky 0.9

The Forester parser is not using `chumsky` as a thin combinator layer. It is tightly coupled to the `0.9` API surface in these areas:

- `Simple<Token>` is the parser error type across helper combinators and parser signatures.
- `SimpleReason` is matched directly and translated into the project-specific `ParseError` model.
- `Stream::from_iter(...)` is used to build a token stream over the lexer output.
- Recovery helpers are written around `BoxedParser<'static, Token, O, Simple<Token>>`.
- The project already layers custom delimiter tracking, structured JSON diagnostics, ariadne labels/notes, and explicit strict vs recovery modes on top of `chumsky`.

This means the upgrade question is not just dependency freshness. It is a parser-internals rewrite question.

## Upstream state as of this evaluation

- The current stable line is still `0.9.3`.
- The latest crates.io release is `1.0.0-alpha.8`.
- The newer API examples use lifetime-parameterized parser traits and `extra::Err<Rich<...>>` instead of the `0.9` `Simple<_>` model.
- Recovery examples are expressed through the newer `ParseResult` flow (`parse(...).into_output_errors()`) rather than the exact `0.9` `parse_recovery(...)` path currently used here.

In other words: the interesting richer API surface exists, but it is still on the alpha track rather than a stable release line.

## Disposable upgrade probe

A local probe was run by temporarily changing `tools/rust-parser/Cargo.toml` to `chumsky = "1.0.0-alpha.8"` and running:

```sh
cargo check --manifest-path tools/rust-parser/Cargo.toml
```

Representative failures from that probe:

- `Parser<Token, O, Error = Simple<Token>>` bounds now require explicit lifetimes.
- `SimpleReason` is no longer available at `chumsky::error::SimpleReason`.
- Helper return types such as `BoxedParser<'static, Token, O, Simple<Token>>` no longer type-check as written.
- The parser helper layer fails early before getting to higher-level grammar behavior.

This is not a one-line migration. It is a broad refactor of parser signatures, error conversion, and recovery glue.

## Decision

Do not upgrade to `chumsky 1.0.0-alpha.8` yet.

Reasons:

1. The target release line is still alpha.
2. The current parser already has the core benefits we needed from the newer ecosystem work:
   - structured machine-readable diagnostics
   - ariadne-backed human reports
   - targeted recovery strategies
   - explicit strict vs recovery modes
3. The migration cost is high because the parser is deeply coupled to `0.9` error and stream APIs.
4. Upgrading now would spend time on framework churn instead of remaining parser-parity, fuzzing, and performance tasks.

## Equivalent approach while staying on 0.9

Keep the current architecture and continue improving it locally:

- Treat `ParseError` as the source of truth for bridge-facing diagnostics.
- Continue translating `chumsky 0.9` errors into richer project-specific categories.
- Keep recovery behavior in project-owned helpers rather than depending on upstream alpha APIs.
- Use ariadne only as the final presentation layer; keep machine-readable diagnostics independent from report formatting.

This preserves the main user-facing benefits without taking on unstable dependency churn.

## Revisit criteria

Re-open the upgrade when at least one of these is true:

1. `chumsky` ships a stable `1.x` release with the `Rich`/`extra::Err` model.
2. A concrete parser requirement cannot be implemented cleanly on top of the current project-owned recovery/diagnostic layer.
3. We are already planning a larger parser refactor, making the lifetime/signature rewrite cheaper to absorb.

## Expected migration shape when revisited

When the upgrade is revisited, assume the work will include all of the following:

- rewrite parser/helper signatures to the lifetime-parameterized `Parser<'src, ...>` form
- replace `Simple<Token>`/`SimpleReason` conversions with a `Rich`-based translation layer
- replace token-stream construction and parse entrypoints to the newer input/result model
- revalidate strict vs recovery semantics
- rerun JSON contract, OCaml parity, negative fixture, and recovery snapshot tests
