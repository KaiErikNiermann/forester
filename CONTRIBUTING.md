<!--
SPDX-FileCopyrightText: 2026 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

# Contributing

## Development workflow

Use the root `Justfile` instead of ad hoc commands whenever possible:

```bash
just deps
just fmt
just lint
just test
just install-hooks
```

The repository currently spans three language surfaces:

- OCaml: the main compiler, runtime, CLI, and language server
- Rust: the optional parser implementation in `tools/rust-parser/`
- Haskell: the Pandoc-backed Markdown converter scaffold in `tools/pandoc-converter/`

## Formatting and Linting

- OCaml formatting is standardized on `topiary`, not `ocamlformat`.
- Rust formatting uses `cargo fmt`.
- Rust linting uses `cargo clippy -- -D warnings`.
- SPDX / REUSE checks use `reuse lint`.

Before opening a pull request, run:

```bash
just ci
```

If you also have Cabal installed locally, `just ci` will include the Haskell scaffold tests automatically.

To install the local Git hooks managed by `lefthook`:

```bash
just install-hooks
```

The repository ships a `pre-push` hook that runs formatting checks, linting, and tests through the existing `just` targets. If `topiary` is not installed locally, the hook skips the OCaml formatting check but still runs the Rust `cargo fmt --check` path.

## Local installation

For a developer-local install into `~/.local/bin`:

```bash
just install-local
```

This installs:

- `forester`
- `forester-rust-parser`
- `forester-pandoc` when Cabal is available

You can override the location with `PREFIX=/custom/prefix just install-local`.

## CI expectations

GitHub Actions is the canonical CI target for this repository. New changes should preserve:

- `just fmt-check`
- `just lint`
- `just test`
- `just lint-licenses`

The workflow file is `.github/workflows/ci.yml`, and contributors with `act` installed can use `just act-ci` for local validation.

## Design guidance

- Prefer small, reviewable changes over large mixed refactors.
- Keep multi-language integration points explicit.
- Reuse existing commands and scripts rather than adding parallel workflows.
- Preserve SPDX headers in new source and documentation files.
