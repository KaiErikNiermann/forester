<!--
SPDX-FileCopyrightText: 2026 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

# Building Forester

Forester now has a single top-level task runner in the repository root:

```bash
just --list
```

The default local workflow is:

```bash
just deps
just build
just test
```

## Local toolchains

- OCaml: `opam exec -- dune ...`
- Rust parser: `cargo --manifest-path tools/rust-parser/Cargo.toml ...`
- Markdown converter scaffold: `cabal --project-file=tools/pandoc-converter/cabal.project ...`

Common entry points:

```bash
just fmt
just fmt-check
just lint
just test
just install-hooks
just install-local
```

## Nix

The existing flake still works for a fully provisioned shell:

```bash
nix develop
just ci
```

This remains the quickest way to get the OCaml formatter (`topiary`), REUSE tooling, and the dune toolchain together on Linux.

## Docker and Dev Containers

The repository now includes:

- `docker/Dockerfile.dev`: a baseline Linux image with OCaml, Rust, Cabal, `just`, `topiary`, and REUSE tooling.
- `.devcontainer/devcontainer.json`: a VS Code / devcontainer entry point built from that Dockerfile.

These are intended for a reproducible contributor environment, not for release images.

## GitHub Actions

The SourceHut build manifests can coexist for now, but the primary CI definition is:

- `.github/workflows/ci.yml`

The workflow covers:

- SPDX / REUSE validation
- OCaml build and tests
- Rust formatting, tests, and Clippy
- Haskell converter scaffold build and tests

## Local workflow verification with `act`

When `act` is installed, you can run the main jobs locally:

```bash
just act-ci
just act-ci job=license
just act-ci job=haskell-converter
```

If your local `act` image is minimal, use a full Ubuntu image such as `catthehacker/ubuntu:act-latest`.

## Git hooks

This repository uses `lefthook` for local Git hooks. Install them with:

```bash
just install-hooks
```

The `pre-push` hook runs formatting checks, linting, and tests against the same root `just` commands used in CI.
