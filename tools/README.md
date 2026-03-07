<!--
SPDX-FileCopyrightText: 2026 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

# Tools

This directory contains auxiliary components that support the main OCaml Forester codebase but are not the core application itself.

- `rust-parser/`: the optional Rust parser implementation and its dependency policy (`Cargo.toml`, `Cargo.lock`, `deny.toml` at the repository root).
- `pandoc-converter/`: the Pandoc-backed Haskell converter scaffold for Markdown and Forester transformations.
- `grammar-soundness/`: reproducible fuzzing, differential, and stress-test harnesses used to audit parser grammar robustness.

Keeping these under `tools/` makes the repository structure role-oriented rather than language-oriented: the primary product remains at the repository root, and auxiliary multi-language components live in one clearly-defined place.
