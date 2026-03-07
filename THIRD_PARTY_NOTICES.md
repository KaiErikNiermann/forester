<!--
SPDX-FileCopyrightText: 2026 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

# Third-Party Notices

This repository includes or depends on third-party software. The canonical SPDX license texts live in the top-level `LICENSES/` directory, and machine-readable annotations are tracked with `REUSE.toml`.

## Dependency manifests

- OCaml dependencies are declared in `forester.opam` and the generated lock files.
- Rust dependencies for the parser live in `tools/rust-parser/Cargo.toml` and `tools/rust-parser/Cargo.lock`.
- Haskell dependencies for the Markdown converter scaffold live in `tools/pandoc-converter/forester-pandoc.cabal`.
- Front-end theme dependencies live in `bin/forester/theme/package.json` and `bin/forester/theme/package-lock.json`.

## Vendored and bundled assets

- The bundled theme assets under `bin/forester/theme/` include fonts, JavaScript, CSS, and favicon assets.
- License texts relevant to those assets are stored under `bin/forester/theme/LICENSES/` and the top-level `LICENSES/`.

## Compliance workflow

The repository uses REUSE / SPDX conventions for third-party attribution:

```bash
just lint-licenses
```

For release and distribution review, treat the manifests above plus `LICENSES/` as the source of truth.
