#
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later
#

set shell := ["bash", "-euo", "pipefail", "-c"]

default:
  @just --list

deps:
  opam install ./forester.opam --deps-only --with-test --yes

build:
  opam exec -- dune build @install

build-rust:
  cargo build --manifest-path tools/rust-parser/Cargo.toml --release

build-haskell:
  cd tools/pandoc-converter && cabal --project-file=cabal.project build all

fmt:
  ./scripts/format.sh

fmt-check:
  ./scripts/check-format.sh

lint-ocaml:
  opam exec -- dune build @install

lint-rust:
  cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings

lint-rust-deps:
  cargo deny --manifest-path tools/rust-parser/Cargo.toml --locked check bans licenses sources

lint-licenses:
  ./scripts/check-licenses.sh

lint:
  just lint-ocaml
  just lint-rust
  just lint-rust-deps

test-ocaml:
  opam exec -- dune runtest

test-rust:
  cargo test --manifest-path tools/rust-parser/Cargo.toml

test-haskell:
  cd tools/pandoc-converter && cabal --project-file=cabal.project test all
  ./scripts/test-markdown-converter.sh

test-markdown-bridge:
  export PATH="$HOME/.ghcup/bin:$PATH"; \
  cd tools/pandoc-converter; \
  cabal --project-file=cabal.project build forester-pandoc:exe:forester-pandoc; \
  converter_path="$(cabal --project-file=cabal.project list-bin forester-pandoc:exe:forester-pandoc)"; \
  cd ../..; \
  FORESTER_PANDOC_PATH="$converter_path" ./scripts/test-markdown-bridge-ocaml.sh

test-markdown-corpus:
  export PATH="$HOME/.ghcup/bin:$PATH"; \
  ./scripts/check-markdown-realworld-corpus.sh

benchmark-markdown-bridge:
  export PATH="$HOME/.ghcup/bin:$PATH"; \
  ./scripts/benchmark-markdown-converter.sh

test:
  just test-ocaml
  just test-rust

ci:
  just fmt-check
  just lint
  just test
  if command -v cabal >/dev/null 2>&1; then just test-haskell; else echo "Skipping Haskell scaffold checks because cabal is unavailable."; fi

install-local:
  ./scripts/install-local.sh

install-hooks:
  lefthook install

act-ci job="ci":
  act -W .github/workflows/ci.yml -j {{job}}
