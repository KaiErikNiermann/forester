#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

PREFIX="${PREFIX:-$HOME/.local}"
BINDIR="${BINDIR:-$PREFIX/bin}"
COMMAND="install"
SKIP_DEPS=false
SKIP_RUST=false
SKIP_HASKELL=false

info() {
  printf '[install-local] %s\n' "$1"
}

warn() {
  printf '[install-local] %s\n' "$1" >&2
}

install_ocaml_deps() {
  require_cmd opam "Install opam before running install-local."
  (
    cd "$repo_root"
    opam install ./forester.opam --deps-only --with-test --yes
  )
}

build_ocaml() {
  require_cmd opam "Install opam before building Forester."
  require_cmd dune "Install dune before building Forester."
  (
    cd "$repo_root"
    opam exec -- dune build @install
  )
}

build_rust() {
  require_cmd cargo "Install Rust and cargo before building the Rust parser."
  (
    cd "$repo_root"
    cargo build --manifest-path tools/rust-parser/Cargo.toml --release
  )
}

build_haskell() {
  if ! command -v cabal >/dev/null 2>&1; then
    warn "cabal is not installed; skipping the Pandoc converter scaffold."
    return 1
  fi

  (
    cd "$repo_root"
    cabal --project-file=tools/pandoc-converter/cabal.project build forester-pandoc:exe:forester-pandoc
  )
}

install_binary() {
  local source_path="$1"
  local target_name="$2"

  if [[ ! -f "$source_path" ]]; then
    warn "Skipping ${target_name}; expected binary not found at ${source_path}."
    return 1
  fi

  mkdir -p "$BINDIR"
  install -m 0755 "$source_path" "$BINDIR/$target_name"
  info "Installed ${target_name} to ${BINDIR}/${target_name}"
}

install_haskell_binary() {
  if ! command -v cabal >/dev/null 2>&1; then
    return 1
  fi

  local binary_path
  binary_path="$(
    cd "$repo_root" &&
      cabal --project-file=tools/pandoc-converter/cabal.project list-bin forester-pandoc:exe:forester-pandoc
  )"
  install_binary "$binary_path" "forester-pandoc"
}

show_help() {
  cat <<'EOF'
Usage: ./scripts/install-local.sh [OPTIONS] [COMMAND]

Commands:
  install     Install local binaries (default)
  build       Build without installing
  deps        Install opam dependencies only

Options:
  --no-deps       Skip opam dependency installation
  --skip-rust     Skip building/installing the Rust parser
  --skip-haskell  Skip building/installing the Haskell converter scaffold
  --prefix DIR    Override installation prefix (default: ~/.local)
  --bindir DIR    Override binary directory (default: PREFIX/bin)
  -h, --help      Show this help text
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    install|build|deps)
      COMMAND="$1"
      shift
      ;;
    --no-deps)
      SKIP_DEPS=true
      shift
      ;;
    --skip-rust)
      SKIP_RUST=true
      shift
      ;;
    --skip-haskell)
      SKIP_HASKELL=true
      shift
      ;;
    --prefix)
      PREFIX="$2"
      BINDIR="$PREFIX/bin"
      shift 2
      ;;
    --bindir)
      BINDIR="$2"
      shift 2
      ;;
    -h|--help)
      show_help
      exit 0
      ;;
    *)
      printf 'Unknown argument: %s\n' "$1" >&2
      show_help >&2
      exit 1
      ;;
  esac
done

case "$COMMAND" in
  deps)
    install_ocaml_deps
    ;;
  build|install)
    if [[ "$SKIP_DEPS" != true ]]; then
      install_ocaml_deps
    fi

    build_ocaml
    if [[ "$COMMAND" == "install" ]]; then
      install_binary "$repo_root/_build/default/bin/forester/main.exe" "forester" || true

      if [[ "$SKIP_RUST" != true ]]; then
        build_rust
        install_binary "$repo_root/tools/rust-parser/target/release/forester-rust-parser" "forester-rust-parser" || true
      fi

      if [[ "$SKIP_HASKELL" != true ]]; then
        build_haskell || true
        install_haskell_binary || true
      fi
    else
      if [[ "$SKIP_RUST" != true ]]; then
        build_rust
      fi

      if [[ "$SKIP_HASKELL" != true ]]; then
        build_haskell || true
      fi

      info "Build completed."
    fi
    ;;
esac
