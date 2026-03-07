#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd -- "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
temp_root="$(mktemp -d)"
trap 'rm -rf "$temp_root"' EXIT

build_bindir="$temp_root/build-bin"
install_bindir="$temp_root/install-bin"

printf '[test-install-local] verifying build mode does not install binaries\n'
(
  cd "$repo_root"
  PREFIX="$temp_root/build-prefix" \
  BINDIR="$build_bindir" \
  ./scripts/install-local.sh build --no-deps --skip-rust --skip-haskell
)

if [[ -e "$build_bindir/forester" ]]; then
  printf 'build mode unexpectedly installed forester to %s\n' "$build_bindir/forester" >&2
  exit 1
fi

printf '[test-install-local] verifying install mode installs forester into requested bindir\n'
(
  cd "$repo_root"
  PREFIX="$temp_root/install-prefix" \
  BINDIR="$install_bindir" \
  ./scripts/install-local.sh install --no-deps --skip-rust --skip-haskell
)

if [[ ! -x "$install_bindir/forester" ]]; then
  printf 'install mode did not install forester to %s\n' "$install_bindir/forester" >&2
  exit 1
fi

printf '[test-install-local] ok\n'
