#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd opam "Install opam to run OCaml markdown bridge tests."

converter_path="${FORESTER_PANDOC_PATH:-}"
if [[ -z "$converter_path" ]]; then
  if command -v forester-pandoc >/dev/null 2>&1; then
    converter_path="$(command -v forester-pandoc)"
  else
    printf 'Missing converter binary. Set FORESTER_PANDOC_PATH or install forester-pandoc.\n' >&2
    exit 1
  fi
fi

if [[ ! -x "$converter_path" ]]; then
  printf 'Converter binary is not executable: %s\n' "$converter_path" >&2
  exit 1
fi

(
  cd "$repo_root"
  opam exec -- env \
    FORESTER_PANDOC_PATH="$converter_path" \
    FORESTER_PANDOC_REQUIRE_BINARY=1 \
    dune exec lib/compiler/test/Test_markdown_conversion.exe
  opam exec -- env \
    FORESTER_PANDOC_PATH="$converter_path" \
    FORESTER_PANDOC_REQUIRE_BINARY=1 \
    dune exec lib/compiler/test/Test_markdown_fixture_conversion.exe
)
