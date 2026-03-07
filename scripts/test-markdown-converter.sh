#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd cabal "Install cabal to run the Markdown converter scaffold tests."
"$repo_root/scripts/check-markdown-regression-fixtures.sh"

actual_output="$(mktemp)"
diagnostics_output="$(mktemp)"
trap 'rm -f "$actual_output" "$diagnostics_output"' EXIT

(
  cd "$repo_root/tools/pandoc-converter"
  cabal --project-file=cabal.project build forester-pandoc:exe:forester-pandoc >/dev/null
  converter_bin="$(cabal --project-file=cabal.project list-bin forester-pandoc:exe:forester-pandoc)"
  while IFS= read -r fixture_stem || [[ -n "$fixture_stem" ]]; do
    fixture_stem="${fixture_stem%%#*}"
    fixture_stem="$(printf '%s' "$fixture_stem" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
    if [[ -z "$fixture_stem" ]]; then
      continue
    fi
    "$converter_bin" \
      markdown-to-forester "fixtures/markdown/${fixture_stem}.md" >"$actual_output"
    diff -u "fixtures/markdown/${fixture_stem}.forester" "$actual_output"
  done < fixtures/markdown/manifest.txt

  "$converter_bin" \
    markdown-to-forester \
    --diagnostics-format json \
    fixtures/markdown/tables/basic.md \
    >"$actual_output" \
    2>"$diagnostics_output"
  rg '"code":"table-fallback"' "$diagnostics_output" >/dev/null

  if "$converter_bin" \
    markdown-to-forester \
    --strict \
    fixtures/markdown/tables/basic.md \
    >"$actual_output" \
    2>"$diagnostics_output"; then
    printf 'Expected strict markdown conversion to fail when diagnostics are present.\\n' >&2
    exit 1
  fi
)
