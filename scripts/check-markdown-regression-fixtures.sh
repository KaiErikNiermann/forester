#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

fixtures_root="$repo_root/tools/pandoc-converter/fixtures/markdown"
regressions_root="$fixtures_root/regressions"
manifest_path="$fixtures_root/manifest.txt"

normalize_manifest() {
  sed \
    -e 's/#.*$//' \
    -e 's/^[[:space:]]*//' \
    -e 's/[[:space:]]*$//' \
    "$manifest_path" \
    | rg -v '^$'
}

require_regression_companion_files() {
  local markdown_fixture="$1"
  local stem="${markdown_fixture#"$fixtures_root/"}"
  stem="${stem%.md}"

  local forester_fixture="$fixtures_root/$stem.forester"
  local why_fixture="$fixtures_root/$stem.why.md"

  if [[ ! -f "$forester_fixture" ]]; then
    printf 'Missing regression golden fixture: %s\n' "$forester_fixture" >&2
    exit 1
  fi

  if [[ ! -f "$why_fixture" ]]; then
    printf 'Missing regression note fixture: %s\n' "$why_fixture" >&2
    exit 1
  fi

  if [[ ! -s "$why_fixture" ]]; then
    printf 'Regression note fixture is empty: %s\n' "$why_fixture" >&2
    exit 1
  fi

  if ! normalize_manifest | rg -Fx -- "$stem" >/dev/null; then
    printf 'Regression fixture is not listed in manifest.txt: %s\n' "$stem" >&2
    exit 1
  fi
}

if [[ ! -d "$regressions_root" ]]; then
  exit 0
fi

while IFS= read -r markdown_fixture; do
  require_regression_companion_files "$markdown_fixture"
done < <(
  find "$regressions_root" -type f -name '*.md' \
    ! -name '*.why.md' \
    ! -name 'README.md' \
    | sort
)
