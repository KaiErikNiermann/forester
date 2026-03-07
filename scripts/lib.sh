#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

repo_root="$(cd -- "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

require_cmd() {
  local command_name="$1"
  local install_hint="$2"
  if ! command -v "$command_name" >/dev/null 2>&1; then
    printf 'Missing required command: %s\n%s\n' "$command_name" "$install_hint" >&2
    exit 1
  fi
}

collect_ocaml_files() {
  (
    cd "$repo_root"
    rg --files bin lib test -g '*.ml' -g '*.mli'
  )
}

run_topiary() {
  local mode="$1"
  mapfile -t ocaml_files < <(collect_ocaml_files)
  if ((${#ocaml_files[@]} == 0)); then
    return 0
  fi

  if topiary format --help >/dev/null 2>&1; then
    case "$mode" in
      format)
        (
          cd "$repo_root"
          topiary format "${ocaml_files[@]}"
        )
        ;;
      check)
        local temp_root
        temp_root="$(mktemp -d)"
        trap 'rm -rf "$temp_root"' RETURN
        (
          cd "$repo_root"
          for file in "${ocaml_files[@]}"; do
            mkdir -p "$temp_root/$(dirname "$file")"
            cp "$file" "$temp_root/$file"
          done
        )
        (
          cd "$temp_root"
          topiary format --skip-idempotence "${ocaml_files[@]}"
        )
        local mismatch=0
        for file in "${ocaml_files[@]}"; do
          if ! cmp -s "$repo_root/$file" "$temp_root/$file"; then
            printf 'Formatting mismatch: %s\n' "$file" >&2
            mismatch=1
          fi
        done
        if ((mismatch != 0)); then
          printf 'Run ./scripts/format.sh to format OCaml sources.\n' >&2
          exit 1
        fi
        ;;
      *)
        printf 'Unsupported topiary mode: %s\n' "$mode" >&2
        exit 1
        ;;
    esac
  else
    (
      cd "$repo_root"
      topiary "$mode" -s "${ocaml_files[@]}"
    )
  fi
}
