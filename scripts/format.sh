#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd topiary "Install topiary-cli to format OCaml sources."
require_cmd cargo "Install Rust and cargo to format the Rust parser."

run_topiary format

(
  cd "$repo_root"
  cargo fmt --manifest-path tools/rust-parser/Cargo.toml --all
)
