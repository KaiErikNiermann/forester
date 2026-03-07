#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

require_cmd reuse "Install reuse (https://reuse.software/) to validate SPDX metadata."

(
  cd "$repo_root"
  reuse --root . lint
)
