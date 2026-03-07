#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 The Forester Project Contributors
# SPDX-License-Identifier: GPL-3.0-or-later

from __future__ import annotations

import json
import sys
from collections import Counter
from pathlib import Path
from typing import Any


def load_diagnostics(path: str) -> list[dict[str, Any]]:
    diagnostics_path = Path(path)
    if not diagnostics_path.exists() or diagnostics_path.stat().st_size == 0:
        return []
    with diagnostics_path.open("r", encoding="utf-8") as handle:
        data = json.load(handle)
    if not isinstance(data, list):
        raise SystemExit("expected diagnostics JSON array")
    return [entry for entry in data if isinstance(entry, dict)]


def main() -> int:
    if len(sys.argv) != 2:
        raise SystemExit("usage: json_diagnostic_summary.py PATH")
    diagnostics = load_diagnostics(sys.argv[1])
    counts = Counter(
        str(code)
        for entry in diagnostics
        if (code := entry.get("code")) is not None
    )
    summary = ", ".join(f"{code}:{count}" for code, count in sorted(counts.items()))
    print(f"{len(diagnostics)}\t{summary or 'none'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
