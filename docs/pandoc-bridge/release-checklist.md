# Markdown Bridge Release Checklist

Applies to: `tools/pandoc-converter` and the OCaml markdown bridge integration.

Use this checklist before declaring the Markdown to Forester bridge feature-complete.

## 1. Mapping and Policy Documentation

- [ ] `docs/pandoc-bridge/conversion-policy.md` reflects the current mapping behavior, diagnostics policy, reader profiles, and acceptance criteria.
- [ ] `docs/pandoc-bridge/examples.md` contains representative examples for the supported subset and current fallback behavior.
- [ ] Any new lossy or fallback mapping added since the last release is documented with its diagnostic code and rationale.
- [ ] If provenance support changed, the policy doc mentions `--emit-sourcepos-comments` and the expected `source_span` diagnostic field behavior.

## 2. Fixture and Library Test Gates

Run these commands from the repository root:

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
just test-haskell
just test-markdown-bridge
just test-markdown-corpus
just benchmark-markdown-bridge
```

Release gate:

- [ ] `just test-haskell` passes.
- [ ] `just test-markdown-bridge` passes.
- [ ] `just test-markdown-corpus` passes.
- [ ] `just benchmark-markdown-bridge` passes within the configured thresholds.
- [ ] The manifest-driven fixture corpus is current and regression fixtures follow `tools/pandoc-converter/fixtures/markdown/regressions/README.md`.

## 3. CI and Nightly Gates

- [ ] `.github/workflows/ci.yml` markdown-bridge job is green on the release candidate commit.
- [ ] `.github/workflows/ci.yml` haskell-converter job is green on the release candidate commit.
- [ ] `.github/workflows/markdown-bridge-nightly.yml` most recent scheduled or manually triggered run is green.
- [ ] The nightly real-world corpus artifact report was reviewed for new diagnostics, unexpected corpus growth, or suspicious output changes.

## 4. OCaml Integration Requirements

- [ ] OCaml integration still resolves the converter through `FORESTER_PANDOC_PATH` or `PATH` without ad hoc local changes.
- [ ] The OCaml markdown parser integration tests still cover successful conversion, missing converter, converter failure, and invalid emitted Forester.
- [ ] The OCaml fixture bridge tests still enforce golden outputs, parser validity, hazard checks, fuzz coverage, and Rust parity where parity is expected.
- [ ] No bridge change regressed parser-valid output for the authoritative OCaml parser.

## 5. Diagnostics and Provenance

- [ ] Every lossy or fallback conversion path emits a stable diagnostic code.
- [ ] Machine-readable diagnostics still serialize with `--diagnostics-format json`.
- [ ] If provenance mode is part of the release target, `forester-pandoc markdown-to-forester --emit-sourcepos-comments` emits `% pandoc-sourcepos: ...` comments for GFM/CommonMark input and diagnostics expose `source_span` when available.
- [ ] Strict mode still fails when diagnostics are present.

## 6. Release Sign-off

- [ ] No pending Taskwarrior items remain in project `ocaml-forester.pandoc-bridge` for the targeted release scope.
- [ ] Any intentionally deferred gaps are documented explicitly in the release notes or bridge policy doc.
- [ ] The release commit message and changelog entry describe whether the bridge is MVP-only, beta-ready, or full-release-ready.
