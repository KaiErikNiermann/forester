# Cleanup Refactor Plan

## Goal

Turn the maintainability scan into a sequenced cleanup backlog that reduces structural risk without stalling active parser and converter work.

This plan deliberately separates:

- high-risk module decomposition that should happen in focused slices,
- medium-risk extraction work that can proceed incrementally,
- lower-risk tooling cleanup that should not block feature work.

## Principles

1. Prefer seam extraction over wholesale rewrites.
2. Preserve behavior first; refactor behind existing tests and parity harnesses.
3. Split by responsibility boundaries, not by arbitrary line count.
4. Add helper modules only when they reduce duplication across at least two call sites or clarify a major boundary.
5. Avoid merge-heavy refactors in churn-heavy files unless there is a clear isolating seam.

## Phase 0: Safety Rails

Before touching the highest-risk modules, keep the existing regression surfaces green:

- OCaml parser tests,
- Rust parser parity and fixture corpus,
- pandoc-converter tests and markdown bridge tests,
- benchmark guardrails where parser performance is affected.

This phase is already mostly in place. The cleanup work should reuse those gates rather than add parallel one-off checks.

## Phase 1: Highest-risk decompositions

### 1. Split `lib/compiler/Eval.ml` by semantic family

Target outcome:

- a smaller `Eval` coordination module,
- extracted helpers for argument/type checking,
- separate semantic clusters for object, datalog, transclusion/subtree, and artefact/job evaluation.

Candidate module seams:

- `Forester_compiler.Eval_args`
- `Forester_compiler.Eval_errors`
- `Forester_compiler.Eval_object`
- `Forester_compiler.Eval_datalog`
- `Forester_compiler.Eval_transclusion`

Why first:

- highest semantic density,
- highest blast radius for future feature work,
- current error handling duplication is already a maintenance liability.

Risk:

- recursion and environment threading are easy to break.
- Keep this as a series of extraction commits, not a single rewrite.

### 2. Split `tools/pandoc-converter/src/Forester/Pandoc.hs`

Target outcome:

- a thin top-level facade module,
- normalization, rendering, provenance, and coverage logic separated into dedicated modules,
- shared rendering helpers replacing duplicated block/inline fallback logic.

Candidate module seams:

- `Forester.Pandoc.Normalize.Metadata`
- `Forester.Pandoc.Normalize.Block`
- `Forester.Pandoc.Normalize.Inline`
- `Forester.Pandoc.Render.Forester`
- `Forester.Pandoc.Render.Markdown`
- `Forester.Pandoc.SourcePos`
- `Forester.Pandoc.Coverage`

Why first:

- largest real source file in the repo,
- already contains duplicated traversal knowledge,
- bridge feature growth will otherwise keep deepening the monolith.

Risk:

- module moves can disturb import cycles and test naming.
- Start by extracting pure helpers with unchanged public API.

### 3. Split `lib/parser/Rust_parser.ml`

Target outcome:

- transport/process logic isolated from JSON decoding,
- AST decoding isolated from public parse entrypoints,
- common parse-failure construction extracted and reused.

Candidate module seams:

- `Forester_parser.Rust_process`
- `Forester_parser.Rust_json`
- `Forester_parser.Rust_ast_decode`
- `Forester_parser.Rust_bridge`

Why first:

- current bridge couples subprocess execution to schema details,
- decoder growth will remain expensive while everything is in one file,
- this module is a key boundary between OCaml and Rust and should stay easy to audit.

Risk:

- avoid changing the JSON contract while restructuring.
- keep public `parse` / `parse_with_mode` entrypoints stable.

## Phase 2: Medium-risk structural cleanup

### 4. `lib/language_server/Document_format.ml`

Plan:

- extract shared command-body rendering helpers,
- split inline rendering from block rendering,
- isolate frontmatter/body layout policy.

Candidate module seams:

- `Forester_language_server.Document_format_render`
- `Forester_language_server.Document_format_layout`

### 5. `lib/frontend/Htmx_client.ml`

Plan:

- split metadata/frontmatter rendering from content rendering,
- isolate TOC and query-result rendering,
- centralize “meta key -> labelled item/link” helpers.

Candidate module seams:

- `Forester_frontend.Htmx_frontmatter`
- `Forester_frontend.Htmx_content`
- `Forester_frontend.Htmx_toc`
- `Forester_frontend.Htmx_query`

### 6. `lib/language_server/Completion.ml`

Plan:

- extract syntax-node completion-kind mapping,
- split completion-source producers from request orchestration,
- make `compute` mostly composition and logging.

Candidate module seams:

- `Forester_language_server.Completion_kind`
- `Forester_language_server.Completion_sources`
- `Forester_language_server.Completion_compute`

### 7. Rust parser internal follow-up split

Plan:

- break parser grammar families into smaller modules,
- break lexer mode/state code into smaller helpers,
- split parser tests by grammar family and diagnostic family.

Candidate module seams:

- `parser/grammar/*.rs`
- `lexer/modes/*.rs`
- `parser/tests/{commands,groups,datalog,recovery}.rs`

This is valuable, but it should follow the bridge cleanup rather than compete with it.

## Phase 3: Shared test and tooling extraction

### 8. Shared fixture/process helpers

Targets:

- `lib/compiler/test/Test_markdown_fixture_conversion.ml`
- `lib/parser/test/Test_rust_parser_json_conversion.ml`
- `tools/pandoc-converter/test/Spec.hs`

Plan:

- extract fixture manifest loading,
- extract subprocess capture helpers where they repeat,
- isolate AST normalization/comparison helpers from individual test files.

### 9. Shell and CI cleanup

Targets:

- `scripts/benchmark-parser-performance.sh`
- `scripts/check-markdown-realworld-corpus.sh`
- `.github/workflows/ci.yml`

Plan:

- move repeated shell helpers to `scripts/lib.sh`,
- reduce repeated JSON scraping and failure-report scaffolding,
- centralize repeated CI path filters or job setup fragments.

This should remain behind the code-structure cleanups unless a script becomes actively painful to maintain.

## Fix now vs defer

Fix now:

- module-boundary cleanups that reduce feature friction in active areas,
- duplicated helper extraction with clear immediate callers,
- test-helper extraction where it lowers future maintenance cost quickly.

Defer:

- generated/bundle cleanup,
- style-only reformatting,
- broad workflow reshuffles with little day-to-day payoff,
- speculative abstraction that does not remove real duplication yet.

## Suggested sequencing

1. `Eval.ml` extraction seam pass
2. `Forester.Pandoc` module split
3. `Rust_parser.ml` bridge split
4. formatter/frontend/completion extractions
5. shared test helper extraction
6. shell/CI deduplication
7. Rust parser internal split follow-up

## Backlog seeding

The immediate shared-module backlog should track at least these implementations:

- evaluator helper extraction,
- pandoc normalize/render/provenance split,
- Rust bridge process/decode split,
- formatter shared render helpers,
- frontend metadata/content split,
- completion source/kind split,
- test fixture/process helper extraction.
