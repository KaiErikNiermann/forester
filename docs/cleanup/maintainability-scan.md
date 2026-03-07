# Maintainability Scan

## Scope

This scan covered OCaml, Rust, Haskell, shell, and repo-tooling code with three goals:

1. identify structural cleanup hotspots that are already slowing feature work,
2. distinguish high-value refactors from generated or low-value churn,
3. seed a concrete backlog instead of producing a vague code-health report.

Method used:

- whole-repo size and churn scan,
- targeted reads of the highest-risk modules,
- spot-checking of function boundaries and duplicated control flow,
- review of large test harnesses and support scripts.

Explicitly excluded from direct refactor scope:

- `bin/forester/theme/forester.js`, which is effectively a generated/vendor-style bundle and should not drive cleanup priorities.

## Findings

### P0: `lib/compiler/Eval.ml`

Primary hotspot. The evaluator core is concentrated into a large mutually-recursive cluster starting around `process_tape`, `eval_tape`, and `eval_node`, with `eval_node` carrying a large semantic dispatch over many unrelated language features.

Why this is a problem:

- command semantics, object semantics, datalog semantics, transclusion, routing, and effectful job emission are interleaved in one recursive unit,
- repeated `Reporter.fatal` / `Type_error` construction makes error handling inconsistent and hard to audit,
- repeated argument extraction (`pop_*`, `extract_*`) is partially abstracted but still scattered across branches,
- the current shape makes it difficult to test or reason about one semantic family without loading the entire evaluator context.

Likely split points:

- argument extraction and type-check helpers,
- object / patch / call evaluation,
- datalog and syndication evaluation,
- transclusion / subtree / asset routing,
- TeX and embed-artefact handling.

### P0: `tools/pandoc-converter/src/Forester/Pandoc.hs`

Largest real source file in the repo at roughly 1.3k LOC. It currently mixes types, option plumbing, metadata normalization, block normalization, inline normalization, diagnostics/coverage, provenance handling, and rendering back to Forester/Markdown.

Why this is a problem:

- `normalizeTopLevelBlocks`, `normalizeBlock`, and `normalizeInline` form one large normalization pipeline with many lossy-fallback branches embedded inline,
- `renderBlock`, `renderBlockAsInline`, `renderInline`, and `renderForesterSubset` mix rendering policy with escape/provenance details,
- coverage collection adds another exhaustive traversal over Pandoc constructors instead of reusing normalized-shape information,
- normalization and rendering concerns are tightly coupled, which makes the bridge hard to extend safely,
- repeated fallback rendering logic is already visible between block and inline rendering paths.

Likely split points:

- normalization modules,
- rendering modules,
- provenance/source-position helpers,
- diagnostics and coverage collection.

### P0: `lib/parser/Rust_parser.ml`

The OCaml bridge to the Rust parser is still carrying too many responsibilities in one file.

Why this is a problem:

- subprocess execution, timeout handling, JSON schema assumptions, AST decoding, error decoding, and public parser entrypoints live together,
- `node_of_json` is a large hand-written decoder with repeated path/body extraction patterns,
- exception-to-parse-error conversion is repeated in the subprocess/bridge path,
- future JSON contract evolution will remain fragile until decoding and transport concerns are separated.

Likely split points:

- process spawning and timeout handling,
- JSON envelope decoding,
- AST node decoding,
- public `Strict` / `Recovery` bridge API.

### P1: `lib/language_server/Document_format.ml`

The formatter file is already past the point where simple local edits are cheap.

Why this is a problem:

- `pp_node` is a long recursive renderer with many repeated `open command -> render body -> close delimiter` branches,
- the `is_simple_content` inline-vs-block decision is duplicated across many command cases,
- `pp_inline_node` duplicates some logic already present in `pp_node`, especially for verbatim and textual constructs,
- frontmatter splitting, inline rendering, and block rendering are still too tightly packed into one module.

Likely split points:

- shared command-body renderer,
- inline renderer vs block renderer,
- frontmatter/body layout policy.

### P1: `lib/frontend/Htmx_client.ml`

The frontend renderer mixes page shell, frontmatter rendering, body rendering, TOC logic, query result rendering, and XML rendering in one file.

Why this is a problem:

- `render_frontmatter` contains a long sequence of meta-key lookups and special-case renderers,
- TOC and query rendering are unrelated concerns but live in the same module,
- route-aware link generation and metadata formatting are mixed with raw HTML construction,
- repeated patterns exist for “lookup metadata key -> turn into labelled item/link”.

Likely split points:

- frontmatter/meta rendering,
- TOC rendering,
- content/body rendering,
- query-result rendering.

### P1: `lib/language_server/Completion.ml`

The completion path still combines syntax classification, completion source assembly, and LSP request plumbing.

Why this is a problem:

- `kind` is a long classifier over syntax nodes that would be clearer as a dedicated mapping module,
- `visible_completions` mixes syntax completions and trie-driven semantic completions,
- `compute` orchestrates request context, logging, forest lookup, completion-type selection, and item materialization in one function,
- large local control flow makes it harder to add or test one completion source in isolation.

Likely split points:

- syntax-node to completion-kind mapping,
- completion-source producers,
- request orchestration / logging.

### P1: Rust parser internals still have large central modules

The Rust parser was already split once, but `tools/rust-parser/src/parser/mod.rs`, `tools/rust-parser/src/lexer/mod.rs`, and `tools/rust-parser/src/parser/tests.rs` remain large enough to justify another cleanup pass.

Why this is a problem:

- parser grammar families still share one large combinator module,
- lexer mode handling remains centralized in one long scanner function cluster,
- parser unit tests are accumulated in one very large file instead of grammar-family files,
- this is a maintainability issue more than a correctness issue now.

Likely split points:

- parser grammar families by construct class,
- lexer token groups / mode families,
- tests by grammar family and diagnostic family.

### P2: Large custom test harnesses need extraction

The repo has several oversized test modules that mix fixture discovery, environment detection, subprocess control, normalization, and assertions.

Files worth fixing:

- `lib/compiler/test/Test_markdown_fixture_conversion.ml`
- `lib/parser/test/Test_rust_parser_json_conversion.ml`
- `tools/pandoc-converter/test/Spec.hs`

Why this is a problem:

- manifest loading and file-path resolution are repeated,
- subprocess execution and capture helpers are reimplemented locally,
- custom assertion/reporting helpers are duplicated instead of being shared,
- fixture-heavy tests are harder to extend when every new case touches setup code.

Likely split points:

- fixture manifest helper,
- subprocess capture helper,
- AST normalization and comparison helper modules.

### P2: Shell tooling is serviceable but drifting toward mini-programs

`scripts/benchmark-parser-performance.sh` is useful and currently correct, but it already contains orchestration, build steps, embedded Python, JSON shaping, threshold evaluation, and reporting.

Additional shell/tooling findings:

- `scripts/benchmark-parser-performance.sh` duplicates `require_cmd`, which already exists in `scripts/lib.sh`,
- `scripts/check-markdown-realworld-corpus.sh` has a long `run_case` failure path with repeated report scaffolding and brittle JSON scraping via `rg`/`sed`,
- `.github/workflows/ci.yml` repeats large path-filter lists and similar setup/build structure across several jobs.

This is not urgent, but if these tools grow further they should move toward shared helpers and data-driven configuration instead of larger one-off scripts/workflows.

## Cross-cutting patterns

These patterns appear in more than one place and are worth tracking as explicit cleanup goals:

- repeated decode patterns for path/body/bindings in the Rust bridge,
- repeated “simple inline body vs multiline block body” rendering branches in formatters/renderers,
- repeated metadata-key lookup and external-link construction in frontend rendering,
- repeated fixture manifest and path-resolution logic in tests,
- repeated local subprocess capture helpers in bridge/test code,
- repeated shell helper logic and duplicated CI path-glob configuration,
- large files where core logic and diagnostics/reporting are interleaved instead of layered.

## Immediate priorities

If cleanup capacity is limited, the highest-value order is:

1. `lib/compiler/Eval.ml`
2. `tools/pandoc-converter/src/Forester/Pandoc.hs`
3. `lib/parser/Rust_parser.ml`
4. `lib/language_server/Document_format.ml`
5. `lib/frontend/Htmx_client.ml`
6. `lib/language_server/Completion.ml`
7. large test harness extraction

## Low-priority / defer

These should stay out of the first cleanup wave:

- generated/theme bundle cleanup,
- style-only reformatting with no structural payoff,
- broad shell rewrites without a clear maintenance win,
- churn-heavy refactors in modules that are about to be replaced anyway.
