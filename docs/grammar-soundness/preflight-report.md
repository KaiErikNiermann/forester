# Forester Grammar Soundness Preflight Report

Date: 2026-03-07
Scope: `ocaml-forester.grammar-soundness` tasks `147`-`161`.

## Task 147 - Methodology

This preflight uses a combined methodology to avoid blind random testing:

1. Grammar-driven generation: derive valid and near-valid cases directly from `lib/parser/Grammar.mly` and mode rules in `lib/parser/Lexer.mll`.
2. Mutation-driven adversarial testing: mutate known-valid seeds with syntax-aware operators (delimiter, keyword, binder, and mode-boundary perturbations).
3. Differential testing: compare parser outcomes across OCaml parser (source of truth) and Rust parser (in-progress implementation) by acceptance/rejection, normalized AST class, and error class.
4. Metamorphic testing: apply semantics-preserving transforms (whitespace/comment normalization, equivalent grouping rewrites) and assert invariants.
5. Robustness/performance stress: deep nesting, long verbatim/comment blocks, and delimiter-pathological inputs to expose nontermination or excessive resource use.

## Task 148 - Syntactic Construct Inventory

### Lexer Modes

- `Main`
- `Ident_init`
- `Ident_fragments`
- `Verbatim`

### Token Families

- Structural delimiters: `{`, `}`, `[`, `]`, `(`, `)`
- Math delimiters: `#{`, `##{`
- Identifier/path operators: `\\`, `/`
- Datalog operators: `-:`, `?var`, `'`, `@`, `#`
- Keywords after backslash: `scope`, `put`, `put?`, `get`, `import`, `export`, `namespace`, `open`, `def`, `alloc`, `let`, `fun`, `subtree`, `object`, `patch`, `call`, `datalog`
- XML forms: `\\<name>`, `\\<prefix:name>`, `\\xmlns:prefix`
- Textual: `TEXT`, `WHITESPACE`, `VERBATIM`

### Grammar Construct Groups

- Top-level: `main`, `head_node_or_import`
- Binding/function: `binder`, `fun_spec`, `def`, `let`, `fun`
- Dynamic env: `scope`, `put`, `default`, `get`, `alloc`, `open`, `namespace`
- Grouping/math: braces/squares/parens, inline/display math
- Object model: `object`, `patch`, `method_decl`, `call`
- Module/import: `import`, `export`
- XML declarations/elements
- Datalog: `dx_rel`, `dx_term`, `dx_prop`, `dx_sequent`, `dx_query`

## Task 149 - Risk Model (High- Weirdness Surfaces)

Risk tiers:

- Critical:
  - Mode transitions (`Main <-> Ident_init <-> Ident_fragments <-> Verbatim`)
  - Delimiter matching/nesting across mixed constructs (`#{}`, `##{}`, `{}`, `[]`, `()`)
  - Verbatim herald termination edge cases
- High:
  - Escape/special-name sequences after backslash
  - Datalog syntax boundaries with non-datalog text
  - Object/patch binders and nested method bodies
- Medium:
  - XML identifier forms and namespace declaration tokens
  - Long path fragments and unusual identifier characters
  - Comment/newline continuation behavior

## Task 150 - Correctness Oracles and Failure Taxonomy

Primary oracles:

1. Acceptance oracle: known-valid corpus parses successfully.
2. Rejection oracle: known-invalid corpus fails with diagnostic.
3. Differential oracle: OCaml vs Rust parser parity on acceptance/error class where both claim support.
4. Stability oracle: transformed equivalent inputs preserve parse class/invariants.

Failure taxonomy:

- Crash/assertion failure
- Hang/nontermination/timeout
- Excessive memory/CPU usage
- False accept (invalid input accepted)
- False reject (valid input rejected)
- Misleading diagnostics (wrong locus or class)
- Cross-parser divergence

## Task 151 - Minimal Valid Seed Corpus Design

Seed families (canonical + adversarial valid for each):

- Basic text/whitespace and mixed groups
- Escaped specials after backslash
- All keyword commands with minimal valid arguments
- Binder variants (`[x]`, `[~x]`, mixed multi-binder)
- Subtree with and without address
- Object and patch with minimal method tables
- Call command forms
- XML element and namespace declaration
- Datalog sequent/query variants
- Verbatim inline and block variants
- Deeply nested but valid delimiter constructions

## Task 152 - Minimal Invalid Corpus Matrix

Malformed variants per construct class:

- Delimiter errors: missing close, extra close, cross-typed close
- Path/identifier errors at mode boundaries
- Broken binder syntax and malformed strictness markers
- Invalid/malformed verbatim herald or unterminated block verbatim
- Datalog malformed entailment/query separators
- Object/patch malformed method entries and binder combinations
- XML malformed qname/prefix patterns
- Invalid comment/escape boundary scenarios

## Task 153 - Grammar-Based Fuzzing Campaign Design

Design:

- Generator constrained by grammar productions (weighted expansion)
- Depth controls for nesting and recursive growth
- Risk-target weighting for high/critical surfaces
- Budgeted shards:
  - High-validity generation
  - Near-boundary generation
  - Delimiter-heavy generation

Stop conditions:

- N unique failures or timeout budget exhausted.

## Task 154 - Mutation Fuzzing Campaign Design

Mutation operators:

- Delimiter insert/delete/substitute
- Keyword swap and arg-shape perturbation
- Binder strictness flips and bracket perturbations
- Path fragment split/merge
- Datalog operator/token perturbation
- Comment/newline and whitespace normalization perturbation
- Verbatim herald perturbation and near-match termination cases

## Task 155 - Differential Testing Design

Comparator dimensions:

- Parse outcome class: success vs failure
- Error category mapping: unexpected token, EOF/unclosed delimiter, lexer-like invalid lexeme
- Normalized AST envelope (top-level constructor sequence, ignoring location data)

Policy:

- OCaml parser remains source of truth for current grammar behavior.
- Rust divergence logged as parity issue (unless spec/grammar reconciliation task explicitly overrides).

## Task 156 - Metamorphic Test Design

Transformations that should preserve parse class:

- Whitespace compaction/expansion in safe textual zones
- Equivalent newline/comment placements
- Re-grouping with semantically equivalent wrapper placement where grammar permits
- Idempotent normalization of repeated neutral whitespace around separators

Negative metamorphics:

- Deliberately non-equivalent transforms should flip parse or AST class predictably.

## Task 157 - Lexer-Mode Transition Stress Design

Focus set:

- `Main -> Ident_init` via backslash on edge characters
- `Ident_init -> Ident_fragments` via slash-heavy paths
- `Ident_init/Main -> Verbatim` and return paths
- Comment behavior around mode exits/newlines
- Escapes adjacent to mode transitions

## Task 158 - Worst-Case Performance Stress Design

Scenarios:

- Very deep balanced nesting
- Long runs of text/path fragments
- Large verbatim payloads with near-miss herald suffixes
- Pathological delimiter alternation patterns

Metrics:

- Parse wall time
- Peak memory proxy (RSS in runner where available)
- Timeout hit rate

## Task 159 - Resource-Safety Checks

Safety checks:

- Timeout thresholds per input size bucket
- Stack depth guard scenarios (deep recursion)
- Memory growth thresholds under stress suites
- Denial-of-service style pattern catalog and expected protective behavior

## Task 160 - Triage/Minimization Workflow

Workflow:

1. Reproduce failure with seed and metadata.
2. Minimize input while preserving failure class.
3. Classify root cause (lexer mode, grammar production, diagnostic mapping, performance).
4. Assign severity (`critical`, `high`, `medium`, `low`).
5. Promote minimized case to permanent regression fixture.
6. Link to remediation task and verification criteria.

## Task 161 - Campaign Execution and Findings

### Executed in this preflight pass

- Source-level grammar/lexer audit of `Grammar.mly`, `Lexer.mll`, `Parse.ml`, and Rust parser/lexer parity implementation files.
- Existing parser tests:
  - OCaml parser test suite (command to run recorded below)
  - Rust parser test suite (command to run recorded below)
- Focused parser robustness probes (documented in findings).

### Findings (Current)

1. OCaml parser has an explicit TODO in error handling path (`lib/parser/Parse.ml`) indicating non-recovery behavior is currently intentional but incomplete for multi-error progression.
2. Rust parser differential risk has narrowed substantially; the latest campaign reports `8 / 1040` divergences, concentrated in nested `def` bodies inside grouped/math/scope/subtree contexts.
3. COMMENT token exists in grammar but OCaml lexer consumes comments without emitting COMMENT tokens in normal flow; this should be tracked as a grammar/lexer representational asymmetry risk during differential testing.
4. High-risk zones confirmed for campaign prioritization: mode transitions, verbatim herald matching, mixed delimiter nesting, datalog boundaries.

### Current go/no-go recommendation

- Recommendation: **GO with conditions**.
- Conditions:
  1. Execute designed fuzz/differential/metamorphic campaigns with automated harness before un-gating downstream implementation backlogs.
  2. Track and close any critical/high findings from those campaigns before declaring grammar-soundness complete.

## Verification Commands

- OCaml parser tests: `opam exec -- dune runtest lib/parser/test`
- Rust parser tests: `cargo test --manifest-path tools/rust-parser/Cargo.toml`
- Repository formatting/linting entrypoints used by project:
  - `just fmt-check`
  - `just lint`

### Verification Results (This Pass)

- `opam exec -- dune runtest lib/parser/test`: passed (exit code 0).
- `opam exec -- dune build @install`: passed (exit code 0).
- `cargo test --manifest-path tools/rust-parser/Cargo.toml`: passed.
- `cargo fmt --manifest-path tools/rust-parser/Cargo.toml -- --check`: passed.
- `cargo clippy --manifest-path tools/rust-parser/Cargo.toml --all-targets --all-features -- -D warnings`: passed.
- `just fmt-check`: failed in this environment because `topiary` is not installed (`scripts/check-format.sh` dependency check).

## Execution Campaign Results (Actual Runs)

Artifacts:

- `docs/grammar-soundness/campaign-results.json`
- `tools/grammar-soundness/run_campaigns.py`
- `tools/grammar-soundness/ocaml_parse_check.ml`

Campaign configuration:

- Random seed: `42`
- Time limit per parse: `3.0s`
- Generated grammar-based cases: `500`
- Mutation cases: `500`
- Differential pool size: `1040`

### Fuzzing and Mutation Outcomes

- Valid seed corpus (`25`): `25 ok`, `0 error`, `0 timeout`, `0 crash`
- Invalid corpus (`15`): `15 error`, `0 timeout`, `0 crash`
- Grammar-generated corpus (`500`): `392 ok`, `108 error`, `0 timeout`, `0 crash`
- Mutation corpus (`500`): `173 ok`, `327 error`, `0 timeout`, `0 crash`

### Differential Outcomes (OCaml vs Rust Parser)

- Divergences: `8 / 1040` cases.
- Remaining divergences cluster in nested macro/grouping scenarios such as `\def` bodies embedded inside math, grouped text, `\scope`, and `\subtree`, not broad command-surface gaps.
- This confirms the previously identified parity risk but does not by itself indicate OCaml grammar unsoundness.

### Metamorphic Outcomes

- Metamorphic pairs tested: `60`
- Invariant breaks: `0`

### Performance and Resource Outcomes

Deep-nesting probes (`64` through `1024` balanced groups) all returned `ok`:

- length `133`: ~`2.395ms`
- length `261`: ~`2.466ms`
- length `517`: ~`2.651ms`
- length `1029`: ~`2.988ms`
- length `1541`: ~`3.301ms`
- length `2053`: ~`4.936ms`

Large verbatim probes (all `ok`):

- ~`16.9k` chars: ~`5.627ms`
- ~`70.9k` chars: ~`16.397ms`

No timeout, nontermination, or crash-class outcome was observed in the executed stress ranges.

## Triage and Minimization Results

Two reproducible crash-class failures were found on malformed verbatim input, both in OCaml parser path:

1. Seed: `\\verb<<|unterminated`
   - Minimized repro: `\\verb<|`
   - Failure: uncaught exception `Failure(\"lexing: empty token\")`
2. Seed: `\\startverb\\nmissing stop`
   - Minimized repro: `\\startverb`
   - Failure: uncaught exception `Failure(\"lexing: empty token\")`

Severity classification:

- `high`: malformed verbatim inputs can crash parser process instead of returning structured diagnostic.

Regression promotion:

- Add minimized repros to grammar-soundness regression fixture set for mandatory re-check.

## Updated Go/No-Go Recommendation

- Recommendation: **NO-GO** for declaring grammar-soundness complete.
- Blocking reason: high-severity crash-class failures exist for malformed verbatim forms.
- Exit criteria to reach GO:
  1. parser returns structured diagnostics (not uncaught exception) for `\\verb<|` and `\\startverb`;
  2. regression fixtures for these cases are enforced in automated runs;
  3. campaigns rerun with zero crash-class outcomes.

## Follow-up: Verbatim Crash Fix (2026-03-07)

Implemented remediation for the crash-class verbatim failures:

- `lib/parser/Lexer.mll` now handles `eof` in `verbatim` mode by raising `Lexer.SyntaxError "unterminated verbatim"` instead of triggering an uncaught `Failure("lexing: empty token")`.
- `lib/parser/test/Test_parser.ml` now includes regression coverage for unterminated inline and block verbatim forms:
  - `\\verb<|`
  - `\\startverb`

Reverification:

- Direct repro check via `tools/grammar-soundness/ocaml_parse_check.exe`:
  - `\\verb<|` -> `error`
  - `\\startverb` -> `error`
- Parser tests (`_build/default/lib/parser/test/Test_parser.exe`): pass, including new regression case.
- Campaign rerun (`tools/grammar-soundness/run_campaigns.py --generated 500 --mutations 500 --timeout 3.0`):
  - Invalid corpus: `15 error`, `0 crash`
  - Generated corpus: `0 crash`
  - Mutation corpus: `0 crash`

Updated recommendation:

- Grammar-soundness preflight crash blocker is resolved for verbatim EOF cases.
- **GO** to proceed past preflight gate, while continuing to track the remaining `8` OCaml-vs-Rust differential divergences as parser parity backlog items.
