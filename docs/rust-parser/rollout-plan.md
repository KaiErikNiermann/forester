# Rust Parser Backend Rollout Plan

This plan describes how to expose the Rust parser as an optional backend after parity is already proven. It is intentionally a rollout document, not an implementation commitment.

## Goals

- allow controlled selection of OCaml or Rust parser backends
- keep OCaml as the safe fallback while rollout confidence is built
- make backend choice observable in tests and diagnostics
- fail closed when the Rust parser is unavailable or unhealthy

## Non-goals

- changing the default backend before parity and release gates are green
- silently switching users to Rust without an explicit opt-in phase
- introducing telemetry that leaks source content

## Proposed selection surfaces

Add backend selection in descending precedence order:

1. explicit CLI flag, if and where the application exposes parser choice
2. environment variable for developer and CI use
3. config file setting for persistent local/project preference
4. default backend baked into the application build or runtime default

Proposed enum:

- `ocaml`
- `rust-strict`
- `rust-recovery`
- `auto`

Semantics:

- `ocaml`: always use the OCaml parser
- `rust-strict`: require the Rust parser and fail if it is unavailable or unhealthy
- `rust-recovery`: use Rust recovery mode only for diagnostics-oriented entrypoints where partial ASTs are acceptable
- `auto`: prefer Rust only if the rollout gate for that surface is enabled and the health checks pass; otherwise fall back to OCaml

## Recommended initial surfaces

Phase the rollout by caller rather than flipping the entire application at once.

### Phase 0: existing parity gate only

- current state
- Rust is exercised in CI and test harnesses
- end-user parsing still behaves as OCaml-first

### Phase 1: developer opt-in

- expose `FORESTER_PARSER_BACKEND=rust-strict|ocaml`
- use it only in development, local scripts, and targeted CI jobs
- keep default behavior unchanged

### Phase 2: shadow mode in selected workflows

- parse with Rust in the background for selected commands or tests
- keep OCaml result authoritative
- log backend agreement/disagreement and timing without changing user-visible output

### Phase 3: explicit production opt-in

- allow a stable config or CLI flag to choose `rust-strict`
- keep OCaml fallback available via explicit override
- require release checklist sign-off before enabling for wider use

### Phase 4: default switch evaluation

- only consider after sustained green CI, stable diagnostics, and benchmark confidence
- switching the default should be a separate decision with its own task and release note

## Fallback rules

Fallback must be deliberate and narrow.

- `ocaml`
  - never invokes Rust
- `rust-strict`
  - no fallback; failure to spawn, timeout, invalid JSON, or parser error is surfaced as Rust backend failure
- `rust-recovery`
  - no fallback for backend failures; fallback would hide recovery-mode defects
- `auto`
  - may fall back to OCaml only for backend-availability failures or rollout gating decisions, not for normal Rust parse errors

Do not treat a Rust parse error on invalid input as a reason to retry with OCaml automatically. That would hide parity bugs and confuse diagnostics ownership.

## Health checks for `auto`

Before `auto` prefers Rust, all of these should be true:

- Rust parser binary resolves successfully
- `Rust_parser.is_available ()` passes
- parser-sync CI is green on the target branch or release candidate
- JSON contract and schema tests are green
- benchmark gate is green on the maintained corpora

## Telemetry and observability

Any telemetry should be minimal and content-free.

Recommended fields:

- selected backend
- requested backend mode
- whether fallback occurred
- fallback reason class: `unavailable`, `timeout`, `invalid_json`, `gated_off`
- parse outcome class: `parsed`, `recovered`, `error`
- elapsed milliseconds
- parser binary version or commit SHA if available

Do not log raw source text, raw AST JSON, or full diagnostics by default.

## CI implications

When backend selection lands, add dedicated coverage for:

- `ocaml` mode still behaving as current baseline
- `rust-strict` mode requiring the binary and surfacing failures clearly
- `auto` mode falling back only for approved backend-health reasons
- no accidental fallback during parser-sync CI jobs

## Suggested implementation order

1. add a backend selection type in the OCaml-facing parser integration layer
2. thread explicit selection through the caller surfaces without changing defaults
3. add backend-selection unit tests in OCaml
4. add structured logging or counters for backend choice and fallback reason
5. add optional shadow-mode wiring if needed
6. only then evaluate wider enablement

## Exit criteria for rollout readiness

This rollout plan is ready to implement when:

- the Rust parser MVP release checklist is green
- parity and bridge regressions are treated as blocking in CI
- there is a clear owner for fallback semantics and telemetry review
- the first backend-selection surface is chosen explicitly rather than added everywhere at once
