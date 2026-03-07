# Phase 1 Header-Binder Rollout Plan

This document defines the rollout and compatibility controls for phase-1
parenthesized header-binder sugar.

Scope:

- `\def\name(x, ~y){body}`
- `\fun(x, y){body}`
- `\object(self){...}`
- `\patch{target}(self, super){...}`

It does not authorize any broader syntax-sugar wave.

## Release Gate

Do not call phase 1 ready for broad author use unless all of the following are true:

1. OCaml and Rust parsers both accept the supported forms and reject the documented invalid forms.
2. Parser-sync tests cover positive and negative header-binder fixtures.
3. Strict-mode diagnostics for malformed parenthesized headers carry stable structured details and source ranges through the Rust bridge.
4. Recovery mode preserves following nodes after malformed parenthesized headers instead of poisoning the rest of the document.
5. Formatter behavior is explicit and tested: phase-1 sugar normalizes to longhand square-binder syntax.
6. Snippet/documentation surfaces expose the supported sugar forms without implying unsupported extensions such as `\let(x)` or method-entry sugar.
7. The benchmark gate in `./scripts/benchmark-parser-performance.sh --fail-on-regression` is green, including the dedicated header-binder corpora.
8. Release notes and authoring docs clearly state that sugar is accepted on input but not preserved by formatting.

## Backward-Compatibility Controls

Phase 1 must remain compatibility-preserving for existing Forester documents.

- Existing square-binder syntax remains fully supported and remains the formatter’s canonical output.
- No existing valid `[]` document may change meaning because phase-1 sugar exists.
- Mixed header styles inside one construct remain invalid:
  - `\def\f(x)[y]{...}`
  - `\def\f[x](y){...}`
  - `\patch{t}(self)[super]{...}`
- The new comma separator has meaning only inside the new parenthesized header form.
- No repository-wide migration is required to adopt phase 1.
- The release must not silently rewrite user-authored sources except through an explicit formatter invocation.

## Migration Guidance

Recommended author guidance for the phase-1 release:

1. Treat the sugar forms as optional input shorthand, not as a new canonical source format.
2. Use longhand `[]` directly in repositories that expect formatting to be idempotent on binder headers.
3. Use sugar selectively in hand-written examples and drafts where readability improves.
4. Do not mass-convert existing documents to sugar as part of the initial rollout.
5. If a repository adopts sugar in examples or docs, expect formatter output and generated snapshots to normalize back to `[]`.

Recommended release note wording:

- phase 1 adds parser support for parenthesized header binders on `def`, `fun`, `object`, and `patch`
- square-binder syntax remains canonical and unchanged
- formatting currently normalizes sugar back to square-binder form
- unsupported variants such as `\let(x)` remain errors

## Operational Rollout Sequence

Recommended sequence:

### Phase A: parser/tooling complete

- parser support lands in OCaml and Rust
- parity, diagnostics, recovery, formatter, and snippet coverage are green
- documentation is updated

### Phase B: documented availability

- release notes mention the new accepted surface forms
- authoring/spec docs show both sugar and canonical longhand
- no behavior changes beyond parse acceptance and formatter normalization

### Phase C: adoption observation

- watch for user confusion around formatter normalization
- watch for unexpected requests for `\let` parity or method-entry sugar
- watch for evidence that parenthesized headers cause real ambiguity or diagnostics regressions in practice

Only after a stable observation window should Forester consider additional sugar.

## Reconsideration Criteria for Deferred Sugar

Deferred candidates should not be revived just because phase 1 shipped.

Reconsider a deferred sugar candidate only if all of the following are true:

1. There is concrete evidence of repeated author pain that phase 1 did not solve.
2. The candidate has a precise desugaring into an existing core form.
3. The candidate does not introduce precedence, grouping, or whitespace ambiguity.
4. Parser/formatter/LSP/test impact remains bounded and explicitly costed.
5. Benchmarks and pathological-input analysis do not show a disproportionate complexity increase.
6. There is a clear compatibility story for existing documents and formatter behavior.
7. The candidate can be gated and tested independently rather than bundled into a broad sugar wave.

Until those conditions are met, the following remain deferred:

- method-entry sugar inside `object`/`patch`
- verbatim fence sugar
- alternate namespace/import/export declaration forms

The following remain rejected unless new evidence appears:

- datalog token sugar
- XML shorthand variants
- lexer-heuristic escape sugar
- implicit grouping or precedence-affecting sugar
