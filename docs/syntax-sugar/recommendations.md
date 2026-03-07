# Syntax Sugar Recommendations

## Verdict

**GO, but narrowly.**

Sugar is useful enough to justify implementation effort only for a small, structurally disciplined first wave.

The recommended first wave is limited to **parenthesized header binders** for declaration and object-style headers.

Everything else should remain deferred until that smaller experiment proves that sugar can be added without harming determinism, diagnostics, formatter behavior, or parser parity discipline.

## Recommended Phase 1 Scope

Implement only these surface forms:

- `\def\name(x, ~y){body}`
- `\fun(x, y){body}`
- `\object(self){methods}`
- `\patch{target}(self, super){methods}`

Desugar them to the current core grammar:

- `\def\name[x][~y]{body}`
- `\fun[x][y]{body}`
- `\object[self]{methods}`
- `\patch{target}[self][super]{methods}`

## Rejected or Deferred for Now

Reject for now:

- environment synonym sugar (`set/default/use`-style aliases)
- any sugar that merely renames an existing command without reducing structural complexity

Defer for later evaluation:

- method-entry sugar inside object/patch bodies
- verbatim fence sugar
- import/export/namespace alternate declaration forms

Reject outright unless new evidence appears:

- datalog token sugar
- XML shorthand variants
- escape or lexer-mode heuristic sugar
- implicit grouping or precedence-affecting sugar

## Why the Verdict Is Narrow

The current research base supports one strong claim:

- Forester's biggest ergonomic cost is header density around binders and related self/super binders.

It does **not** support a strong claim that broad punctuation or keyword sugar would be safe.

The smallest valuable experiment is therefore the one that:

- reduces delimiter repetition;
- keeps the transformation local;
- preserves an obvious longhand core form;
- introduces no new reserved command words.

## Adoption Guidance

Phase 1 should be gated by the following rules:

1. Both OCaml and Rust parsers must accept the new forms and produce the current AST shape after desugaring.
2. Existing `[]` forms must remain supported unchanged.
3. Mixed or malformed parenthesized binder lists must produce explicit diagnostics.
4. Formatter behavior must be decided up front: preserve user form or normalize intentionally.
5. Parser-sync tests must cover both accepted and rejected edge cases.

Only after a stable Phase 1 should Forester reconsider broader sugar.

The concrete rollout and compatibility rules for that phase live in
`docs/syntax-sugar/rollout-plan.md`.

## Policy for Future Sugar Proposals

No future sugar proposal should be accepted unless it ships with all of the following:

1. explicit problem statement tied to observed authoring pain;
2. exact desugaring spec into existing core grammar;
3. ambiguity and determinism analysis;
4. backward-compatibility statement;
5. performance and pathological-input assessment;
6. error-reporting and source-range mapping plan;
7. formatter/LSP/docs impact review;
8. valid/invalid/recovery test plan.

## Implementation Backlog Trigger

Because the verdict is go for a narrow Phase 1, the follow-up implementation work should be tracked as a dedicated backlog under a syntax-sugar implementation subproject rather than mixed into general parser work.
