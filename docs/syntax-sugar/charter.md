# Syntax Sugar Exploration Charter

## Purpose

Evaluate whether Forester should gain additional surface syntax on top of the now-stabilized core grammar.

The baseline assumption is conservative: the existing grammar is the semantic source of truth, and any sugar must desugar cleanly into current constructs without changing meaning, parser determinism, or diagnostic ownership.

## Preconditions

This exploration starts only after:

- grammar-soundness preflight is green;
- core parser backlog is implemented and parity-gated;
- parser-sync, fixture, and regression infrastructure exist.

Those preconditions are now satisfied.

## Objectives

1. Identify concrete authoring painpoints in the current syntax using evidence from the language spec, parser source-of-truth docs, tests, fixtures, and grammar-soundness findings.
2. Determine whether any candidate sugar materially improves readability, writability, or error-locality for real users.
3. Require every sugar candidate to map to an existing core-grammar form via explicit desugaring.
4. Reject candidates that introduce ambiguity, hidden precedence, surprising whitespace sensitivity, or materially worse diagnostics.
5. End with a clear go/no-go recommendation and, only if justified, a follow-on implementation backlog.

## Scope

In scope:

- surface syntax that expands to existing Forester core forms;
- authoring ergonomics for frequently-used constructs;
- parser, lexer, formatter, LSP, and documentation implications of sugar;
- diagnostic quality before and after desugaring;
- backward-compatibility constraints.

Out of scope:

- changing the semantics of the core language;
- replacing the current grammar as the canonical form;
- adding sugar that requires hidden runtime behavior rather than syntactic desugaring;
- introducing context-sensitive syntax that obscures parse determinism;
- adding multiple equivalent spellings without a strong ergonomics case.

## Success Criteria

A candidate sugar is only viable if all of the following are true:

1. It addresses a painpoint observed in current Forester authoring or diagnostics.
2. Its desugared form is already representable in the existing grammar.
3. Ambiguity and precedence interactions are explicitly analyzed.
4. Error messages remain understandable after desugaring, including source-range mapping.
5. Existing documents remain valid and behaviorally unchanged.
6. Tooling impact is bounded and acceptable.

## Required Deliverables

1. Exploration charter.
2. Baseline painpoint inventory.
3. Construct-by-construct friction map.
4. External research on sugar design principles, competing grammars, and failure modes.
5. Candidate taxonomy with explicit desugaring targets.
6. Final recommendations report with go/no-go verdict.
7. Conditional backlog:
   - implementation tasks if verdict is go;
   - deferment criteria if verdict is no-go.

## Evaluation Heuristics

Prefer sugar that is:

- local rather than global;
- mechanically desugarable;
- visually obvious at first read;
- easy to diagnose when malformed;
- aligned with existing Forester idioms instead of importing unrelated syntax wholesale.

Reject sugar that is:

- order-sensitive in surprising ways;
- lexer-mode fragile;
- likely to hide core semantics from users;
- likely to create multiple near-duplicate style dialects.

## Decision Policy

No sugar proposal advances without:

- a written desugaring rule;
- ambiguity and performance analysis;
- diagnostic impact analysis;
- a concrete test plan spanning valid, invalid, and recovery cases.
