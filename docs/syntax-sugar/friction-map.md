# Construct Friction Map

This map identifies where sugar could help, and where it is likely to cause more trouble than benefit.

## Friction Matrix

| Construct family | Current surface form | Main friction | Plausible sugar direction | Primary guardrail |
| --- | --- | --- | --- | --- |
| Imports / exports | `\import{tree}` / `\export{tree}` | top-level-only restriction is non-local; braced tree names differ from path-style neighbors | optional lighter import declaration surface | must stay top-level-only and preserve clear module boundaries |
| Namespace / open | `\namespace\foo{...}` / `\open\foo/bar` | path-style backslash chaining is dense; spec mismatch around namespace form | more explicit namespace declaration spelling | do not introduce ambiguous namespace-body parsing |
| Dynamic environment | `\put\foo{bar}`, `\put?\foo{bar}`, `\get\foo`, `\alloc\foo` | similar commands split across path-only vs path-plus-body shapes | grouped assignment/reference sugar | must preserve dynamic-scope semantics and path identity |
| Binders | `[x]`, `[~x]` | strictness is encoded opaquely; repeated brackets create noise | clearer binder markers or grouped parameter list sugar | no hidden defaults; strictness must remain explicit |
| `def` / `fun` | `\def\name[x]{...}` / `\fun[x]{...}` | combines command, path, binders, and body in one dense cluster | alternate declaration surface that desugars to existing fun-spec | avoid introducing precedence or arity ambiguity |
| Subtree | `\subtree[addr]{...}` | address lives in bracket syntax that visually resembles binders | clearer subtree header form | preserve address/body distinction and existing optionality |
| Object | `\object[self]{[render]{...}}` | method table syntax is easy to lose inside nested delimiters | more explicit method-list surface | preserve current object/self semantics and method body boundaries |
| Patch | `\patch{target}[self][super]{...}` | target + binders + methods are visually stacked | structured patch header sugar | must not blur target vs binder vs body roles |
| Call | `\call{target}{method}` | simple but verbose for common method dispatch | infix or dot-like call sugar | avoid stealing punctuation that already has grammar roles |
| Datalog | `\datalog{...}` with `?`, `-:`, `#`, `'`, `@` | embedded sublanguage has distinct mental model and token rules | only wrapper-level sugar, if any | do not mix core and datalog token regimes ambiguously |
| XML / xmlns | `\<html>`, `\xmlns:fr{...}` | obscure, lexer-shaped forms | possibly documentation sugar, not syntax sugar | avoid creating alternate XML dialects inside Forester |
| Escapes / special commands | backslash-driven mode switching | user must know when punctuation becomes syntax | likely better diagnostics/docs, not sugar | avoid context-sensitive heuristics |
| Verbatim | herald-based inline verbatim and block mode | high delimiter sensitivity; low discoverability | delimiter-light verbatim wrapper if parser-safe | must preserve unambiguous termination and strong diagnostics |
| Nesting / grouping | `{}`, `[]`, `()`, `#{}`, `##{}` | delimiter overload harms readability before it harms parser performance | syntactic shorthands that reduce bracket count | no hidden grouping or precedence changes |

## High-Value Candidate Zones

These are the surfaces most likely to benefit from sugar exploration:

1. Declaration headers (`def`, `fun`, binder-heavy forms).
2. Object and patch method tables.
3. Namespace/open and dynamic-environment command families.
4. Verbatim ergonomics, if termination remains explicit and deterministic.

## Low-Value or High-Risk Zones

These should be approached very cautiously, or possibly rejected up front:

1. Datalog core token syntax.
2. XML element and namespace syntax.
3. Escape behavior and lexer-mode boundaries.
4. Anything that makes grouping implicit.

## Working Hypotheses

1. The best sugar candidates will likely be declaration-oriented and method-table-oriented, because those currently combine multiple syntactic roles into one visually dense cluster.
2. The worst sugar candidates will likely be punctuation-heavy shortcuts in datalog, XML, or escape-sensitive areas, because they trade a small typing win for a large ambiguity and diagnostics risk.
3. Some painpoints may be better solved by diagnostics, formatter conventions, or docs rather than new syntax.

## Required Follow-Up for Every Candidate

For each future sugar proposal, record:

1. current core form;
2. proposed surface form;
3. exact desugaring rule;
4. ambiguity analysis;
5. error-reporting impact;
6. formatter/LSP/docs impact;
7. backward-compatibility story.
