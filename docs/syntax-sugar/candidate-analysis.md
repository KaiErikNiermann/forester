# Candidate Sugar Analysis

This analysis converts the baseline painpoints and external research into concrete candidate sugar families.

## Candidate Taxonomy

### Candidate A: Parenthesized header binders

Proposed surface forms:

- `\def\name(x, ~y){body}`
- `\fun(x, y){body}`
- `\object(self){methods}`
- `\patch{target}(self, super){methods}`

Core desugaring targets:

- `\def\name[x][~y]{body}`
- `\fun[x][y]{body}`
- `\object[self]{methods}`
- `\patch{target}[self][super]{methods}`

Why this candidate exists:

- binder-heavy headers are the highest-value readability painpoint;
- `(` in these header positions is currently not the canonical binder syntax, so the new form is locally identifiable;
- the sugar reduces bracket repetition without hiding semantics.

### Candidate B: Object/patch method-entry sugar

Possible surface form:

- `\method\render{body}` inside `\object` / `\patch` bodies

Core desugaring target:

- `[render]{body}`

Why this candidate exists:

- method tables are visually dense and bracket-led entries are easy to lose among surrounding groups.

### Candidate C: Environment command sugar

Possible surface forms:

- `\set\name{value}` -> `\put\name{value}`
- `\default\name{value}` -> `\put?\name{value}`
- `\use\name` -> `\get\name`

Why this candidate exists:

- the current `put` / `put?` / `get` family is compact but not especially self-descriptive to new users.

### Candidate D: Verbatim fence sugar

Possible surface form:

- fenced block verbatim notation that desugars to `\startverb ... \stopverb`

Why this candidate exists:

- verbatim is a real authoring painpoint, especially around inline herald choice.

### Candidate E: Module/header sugar

Possible surface forms:

- lighter top-level import/export declarations;
- clearer namespace header surface.

Why this candidate exists:

- module/environment forms currently mix braced and path-style arguments.

## Detailed Analysis

### Candidate A: Parenthesized header binders

#### Desugaring rules

1. `\def\name()` => `\def\name{...}` only if empty binder lists are already valid for that construct; otherwise reject.
2. `\def\name(x1, ..., xn){body}` => `\def\name[x1]...[xn]{body}`
3. `\def\name(~x){body}` => `\def\name[~x]{body}`
4. `\fun(x1, ..., xn){body}` => `\fun[x1]...[xn]{body}`
5. `\object(self){methods}` => `\object[self]{methods}`
6. `\patch{target}(self, super){methods}` => `\patch{target}[self][super]{methods}`

#### Ambiguity and determinism

Low risk.

Reason:

- after `\def\ident`, `\fun`, `\object`, and `\patch{target}`, the parser is already in a header position;
- `(` is currently not the canonical binder syntax at those points;
- the lookahead required is local and explicit.

#### Backward compatibility

Manageable.

- existing `[]` forms remain valid unchanged;
- the main compatibility risk is if any currently-invalid documents rely on those header-position `(` parse failures;
- unlike comma-inside-`[]` sugar, this does not reinterpret currently-valid binder text.

#### Performance impact

Minimal.

- adds one local branch in a few grammar productions;
- no new lexer mode is required if commas and parentheses are already tokenized as ordinary punctuation.

#### Error-reporting impact

Good if implemented carefully.

- malformed binder lists can produce focused diagnostics like "expected binder name after comma" or "expected ')' before body";
- source-range mapping through desugaring is straightforward because each binder maps to a contiguous header segment.

#### Tooling impact

Manageable.

- formatter must choose whether to preserve user style or normalize to one form;
- AST need not change if desugaring happens before/while building existing nodes;
- LSP examples and snippets need updates, but the surface area is bounded.

#### Recommendation

**Recommend for phase-1 implementation.**

This is the best benefit-to-risk candidate.

### Candidate B: Object/patch method-entry sugar

#### Desugaring rule

- `\method\name{body}` => `[name]{body}`

#### Ambiguity and determinism

Moderate risk.

- parse determinism inside object bodies is tractable;
- global compatibility risk is higher because `\method` would become a reserved command name.

#### Backward compatibility

Weak.

- reserving `\method` may conflict with existing user-defined names or macros.

#### Performance impact

Minimal.

#### Error-reporting impact

Potentially good, but only if method-only context is enforced.

#### Tooling impact

Moderate.

- formatter and docs would benefit;
- parser and keyword tables would grow.

#### Recommendation

**Defer.**

The ergonomics gain is real, but command-keyword reservation makes this a worse first candidate than header-binder sugar.

### Candidate C: Environment command sugar

#### Desugaring rules

- `\set` => `\put`
- `\default` => `\put?`
- `\use` => `\get`

#### Ambiguity and determinism

Low parser risk, high semantic/documentation risk.

#### Backward compatibility

Weak to moderate.

- introduces new reserved words;
- duplicates existing concepts rather than clarifying structure.

#### Performance impact

Negligible.

#### Error-reporting impact

Neutral.

#### Tooling impact

Moderate documentation churn for limited ergonomic gain.

#### Recommendation

**Reject for now.**

This creates synonym clutter more than structural improvement.

### Candidate D: Verbatim fence sugar

#### Desugaring rule

- fence-delimited block => `\startverb ... \stopverb`

#### Ambiguity and determinism

High risk.

- verbatim already sits on a sensitive lexer-mode boundary;
- fence recognition risks interacting badly with ordinary text and existing escape rules.

#### Backward compatibility

Potentially acceptable if new fence openers are chosen carefully, but parser/lexer risk dominates.

#### Performance impact

Needs explicit worst-case analysis because delimiter scanning is the relevant hot path.

#### Error-reporting impact

Potentially worse unless fence mismatch messages are excellent.

#### Tooling impact

Would require formatter, syntax-highlighting, and diagnostics updates.

#### Recommendation

**Defer.**

Real authoring value, but not a good first sugar because it touches the most failure-prone area of the lexer.

### Candidate E: Module/header sugar

#### Ambiguity and determinism

Moderate risk.

- imports are top-level only;
- namespace already has spec-vs-parser canonical-form tension;
- new sugar here risks multiplying ways to write module boundaries.

#### Backward compatibility

Moderate.

#### Performance impact

Minimal.

#### Error-reporting impact

Mixed: could improve readability, but top-level-only errors become harder to explain if surface forms proliferate.

#### Tooling impact

Moderate.

#### Recommendation

**Defer.**

Not a first-wave candidate.

## Benefit-vs-Risk Ranking

1. **Candidate A: Parenthesized header binders**
   - benefit: high
   - risk: low to moderate
   - confidence: high
2. **Candidate B: Method-entry sugar**
   - benefit: medium to high
   - risk: moderate
   - confidence: medium
3. **Candidate D: Verbatim fence sugar**
   - benefit: medium
   - risk: high
   - confidence: low to medium
4. **Candidate E: Module/header sugar**
   - benefit: medium
   - risk: moderate to high
   - confidence: medium
5. **Candidate C: Environment synonym sugar**
   - benefit: low
   - risk: moderate
   - confidence: high

## Decision

The evidence supports a narrow implementation path, not a broad sugar wave.

If any syntax sugar proceeds, it should start with **Candidate A only** and treat all other candidates as deferred pending post-implementation reassessment.
