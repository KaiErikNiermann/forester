# External Research: Sugar Design, Comparative Grammars, and Failure Modes

This note synthesizes external research relevant to Forester syntax-sugar exploration.

The goal is not to copy another language's notation. It is to identify patterns that improve authoring while preserving determinism, conformance, and diagnostics.

## Cross-Source Principles

### 1. Sugar must reduce ambiguity, not add it

CommonMark is the clearest cautionary example. Its spec exists because original Markdown was not specified unambiguously, which led to implementation divergence and user surprise. The official CommonMark spec explicitly frames unambiguous syntax and executable conformance examples as the remedy.

Implication for Forester:

- do not add sugar unless it has a single parse and a single desugaring target;
- require fixture-level examples for every sugar rule;
- treat ambiguity reduction as a primary requirement, not a nice-to-have.

### 2. Shorthand works best when there is an obvious longhand

Typst and AsciiDoc both show productive shorthand patterns, but in different ways.

- Typst keeps the language organized into explicit modes (`markup`, `math`, `code`) and presents compact surface syntax as a readable shorthand over a more structured underlying model.
- AsciiDoc often provides both shorthand and formal syntax for the same feature, such as `%option` versus `opts=...` for block options.

Implication for Forester:

- good sugar should have an explicit core longhand form;
- authors should be able to learn the sugar by understanding the longhand once;
- docs and diagnostics should be able to refer back to the core form directly.

### 3. Diagnostics depend on preserving source structure through expansion

Racket's syntax-object model is the strongest external evidence here. Its syntax objects carry source-location and binding information specifically so that macro expansion can preserve hygiene and produce useful syntax errors. The macro debugger then exposes source-location and expansion provenance instead of hiding them.

Implication for Forester:

- any sugar layer must preserve source ranges through desugaring;
- expansion should remain inspectable in tests and debug tooling;
- sugar that is easy to expand but hard to diagnose should be rejected.

### 4. Explicit mode boundaries are safer than implicit heuristics

Typst's syntax reference is explicit about its three modes and how authors enter them. That explicitness helps keep compact syntax readable without making the parser guess too much from context.

Implication for Forester:

- sugar should prefer explicit entry markers over context-sensitive guessing;
- avoid sugar that depends on soft whitespace or hidden local context to disambiguate parse mode;
- embedded sublanguages such as datalog and XML should remain clearly fenced.

## Comparative Grammar Benchmarks

### CommonMark

Useful pattern:

- the spec is paired with executable examples and a parsing strategy, which turns syntax decisions into testable behavior rather than folklore.

Relevant lesson:

- CommonMark's history shows that permissive "everything is valid" surface syntax creates interoperability and authoring problems when ambiguity is left unresolved.

What Forester should borrow:

- example-driven conformance for any new sugar;
- explicit precedence and interruption rules where sugar could compete with existing forms.

What Forester should avoid:

- sugar that produces multiple plausible readings with no syntax error and no warning path.

### Typst

Useful pattern:

- built-in syntax for common document elements is intentionally optimized to be pleasant to write and read;
- mode changes are explicit (`#` for code, `$...$` for math, `[..]` back to markup), which keeps sugar compact without blurring parse regimes.

Relevant lesson:

- readable sugar can coexist with strong structure if mode boundaries stay obvious.

What Forester should borrow:

- sugar candidates should be judged partly on visual scanability;
- declaration and reference sugar should feel locally obvious on first read.

What Forester should avoid:

- hidden switching between text and special parsing behavior.

### AsciiDoc

Useful pattern:

- formal and shorthand spellings coexist for some features, which helps both power users and readers;
- block metadata is factored into a recognizable attribute-list surface.

Relevant lesson:

- shorthand can be productive, but too many orthogonal shorthands increase discovery cost and accidental reinterpretation risk.

The Asciidoctor docs explicitly note that a leading bracketed line is treated as a block attribute line; that means text that merely looks like content can change parse interpretation.

What Forester should borrow:

- where sugar exists, retain a formal spelling and document the relationship clearly.

What Forester should avoid:

- introducing new bracket-led surfaces that are easy to confuse with existing grouping or binder syntax.

### Racket / Lisp-family macro practice

Useful pattern:

- sugar is treated as a source-to-source transformation with preserved lexical structure and source locations;
- expansion is inspectable rather than magical.

Relevant lesson:

- extensibility is viable only when expansion provenance is preserved and debuggable.

What Forester should borrow:

- desugaring should be explicit in the spec and test suite;
- future sugar should come with expansion-focused regression tests.

What Forester should avoid:

- opaque rewrites that make user-facing errors point to the wrong place or hide the real core construct.

## Documented Failure Modes and Anti-Patterns

### 1. Ambiguity without explicit failure

CommonMark's motivation section shows the cost directly: if a format has many plausible parses and no syntax-error concept, users get silently divergent outputs.

Forester guardrail:

- every sugar candidate must have a deterministic parse and, if necessary, a warning or hard error for malformed near-miss forms.

### 2. Too many shorthand layers

AsciiDoc demonstrates how expressive shorthand can become dense quickly once style, role, options, anchors, and block context are stacked together.

Forester guardrail:

- cap sugar depth;
- reject any candidate that adds another punctuation layer to already dense constructs unless it removes more complexity than it introduces.

### 3. Hidden desugaring cost

If users cannot easily tell what core construct a sugar form becomes, they cannot predict semantics, formatting behavior, or diagnostics.

Forester guardrail:

- each sugar form must have a one-step explanation into a current core form;
- the formatter, docs, and tests should be able to expose that mapping.

### 4. Diagnostic degradation after expansion

Racket's syntax-object model exists precisely because naive expansion discards the information needed for good error reporting.

Forester guardrail:

- sugar is unacceptable if it weakens source ranges, expected-token messages, or recovery behavior.

### 5. Context-sensitive punctuation overload

Both Forester and AsciiDoc already use punctuation-rich surfaces. Adding more punctuation-driven shorthand in dense zones risks accidental parse-mode collisions.

Forester guardrail:

- prefer sugar that reduces punctuation burden over sugar that merely rearranges punctuation.

## Research-Derived Evaluation Metrics

Use these metrics for candidate scoring:

1. delimiter count reduction relative to the core form;
2. token-count reduction without loss of role clarity;
3. zero new ambiguous parse cases in the grammar and fixture suite;
4. error-location fidelity after desugaring;
5. ease of explaining the sugar in one sentence by reference to an existing core construct;
6. formatter/LSP/doc impact remaining bounded.

## Implications for Forester's Next Research Steps

Strong candidate zones:

- declaration headers and binder-heavy forms;
- object / patch method tables;
- some namespace / environment operations;
- possibly verbatim ergonomics, but only with explicit termination.

Weak or high-risk candidate zones:

- datalog core token syntax;
- XML forms;
- escape behavior and lexer-mode transitions;
- any sugar that makes grouping implicit.

## Sources

- CommonMark Spec 0.31.2: https://spec.commonmark.org/0.31.2/
- Typst Syntax Reference: https://typst.app/docs/reference/syntax/
- Typst home page syntax examples: https://typst.app/
- AsciiDoc Syntax Quick Reference: https://docs.asciidoctor.org/asciidoc/latest/syntax-quick-reference/
- AsciiDoc Blocks reference: https://docs.asciidoctor.org/asciidoc/latest/blocks/
- AsciiDoc Attribute Entries: https://docs.asciidoctor.org/asciidoc/latest/attributes/attribute-entries/
- AsciiDoc Options Attribute: https://docs.asciidoctor.org/asciidoc/latest/attributes/options/
- Racket Guide, Syntax Objects: https://docs.racket-lang.org/guide/stx-obj.html
- Racket Macro Debugger: https://docs.racket-lang.org/macro-debugger/index.html
- CommonMark discussion on warnings for ambiguous inputs: https://talk.commonmark.org/t/warnings-recommendation-system-option/639
