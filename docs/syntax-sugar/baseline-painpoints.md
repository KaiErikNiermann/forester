# Baseline Syntax Painpoints

This baseline is derived from the current Forester language spec, OCaml parser source of truth, Rust-parser parity docs, existing fixtures, and grammar-soundness outputs.

## Summary

The current core syntax is powerful, but it exposes several constructs in a form that is parser-friendly rather than author-friendly.

The dominant pain pattern is not ordinary text or simple command use. It is concentrated in constructs with one or more of these properties:

- path-shaped commands after backslash;
- bracket-heavy binder syntax;
- mixed delimiter nesting across commands, math, and bodies;
- specialized sublanguages embedded inside the core grammar;
- syntax forms whose canonical spelling is not obvious from the spec alone.

## Evidence-Backed Painpoints

### 1. Path-oriented command forms are terse but visually dense

Examples:

- `\open\foo/bar`
- `\get\foo`
- `\alloc\foo`
- `\namespace\foo{...}`

Why this is a painpoint:

- the command name and the identifier path are both introduced by backslash-driven lexer mode changes;
- slash-fragment identifiers are compact, but hard to skim in longer paths;
- users must internalize which constructs take path-style arguments and which take braced text.

Evidence:

- canonical OCaml parity docs explicitly call out command-dispatch and identifier-fragment mode behavior as critical and non-obvious;
- grammar-soundness differential remnants still cluster in nested macro contexts where path-heavy forms participate.

### 2. Binder syntax is compact but cognitively loaded

Examples:

- `\def\name[x][~y]{...}`
- `\fun[x]{...}`

Why this is a painpoint:

- binders encode both name and strictness inside bracketed text;
- `~` strictness is not self-describing to new readers;
- repeated bracket groups create visual noise in larger definitions.

Evidence:

- source-of-truth notes that binder content is plain text, not a richer syntactic form;
- the spec and parser rely on users understanding `[x]` vs `[~x]` as meaningful surface distinctions.

### 3. Module/environment commands are easy to misuse because shapes differ

Examples:

- `\import{tree}` / `\export{tree}` use braced text;
- `\open\prefix` uses path form;
- `\namespace\prefix{...}` is canonical in OCaml, while the language spec also documents a braced-ident form.

Why this is a painpoint:

- related concepts use different argument shapes;
- the spec-vs-parser mismatch around `\namespace` increases author confusion;
- imports are top-level only, which is semantically correct but easy to discover only through failure.

Evidence:

- `docs/rust-parser/source-of-truth.md` explicitly records that the canonical `\namespace` form is not the same as the spec’s braced-ident presentation;
- parser parity work had to codify top-level-only import behavior as a non-obvious rule.

### 4. Object and patch forms are bracket-heavy and difficult to scan

Examples:

- `\object[self]{[render]{...}}`
- `\patch{\get\obj}[self][super]{[render]{...}}`
- `\call{\get\obj}{render}`

Why this is a painpoint:

- self/super binders, method entries, and nested bodies stack multiple delimiters rapidly;
- method tables look unlike ordinary command bodies;
- visually, the difference between method declaration syntax and surrounding grouping is subtle.

Evidence:

- the object-system section of the spec uses examples that are syntactically dense even at minimal size;
- parser parity and grammar-soundness both treat object/patch bodies as high-weirdness surfaces.

### 5. Datalog syntax is expressive but alien relative to the rest of Forester

Examples:

- `\datalog{?X -: {\rel/is-node ?X}}`
- query negatives require `# { ... }` rather than a more free-form shorthand.

Why this is a painpoint:

- datalog introduces a sublanguage with its own tokens (`?`, `-:`, `#`, `'`, `@`);
- the boundary between ordinary Forester nodes and datalog-specific forms is easy to miss;
- users must learn where datalog tokens are meaningful and where they fall back to plain text.

Evidence:

- source-of-truth docs explicitly describe special token fallback outside datalog productions;
- grammar-soundness risk modeling classifies datalog boundaries as high-risk surfaces.

### 6. XML forms are compact but obscure

Examples:

- `\<html>`
- `\<prefix:name>`
- `\xmlns:fr{http://www.forester-notes.org}`

Why this is a painpoint:

- these forms are efficient once learned, but not discoverable;
- namespace declaration syntax is visually unlike most other Forester commands;
- malformed qnames are likely to produce diagnostics that feel lexer-driven rather than intent-driven.

Evidence:

- lexer documentation and parser source-of-truth explicitly require these to be tokenized specially rather than reconstructed later;
- the spec places them in a distinct XML subsystem rather than the more commonly-used command sections.

### 7. Verbatim syntax is powerful but delimiter-herald driven

Examples:

- `\verb<<|asdf<<`
- `\startverb ... \stopverb`

Why this is a painpoint:

- inline verbatim requires users to choose and visually match a herald;
- malformed or near-miss heralds are not easy to spot by inspection;
- block verbatim requires a dedicated mode transition with special termination rules.

Evidence:

- grammar-soundness preflight initially found crash-class failures specifically on malformed verbatim EOF cases;
- even after the crash fix, verbatim remains a designated high-risk zone for parser stress.

### 8. Deep delimiter nesting compounds readability cost quickly

Examples:

- groups inside command bodies;
- math blocks nested inside definitions;
- object or subtree bodies containing additional grouped expressions.

Why this is a painpoint:

- Forester relies heavily on braces, brackets, and parentheses with meaning that varies by context;
- the same visual tokens serve grouping, binders, arguments, method tables, and math payloads;
- human scanning cost rises faster than parser cost.

Evidence:

- grammar-soundness stress probes show parser stability under deep nesting, but the ergonomic burden remains even when performance is fine;
- the remaining differential cases are all nested, delimiter-rich scenarios.

## Preliminary Implication

Sugar, if any, should target the visually dense surfaces above rather than simple commands or ordinary text blocks.

The strongest candidates are likely to be those that:

- reduce delimiter noise;
- make argument shape more explicit;
- preserve the current core grammar as an internal normal form.

The weakest candidates are likely to be those that merely provide alternate spellings for already-clear simple constructs.
