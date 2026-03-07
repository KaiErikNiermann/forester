# Phase 1 Header-Binder Sugar Spec

Scope: the first implementation wave for Forester syntax sugar.

This document is the parser contract for phase-1 parenthesized header binders.

## Supported Surfaces

Phase 1 adds only these forms:

- `\def\name(x, ~y){body}`
- `\fun(x, y){body}`
- `\object(self){methods}`
- `\patch{target}(self, super){methods}`

No other command gains parenthesized binders in phase 1.

In particular:

- `\let` does **not** gain parenthesized binders in phase 1;
- method declarations do **not** change;
- namespace/import/export/datalog/XML/verbatim surfaces do **not** change.

## Desugaring Targets

The new forms desugar to the existing core grammar:

- `\def\name(x, ~y){body}` -> `\def\name[x][~y]{body}`
- `\fun(x, y){body}` -> `\fun[x][y]{body}`
- `\object(self){methods}` -> `\object[self]{methods}`
- `\patch{target}(self, super){methods}` -> `\patch{target}[self][super]{methods}`
- `\patch{target}(self){methods}` -> `\patch{target}[self]{methods}`

AST shape must remain unchanged after parsing.

## Arity Rules

### `def`

- accepts zero or more binders in `(...)`
- `\def\name(){body}` is valid and desugars to `\def\name{body}`

### `fun`

- accepts zero or more binders in `(...)`
- `\fun(){body}` is valid and desugars to `\fun{body}`

### `object`

- accepts exactly one binder in `(...)`
- `\object(self){...}` is valid
- `\object(){...}` is invalid
- `\object(a, b){...}` is invalid

### `patch`

- accepts one or two binders in `(...)`
- `\patch{target}(self){...}` is valid
- `\patch{target}(self, super){...}` is valid
- `\patch{target}(){...}` is invalid
- `\patch{target}(a, b, c){...}` is invalid

## Binder Entry Syntax

Each entry inside `(...)` is parsed as a binder item.

- `x` -> strict binder `x`
- `~x` -> lazy binder `x`

Whitespace around entries and commas is ignored.

For `object` and `patch`, entries are plain names, not strictness-bearing binders.

- `\object(self){...}` -> `self = "self"`
- `\patch{target}(self, super){...}` -> `self = "self"`, `super = "super"`
- `\object(~self){...}` preserves the literal name `~self`, matching the existing square-binder behavior

Examples:

- `\def\f(x,y){...}`
- `\def\f( x , ~y ){...}`
- `\fun(\n  x,\n  y\n){...}`

## Rejected Forms

The following must be rejected:

- mixed parenthesized and square binder headers in the same construct
  - `\def\f(x)[y]{...}`
  - `\def\f[x](y){...}`
  - `\patch{t}(self)[super]{...}`
- empty binder entries
  - `\def\f(,x){...}`
  - `\def\f(x,){...}`
  - `\def\f(x,,y){...}`
- binder entries that normalize to the empty string
  - `\def\f(~){...}`
  - `\def\f(   ){...}` for constructs that require at least one binder
- object/patch arity violations
- forms that require parser guesswork about header shape

## Whitespace Policy

Phase 1 keeps the existing header discipline:

- no optional code whitespace is introduced between the construct head and the opening `(`
- examples that remain invalid:
  - `\def\name (x){body}`
  - `\object (self){...}`
  - `\patch{target} (self){...}`

This is deliberate. It preserves deterministic header parsing and avoids introducing a new whitespace-sensitive branch that the square-binder form does not have.

Inside the parenthesized binder list, ordinary whitespace and newlines are allowed around commas and binder entries.

## Compatibility Notes

- Existing `[]` binder syntax remains fully supported.
- The new sugar does not reinterpret existing valid `[]` documents.
- Comma is a separator only inside the new `(...)` binder form. Authors who need a literal comma inside a binder name must continue using the longhand square-binder syntax.

## Testing Requirements

The parser test matrix for phase 1 must include:

- positive cases for each supported construct
- zero-binder cases for `def` and `fun`
- whitespace-heavy and newline-heavy positive cases
- mixed-form rejection cases
- arity rejection cases for `object` and `patch`
- empty-entry and trailing-comma rejection cases
- OCaml/Rust parser-sync parity for all accepted and rejected examples

## Tooling Policy

Phase 1 keeps author-facing sugar and formatter output intentionally distinct.

- Parsers accept the parenthesized header forms listed above.
- The formatter canonicalizes desugared ASTs back to the square-binder core syntax:
  - `\def\name(x, ~y){body}` formats as `\def\name[x][~y]{body}`
  - `\fun(x, y){body}` formats as `\fun[x][y]{body}`
  - `\object(self){...}` formats as `\object[self]{...}`
  - `\patch{target}(self, super){...}` formats as `\patch{target}[self][super]{...}`
- This normalization is deliberate. The AST does not retain source-form provenance for phase-1 sugar, and forcing the formatter to preserve `()` would require new representation state that the current design intentionally avoids.
- Editor snippets should expose both the longhand core forms and explicit phase-1 sugar variants.
- LSP keyword completions remain plain command completions for now; phase 1 does not add parameter-aware snippet completions through the language server.

## Authoring Guidance

- Use the sugar forms when they improve readability while drafting.
- Expect document formatting to rewrite them into longhand `[]` binders.
- Use longhand directly when you want formatting to remain a no-op for binder headers.
