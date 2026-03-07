# Rust Parser Source-of-Truth Rules

This document resolves spec-vs-implementation ambiguities for Rust parser parity work.

## Resolution order

When the specification and implementation disagree, use this order:

1. `lib/parser/Lexer.mll`
2. `lib/parser/Grammar.mly`
3. Existing OCaml parser tests
4. `forester_language_spec.md`

Reason: the Rust parser MVP target is OCaml parser parity, not independent reinterpretation of the spec.

## Authoritative rules for parity work

- Comments are lexer-consumed, not preserved AST nodes.
  - The grammar and spec still mention `COMMENT`, but the current OCaml lexer sends `%` into a comment-consuming state instead of emitting a comment token in normal parsing.
- Imports and exports are top-level only.
  - OCaml uses `head_node_or_import` only at `main`; nested `code_expr` and `head_node` do not admit `IMPORT`.
- Keyword recognition is context-bound.
  - `scope`, `put`, `get`, `import`, `export`, `namespace`, `def`, `alloc`, `let`, `fun`, `subtree`, `object`, `patch`, `call`, and `datalog` are only recognized after `\\` enters `Ident_init`.
- Whitespace is semantically relevant to syntax parity.
  - `WHITESPACE` tokens survive into `textual_expr`, `code_expr`, subtree bodies, method bodies, and argument parsing.
- Binders consume `TEXT`, not `IDENT`.
  - `bvar` and `bvar_with_strictness` operate on `TEXT` payloads; lazy strictness is encoded by a leading `~` inside the binder text.
- Verbatim is a lexer responsibility.
  - Inline `\\verb<herald>|...<herald>` and block `\\startverb ... \\stopverb` must be produced as `VERBATIM` before grammar parsing.
- Namespace syntax follows the OCaml grammar.
  - Canonical form is `\\namespace\\path{...}`, not the braced-ident form currently documented in `forester_language_spec.md`.
- Generic commands stay generic.
  - Commands such as `\\title` and `\\p` are ordinary identifiers plus arguments from the parser's perspective; Rust must not special-case them into non-OCaml AST shapes.
- `head_node1` fallback matters.
  - Outside datalog productions, `DX_ENTAILED`, `DX_VAR`, `TICK`, `AT_SIGN`, and `HASH` are converted back into text nodes.
- Subtree addresses use `wstext`.
  - The optional `[addr]` in `\\subtree` may contain mixed `TEXT` and `WHITESPACE`, not just a single identifier token.
- XML forms are lexer tokens.
  - `\\<name>`, `\\<prefix:name>`, and `\\xmlns:prefix` must be tokenized in the lexer, not reconstructed later from generic punctuation.

## Known spec divergences to treat as documentation debt

- The spec documents `\\namespace{ident}{...}`, but the OCaml grammar accepts `\\namespace` followed by `ident` then `{...}`.
- The spec documents `Comment(string)` as a normal AST possibility, but the active lexer implementation consumes comments instead of surfacing them.
- The spec is language-agnostic and therefore hides lexer-mode details that are critical for parity, especially around command dispatch, escapes, and verbatim.

## Process rule

If OCaml grammar or lexer behavior changes, the same change set should update:

- Rust lexer/parser implementation
- parity docs in `docs/rust-parser/`
- any affected differential/parity tests

This repo now enforces the minimum sync rule in CI with `scripts/check-parser-sync.sh`.
The gate watches `lib/parser/Lexer.mll`, `lib/parser/Grammar.mly`, `lib/parser/Parse.ml`,
and `lib/parser/Parse.mli`. If any of those files change, the same change set must also
touch:

- `tools/rust-parser/src/` for the Rust implementation
- `lib/parser/test/` or `tools/rust-parser/tests/` for parity coverage

`docs/rust-parser/` updates remain strongly recommended and are reported, but they do
not currently hard-fail the policy gate.
