# Rust Parser Parity Matrix

Scope: authoritative parity planning for `tools/rust-parser` against the current OCaml parser.

Authoritative sources:

1. `forester_language_spec.md` for language intent and broad user-facing semantics.
2. `lib/parser/Lexer.mll` for actual tokenization, mode transitions, comment handling, verbatim handling, and lexer error behavior.
3. `lib/parser/Grammar.mly` for accepted structure, node placement rules, and AST shape.
4. Existing OCaml parser tests for edge-case expectations.

Status legend:

- `pass`: current Rust implementation appears structurally aligned.
- `partial`: some implementation exists, but it is observably weaker or shape-incompatible.
- `fail`: not implemented, clearly divergent, or currently modeled in the wrong layer.

## Matrix

| Area | OCaml-authoritative behavior | Current Rust status | Notes |
| --- | --- | --- | --- |
| Main document entry | `main := ws_list(locate(head_node_or_import)) EOF` | `partial` | Rust parses repeated nodes to EOF, but does not model `ws_list` or top-level `head_node_or_import` semantics faithfully. |
| Horizontal whitespace | Lexer emits `WHITESPACE` tokens that can survive into `textual_expr` and `code_expr` | `fail` | Rust lexer globally skips `[ \t]+`, so whitespace-sensitive parity is impossible today. |
| Newlines | Lexer emits `WHITESPACE` tokens for newlines and updates locations | `fail` | Rust tracks newlines internally but drops them from the token stream. |
| Comment handling | `%` comments are consumed in the lexer and do not survive as normal AST nodes | `fail` | Rust emits `Comment` tokens/nodes, which is not what OCaml parsing currently does. |
| Backslash command entry | `\\` pushes lexer into `Ident_init`; keyword recognition is context-dependent | `fail` | Rust emits a generic `Backslash` token and recognizes keywords globally. |
| Keyword recognition | `scope`, `put`, `get`, `import`, etc. are only keywords in `Ident_init` | `fail` | Rust tokenizes keyword strings everywhere. |
| Generic command identifiers | `ident` is slash-separated `IDENT` fragments after command lexing | `partial` | Rust has path parsing, but because lexing is global it does not mirror OCaml mode behavior. |
| Special-name escapes after backslash | OCaml supports escaped `%`, space, `\\`, `,`, `"`, `` ` ``, `_`, `;`, `#`, `{`, `}`, `[`, `]`, `|` | `partial` | Rust supports a subset through `EscapedText`; space/comma/quote/backtick/underscore/semicolon are missing. |
| Inline verbatim | `\\verb<herald>|...<herald>` is lexed in `Ident_init`, with herald matching and trimming | `fail` | Rust only tokenizes `verb` as a keyword; no herald-driven lexer mode exists. |
| Block verbatim | `\\startverb ... \\stopverb` is lexed as one `VERBATIM` token | `fail` | Rust tokenizes `startverb`/`stopverb` separately and has no block-verbatim collection mode. |
| XML tokens | `\\<name>`, `\\<prefix:name>`, and `\\xmlns:prefix` are lexer-level tokens | `fail` | Rust only exposes `<`, `>`, and `:` tokens; there is no XML token parity. |
| Datalog tokenization | `-:`, `?var`, `#`, `'`, `@` are context-sensitive with grammar fallback outside datalog | `fail` | Rust tokenizes pieces globally and does not implement OCaml's `head_node1` fallback semantics. |
| Text nodes | `TEXT` and `WHITESPACE` both participate in `textual_node` | `fail` | Rust promotes `Ident`, punctuation, and some specials into text ad hoc, but without OCaml whitespace semantics. |
| Group delimiters | Braces, squares, parens, inline/display math groups parse into generic AST group/math nodes | `pass` | This is one of the few areas that is broadly aligned structurally. |
| `def` / `let` | `\\def\\name[binder]*{arg}` and `\\let\\name[binder]*{arg}` | `partial` | Basic parser support exists, but binder lexing/parsing does not match OCaml `TEXT`-driven binder rules. |
| `fun` | `\\fun[binder]*{arg}` | `fail` | Present in OCaml grammar; not implemented in Rust parser. |
| `scope` | `\\scope{arg}` | `partial` | Basic support exists, but surrounding whitespace/token semantics diverge. |
| `put` / `put?` / `get` / `alloc` / `open` | Dedicated OCaml AST nodes | `fail` | Not implemented as dedicated Rust parser constructs. |
| `namespace` | OCaml grammar is `\\namespace` followed by `ident` and then `{code_expr}` | `fail` | Rust does not implement it, and the spec currently documents a different braced-ident shape. |
| `import` / `export` | Only legal through top-level `head_node_or_import` | `partial` | Rust parses import/export nodes, but not with the OCaml top-level-only restriction. |
| `subtree` | `\\subtree[option(wstext)]{ws_list(head_node)}` | `partial` | Rust supports optional address and body, but only for simple text/ident address parsing, not full `wstext`. |
| Object system | `object`, `patch`, `call` with OCaml method/body grammar | `fail` | AST types exist, parser support does not. |
| Datalog grammar | `dx_rel`, `dx_term`, `dx_prop`, `dx_sequent`, `dx_query` | `fail` | AST types exist, parser support does not. |
| `title` / `p` bridge commands | OCaml treats them as generic identifiers plus normal braced args | `fail` | Rust parser special-cases them into synthetic `Group` shapes. |
| Hash identifiers | `#name` token and node | `pass` | Token and AST path are present. |
| Comments in AST | Spec/grammar mention `Comment`, but actual OCaml parser path consumes comments in the lexer | `fail` | Rust currently exposes comment nodes; parity requires matching OCaml behavior, not the stale grammar branch. |
| Accurate source spans | OCaml locations are real lexer/parser ranges | `fail` | Rust currently fabricates line/column as line 1 from byte offsets in `make_span_from_range`. |
| Error model | OCaml lexer/parser raise syntax errors at lexer-mode boundaries and delimiter failures | `partial` | Rust has structured error types and ariadne output, but the triggering behavior is not OCaml-equivalent yet. |

## Immediate planning implications

- The first parity milestone is a lexer rewrite, not parser polish.
- `tools/rust-parser/src/lexer.rs` and `tools/rust-parser/src/parser.rs` are currently coupled to a global-token model that does not match the OCaml architecture.
- Any parity claim must explicitly exclude whitespace, comments, verbatim, XML, datalog, object system, and top-level import restrictions until those rows move out of `fail`/`partial`.
