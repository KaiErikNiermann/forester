# Rust Parser AST Parity

The Rust AST in
[`tools/rust-parser/src/ast.rs`](/home/as_user/Projects/ocaml-forester/tools/rust-parser/src/ast.rs)
is intended to remain a 1:1 serialization counterpart for the OCaml bridge
decoder in
[`lib/parser/Rust_parser.ml`](/home/as_user/Projects/ocaml-forester/lib/parser/Rust_parser.ml).

Guardrail:

- [`tools/rust-parser/tests/ast_json_shape.rs`](/home/as_user/Projects/ocaml-forester/tools/rust-parser/tests/ast_json_shape.rs)
  constructs every `Node` variant, round-trips it through `serde_json`, and
  checks the emitted `type` tag plus field set against the names the OCaml
  bridge decodes.

This is the contract the bridge relies on:

- variant tags stay `snake_case`
- field names stay stable
- object, patch, datalog, XML, and binding shapes do not drift

If a Rust-side AST serialization change is intentional, update the OCaml bridge
decoder and this test in the same commit.
