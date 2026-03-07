# Rust Parser JSON Contract

`forester-rust-parser --json` and the OCaml bridge in
[`lib/parser/Rust_parser.ml`](/home/as_user/Projects/ocaml-forester/lib/parser/Rust_parser.ml)
share a single Rust-side contract definition in
[`tools/rust-parser/src/json.rs`](/home/as_user/Projects/ocaml-forester/tools/rust-parser/src/json.rs).

Contract surface:

- Top-level envelope is a tagged object with `status = "ok"` or `status = "error"`.
- Successful parses carry `document`, whose AST shape is derived from the Rust
  types in `src/ast.rs`.
- Failed parses carry `errors`, each with `message`, `start_offset`,
  `end_offset`, and `report`.
- Node payloads use the same snake_case variant tags that the OCaml bridge
  decodes today.

Schema artifact:

- Checked-in schema: [`tools/rust-parser/json-schema/parse-result.schema.json`](/home/as_user/Projects/ocaml-forester/tools/rust-parser/json-schema/parse-result.schema.json)
- Regenerate with:

```sh
cargo run --manifest-path tools/rust-parser/Cargo.toml -- --json-schema \
  > tools/rust-parser/json-schema/parse-result.schema.json
```

Guardrails:

- `src/main.rs` and `src/ffi.rs` both serialize through `src/json.rs`, so the
  CLI and FFI cannot silently drift to different envelopes.
- `cargo test` verifies the generated schema still matches the checked-in
  contract artifact.
