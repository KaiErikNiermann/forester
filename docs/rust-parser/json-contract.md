# Rust Parser JSON Contract

`forester-rust-parser --json` and the OCaml bridge in
[`lib/parser/Rust_parser.ml`](/home/as_user/Projects/ocaml-forester/lib/parser/Rust_parser.ml)
share a single Rust-side contract definition in
[`tools/rust-parser/src/json.rs`](/home/as_user/Projects/ocaml-forester/tools/rust-parser/src/json.rs).

Contract surface:

- Top-level envelope is a tagged object with:
  - `status = "ok"` for strict/recovery parses with no diagnostics
  - `status = "recovered"` when recovery mode produced both `document` and
    `errors`
  - `status = "error"` when no document could be produced
- Every envelope now carries `mode = "strict"` or `mode = "recovery"`.
- Successful and recovered parses carry `document`, whose AST shape is derived
  from the Rust types in `src/ast.rs`.
- Failed and recovered parses carry `errors`, each with `message`, `start_offset`,
  `end_offset`, `report`, and a structured `details` object.
- Node payloads use the same snake_case variant tags that the OCaml bridge
  decodes today.
- `details` carries machine-readable diagnostics:
  - `kind`
  - `expected`
  - `found`
  - labeled span contexts in `labels`
  - remediation hints in `notes`

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
