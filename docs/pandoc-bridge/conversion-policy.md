# Pandoc-to-Forester Conversion Policy

Date: 2026-03-07  
Applies to: `tools/pandoc-converter`

## 1. Scope and Milestones

### MVP (implemented in this batch)

- Parse Markdown through Pandoc with explicit reader extensions.
- Normalize Pandoc `Block`/`Inline` nodes into a typed intermediate representation.
- Emit deterministic Forester output.
- Support core structural conversions:
  - headings
  - paragraphs/plain blocks
  - bullet/ordered lists
  - block quotes
  - inline emphasis/strong/code/link/image/math/footnotes
- Emit structured diagnostics for lossy or unsupported mappings.
- Fail in strict mode when diagnostics are present.

### Out of Scope for current MVP

- Full semantic table conversion to native Forester table primitives.
- Full `Figure` and caption semantics parity.
- Rich citation semantics beyond textual preservation.
- Full style-preserving ordered-list styles/delimiters.

## 2. Pandoc IR Notes

### `Attr`

- Shape: `(id, classes, key/value attrs)`.
- Current policy:
  - For `CodeBlock`, first class is preserved as language marker via `\code-language{...}` inside `\pre{...}`.
  - Identifier/extra classes/key-value attrs currently emit a lossy diagnostic.

### `ListAttributes`

- Shape: `(start, numberStyle, numberDelim)`.
- Current policy:
  - preserve `start` by explicit item prefixes in emitted `\ol` list items.
  - `numberStyle` and `numberDelim` are normalized (lossy), with diagnostics.

### Metadata

- Pandoc metadata exists in `Pandoc Meta [Block]`.
- Implemented metadata mapping:
  - `title` -> `\title{...}`
  - `date` -> `\date{...}`
  - `author`/`authors` -> repeated `\author{...}`
  - `contributor`/`contributors` -> repeated `\contributor{...}`
  - `tag`/`tags` -> repeated `\tag{...}`
  - `taxon` -> repeated `\taxon{...}`
  - unknown keys -> `\meta{key}{value}`
- Title precedence rule:
  - Metadata `title` wins over body `#` heading.
  - Additional level-1 headings are emitted as `\section{1}{...}` with a diagnostic.

## 3. Reader Options / Extensions

Configured in `markdownReaderOptions`:

- `Ext_fenced_code_blocks`
- `Ext_backtick_code_blocks`
- `Ext_footnotes`
- `Ext_definition_lists`
- `Ext_citations`
- `Ext_yaml_metadata_block`
- `Ext_tex_math_dollars`
- `Ext_raw_html`
- `Ext_raw_tex`
- `Ext_raw_attribute`

Configured in `gfmReaderOptions` (extends markdown profile):

- `Ext_pipe_tables`
- `Ext_task_lists`
- `Ext_strikeout`
- `Ext_autolink_bare_uris`

Rationale: cover common Markdown + GFM-like constructs needed for bridge viability.

### Provenance Mode

- Optional provenance emission is enabled through `forester-pandoc markdown-to-forester --emit-sourcepos-comments`.
- In provenance mode, GFM input is read through Pandoc's CommonMark reader with `Ext_sourcepos` enabled.
- Emitted Forester output includes `% pandoc-sourcepos: ...` comments ahead of top-level blocks when Pandoc exposes source positions.
- Structured diagnostics include `source_span` when the originating Pandoc node carries source-position data.

## 4. Mapping Matrix

## Block-level mappings

- First top-level `Header 1` (when metadata title absent) -> `\title{...}`.
- Additional `Header 1` and all `Header n>1` -> `\section{level}{...}`.
- `Para`/`Plain` -> `\p{...}` (exact for content class).
- `BulletList` -> `\ul{\li{...}...}` (exact structure class).
- `OrderedList` -> `\ol{\li{<index>. ...}...}` (partially exact; style/delim lossy).
- `BlockQuote` -> `\blockquote{...}` with nested rendered blocks (exact structure class).
- `CodeBlock` -> `\pre{\verb...}` or `\pre{\code-language{lang} \verb...}`.
- `LineBlock` -> paragraph with line join (lossy line semantics).
- `DefinitionList` -> unordered list with bold term prefix (lossy, diagnostic).
- `Div` -> wrapper dropped, children preserved (lossy wrapper semantics).
- `RawBlock` -> `\raw-block{format}{\verb...}` (preserved with diagnostic).
- `HorizontalRule` -> `\p{---}` (lossy).
- `Table` -> `\table-fallback{...}` with emitted `\row{...}` entries (fallback with retained cell text + diagnostic).
- `Figure` -> `\figure{\link{target}{alt} \figcaption{...}}` when image extractable; otherwise fallback paragraph + diagnostic.

## Inline-level mappings

- `Str`/`Space`/`SoftBreak`/`LineBreak` -> text/space (linebreak flattening is lossy).
- `Emph` -> `\em{...}` (exact class).
- `Strong` -> `\strong{...}` (exact class).
- `Underline` -> `\em{...}` (lossy, diagnostic).
- `Strikeout` -> `\strong{...}` (lossy, diagnostic).
- `Superscript`/`Subscript` -> literal textual markers (lossy, diagnostic).
- `SmallCaps` -> `\strong{...}` (lossy, diagnostic).
- `Quoted` -> literal quote wrappers around content (partially lossy).
- `Cite` -> `\cite{id1,id2,...}{body}`.
- `Code` -> `\code{\verb...}` (exact intent class).
- `Math Inline` -> `#{...}` (exact class).
- `Math Display` -> `##{...}` (exact class).
- `RawInline` -> `\raw-inline{format}{\verb...}` (preserved with diagnostic).
- `Link` ->
  - internal target with label equal target -> `\ref{...}`
  - otherwise -> `\link{target}{label}`
- `Image` -> `\image{target}{alt}` (inline strategy).
- `Note` -> `\footnote{...}` (policy decision, parser-valid representation).
- `Span` -> wrapper dropped, children preserved.

## 5. Cross-check Against OCaml Grammar/AST

The emitted forms are parser-valid against `lib/parser/Grammar.mly` because they use generic command + brace-arg forms accepted through `Ident` + `arg` productions:

- `\title`, `\section`, `\p`, `\meta`, `\date`, `\author`, `\contributor`, `\tag`, `\taxon`, `\em`, `\strong`, `\code`, `\pre`, `\code-language`, `\blockquote`, `\ul`, `\ol`, `\li`, `\figure`, `\figcaption`, `\image`, `\cite`, `\raw-block`, `\raw-inline`, `\table-fallback`, `\row`, `\caption`, `\footnote`, `\link`, `\ref` are represented as identifiers with arguments.
- Math uses dedicated grammar delimiters `#{...}` and `##{...}`.
- Verbatim content is emitted via `\verb<herald>|...<herald>` in code contexts.

Current OCaml integration tests validate that markdown conversion errors are surfaced as `External_error` and successful converted output parses via `Parse.parse_content`.

## 6. Cross-check Against Rust Parser State

Rust parser parity is still incomplete for full OCaml grammar coverage; therefore bridge output policy currently prioritizes OCaml parser validity as source of truth.

Temporary compatibility constraints:

- Generated output avoids relying on advanced OCaml-only constructs beyond generic command forms and basic math delimiters.
- Differential parity should be tracked as rust-parser backlog; converter correctness gate remains OCaml parser validity first.

## 7. Behavior Categories

Each mapping must be explicitly tagged as one of:

- Exact: semantic class preserved.
- Lossy: usable but semantic detail reduced (must emit diagnostic).
- Fallback: placeholder representation (must emit diagnostic).
- Reject: hard failure (strict mode can enforce this by treating diagnostics as fatal).

No silent dropping is allowed.

## 8. Acceptance Criteria

Bridge is considered ready for next milestone when all are true:

- Converter build and tests pass (`just test-haskell`).
- Markdown fixture corpus conversion matches expected output (manifest-driven snapshots).
- Converter emits deterministic output for stable inputs.
- Structured diagnostics are emitted for every lossy/fallback case.
- Strict mode fails when diagnostics exist.
- CLI supports `--input-format markdown|gfm`, `--strict`, and machine-readable `--diagnostics-format json`.
- OCaml `Parse.parse_content` integration tests pass for:
  - successful conversion
  - missing converter binary
  - non-zero converter exit
  - invalid converter output (as `External_error`)

## 9. Maintenance Rules

- New Pandoc constructor handling must be added explicitly; no wildcard silent drop behavior.
- Any lossy/fallback mapping must include an explicit diagnostic code and message.
- Any mapping change must include test updates in `tools/pandoc-converter/test/Spec.hs` and, where relevant, OCaml integration tests.

## 10. Local Install and PATH

- Use `./scripts/install-local.sh` to install local developer binaries, including `forester-pandoc`.
- The OCaml parser integration resolves converter path in this order:
  1. `FORESTER_PANDOC_PATH` (if set and non-empty)
  2. `forester-pandoc` on `PATH`
- For CI and strict local bridge tests, set:
  - `FORESTER_PANDOC_PATH=<absolute path to converter binary>`
  - `FORESTER_PANDOC_REQUIRE_BINARY=1`

## 11. User Examples

- See `docs/pandoc-bridge/examples.md` for concrete Markdown input and emitted Forester output pairs covering headings, metadata, tables, figures, and citations.
- See `docs/pandoc-bridge/release-checklist.md` for the full pre-release validation checklist.
