# Markdown Bridge Regression Fixtures

Every converter bug fix should add a regression fixture set in this directory.

Required files for each regression stem:

- `<stem>.md`: the Markdown input that previously failed
- `<stem>.forester`: the expected Forester output after the fix
- `<stem>.why.md`: a short note describing the bug, the failure mode, and why this fixture exists

Each regression stem must also be added to `tools/pandoc-converter/fixtures/markdown/manifest.txt`
so it runs in both the Haskell snapshot tests and the OCaml bridge integration tests.
