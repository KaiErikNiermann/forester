<!--
SPDX-FileCopyrightText: 2024 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later
-->

[![builds.sr.ht status](https://builds.sr.ht/~jonsterling/ocaml-forester.svg)](https://builds.sr.ht/~jonsterling/ocaml-forester?)

This is the source repository for the
[forester](https://sr.ht/~jonsterling/forester/) tool, which is implemented in
the OCaml programming language. Please see [this
page](https://www.forester-notes.org) for more information.

### System Requirements

You need to have [OCaml 5](https://ocaml.org) and
[opam](https://opam.ocaml.org) installed.

### Installation

You can install forester by running `opam install forester`. See
[README.nix.md](README.nix.md) for instructions for using Forester with `nix`.

### Contributing

Please mail patches by [email](https://git-send-email.io/) to
<~jonsterling/forester-devel@lists.sr.ht>. General discussion can be mailed to
<~jonsterling/forester-discuss@lists.sr.ht>.

When you prepare patches, please try to match the surrounding coding style to
the best of your ability (and do not use `ocamlformat`); patches will not be
rejected on grounds of poor formatting but they may be reformatted before being
applied. If you install [Topiary](https://topiary.tweag.io), you can format the
entire project using `./format.sh`.

[Join us on IRC](irc://irc.libera.chat/#forester)

### Example Use

Please see my [Forest](https://github.com/jonsterling/forest) for an example of using forester, or create your own forest using `forester init`.

### Persisting standalone TeX sources

When debugging embedded mathematics you can keep the intermediate TeX documents that Forester produces by running `forester build --persist-tex`. The generated sources are stored next to the cached SVGs in `build/resources/<hash>.tex`.

### Customizing LaTeX rendering

Forester embeds mathematics by compiling small standalone LaTeX documents and converting the resulting DVI files to SVG. You can change the document class, its options, and the exact commands that are executed by editing the `[forest.latex]` section of your `forest.toml`:

```toml
[forest.latex]
document_class = "standalone"
document_class_options = ["preview", "border=2pt", "10pt"]
compile_command = ["latex", "-halt-on-error", "-interaction=nonstopmode"]
dvisvgm_command = ["dvisvgm", "--exact", "--clipjoin", "--font-format=woff", "--bbox=papersize", "--zoom=1.5", "--stdin", "--stdout"]
```

When experimenting, you can override these settings without touching the config file by passing `--latex-document-class`, `--latex-document-class-option`, `--latex-compile-command`, or `--latex-dvisvgm-command` to `forester build`. Repeat the latter three flags once per argument to re-create the exact command line you would like Forester to run.
