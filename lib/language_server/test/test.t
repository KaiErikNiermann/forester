SPDX-FileCopyrightText: 2024 The Forester Project Contributors

SPDX-License-Identifier: GPL-3.0-or-later

  $ git clone https://git.sr.ht/~jonsterling/forester-notes.org
  Cloning into 'forester-notes.org'...
  $ cd forester-notes.org
  $ nvim --headless --clean -l ../test.lua
  client_id: 1
  filetype: forester
  { "\\title{Forester}", "", "\\p{\\em{Forester} is a tool for authoring, exploring, and sharing scientific and mathematical hypertexts. It is your lab notebook, your journal, your blackboard, and the home of your lecture notes.}", "", "\\p{Forester is maintained by [[jonmsterling]] and [[kentookura]].}", "", "\\ul{", "  \\li{[Project information](jms-0065)}", "  \\li{[Forester blog](30FM)}", "  \\li{[Presentations](5CDY)}", "  \\li{[Release notes](jms-005P)}", "}", "", "\\syndicate-query-as-json-blob{forester-notes.org}{", "  \\datalog{", "    ?X -: {\\rel/is-node ?Z}  {\\rel/in-bundle-closure ?Z ?X}", "  }", "}" }
  {}
  nil

