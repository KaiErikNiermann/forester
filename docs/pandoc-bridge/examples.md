# Markdown to Forester Examples

## Heading and Sections

Markdown:

```markdown
# Main Title

## Section One
```

Forester:

```forester
\title{Main Title}

\section{2}{Section One}
```

## Metadata to Forester Commands

Markdown:

```markdown
---
title: Meta Title
author:
  - Alice
tags:
  - parser
custom_key: custom value
---
```

Forester:

```forester
\title{Meta Title}

\author{Alice}

\tag{parser}

\meta{custom_key}{custom value}
```

## Table Fallback

Markdown:

```markdown
| a | b |
|---|---|
| 1 | 2 |
```

Forester:

```forester
\table-fallback{
  \row{a | b}
  \row{1 | 2}
}
```

## Figure and Citation

Markdown:

```markdown
![diagram](assets/diagram.png)

A claim [@doe2020].
```

Forester:

```forester
\figure{\link{assets/diagram.png}{diagram} \figcaption{diagram}}

\p{A claim \cite{doe2020}{\[@doe2020\]}.}
```
