// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! Abstract Syntax Tree definitions for Forester
//!
//! These types mirror the OCaml Code.ml types for interoperability.

use serde::{Deserialize, Serialize};

/// Source location information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            start: if self.start.offset < other.start.offset {
                self.start
            } else {
                other.start
            },
            end: if self.end.offset > other.end.offset {
                self.end
            } else {
                other.end
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }
}

/// A located value (value with optional span)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Located<T> {
    pub value: T,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub span: Option<Span>,
}

impl<T> Located<T> {
    pub fn new(value: T, span: Option<Span>) -> Self {
        Self { value, span }
    }

    pub fn without_span(value: T) -> Self {
        Self { value, span: None }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
        Located {
            value: f(self.value),
            span: self.span,
        }
    }
}

/// Binding strictness: strict evaluates immediately, lazy delays evaluation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BindingInfo {
    Strict,
    Lazy,
}

/// A binding is a (strictness, name) pair
pub type Binding = (BindingInfo, String);

/// Delimiter types for grouping
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Delim {
    Braces,  // { }
    Squares, // [ ]
    Parens,  // ( )
}

/// Math display mode
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MathMode {
    Inline,  // #{...}
    Display, // ##{...}
}

/// Import visibility
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Visibility {
    Private,
    Public,
}

/// A path is a list of identifiers (e.g., foo/bar/baz)
pub type Path = Vec<String>;

/// Object definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectDef {
    pub self_name: Option<String>,
    pub methods: Vec<(String, Nodes)>,
}

/// Patch (object extension) definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PatchDef {
    pub obj: Nodes,
    pub self_name: Option<String>,
    pub super_name: Option<String>,
    pub methods: Vec<(String, Nodes)>,
}

/// AST Node types - mirrors OCaml Code.node
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Node {
    // Content nodes
    Text {
        content: String,
    },
    Verbatim {
        content: String,
    },
    Comment {
        content: String,
    },
    Error {
        message: String,
    },

    // Grouping
    Group {
        delim: Delim,
        body: Nodes,
    },
    Math {
        mode: MathMode,
        body: Nodes,
    },

    // Identifiers
    Ident {
        path: Path,
    },
    HashIdent {
        name: String,
    },
    XmlIdent {
        prefix: Option<String>,
        name: String,
    },

    // Binding constructs
    Let {
        path: Path,
        bindings: Vec<Binding>,
        body: Nodes,
    },
    Def {
        path: Path,
        bindings: Vec<Binding>,
        body: Nodes,
    },
    Fun {
        bindings: Vec<Binding>,
        body: Nodes,
    },
    Scope {
        body: Nodes,
    },
    Namespace {
        path: Path,
        body: Nodes,
    },
    Open {
        path: Path,
    },

    // Dynamic variables
    Put {
        path: Path,
        body: Nodes,
    },
    Default {
        path: Path,
        body: Nodes,
    },
    Get {
        path: Path,
    },
    Alloc {
        path: Path,
    },

    // Objects
    Object {
        def: ObjectDef,
    },
    Patch {
        def: PatchDef,
    },
    Call {
        target: Nodes,
        method: String,
    },

    // Document structure
    Subtree {
        addr: Option<String>,
        body: Nodes,
    },
    Import {
        visibility: Visibility,
        target: String,
    },
    DeclXmlns {
        prefix: String,
        uri: String,
    },

    // Datalog
    DxSequent {
        conclusion: Nodes,
        premises: Vec<Nodes>,
    },
    DxQuery {
        var: String,
        positives: Vec<Nodes>,
        negatives: Vec<Nodes>,
    },
    DxProp {
        relation: Nodes,
        args: Vec<Nodes>,
    },
    DxVar {
        name: String,
    },
    DxConstContent {
        body: Nodes,
    },
    DxConstUri {
        body: Nodes,
    },
}

/// A list of located nodes
pub type Nodes = Vec<Located<Node>>;

/// Complete parse tree for a document
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Document {
    pub nodes: Nodes,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
}

impl Document {
    pub fn new(nodes: Nodes) -> Self {
        Self {
            nodes,
            source_path: None,
        }
    }

    pub fn with_source_path(mut self, path: String) -> Self {
        self.source_path = Some(path);
        self
    }
}

// Helper constructors for common nodes
impl Node {
    pub fn text(s: impl Into<String>) -> Self {
        Node::Text { content: s.into() }
    }

    pub fn verbatim(s: impl Into<String>) -> Self {
        Node::Verbatim { content: s.into() }
    }

    pub fn ident(path: Path) -> Self {
        Node::Ident { path }
    }

    pub fn braces(body: Nodes) -> Self {
        Node::Group {
            delim: Delim::Braces,
            body,
        }
    }

    pub fn squares(body: Nodes) -> Self {
        Node::Group {
            delim: Delim::Squares,
            body,
        }
    }

    pub fn parens(body: Nodes) -> Self {
        Node::Group {
            delim: Delim::Parens,
            body,
        }
    }

    pub fn inline_math(body: Nodes) -> Self {
        Node::Math {
            mode: MathMode::Inline,
            body,
        }
    }

    pub fn display_math(body: Nodes) -> Self {
        Node::Math {
            mode: MathMode::Display,
            body,
        }
    }
}
