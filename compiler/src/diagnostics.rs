use crate::span::Span;
use crate::typeck::TypeError;
use crate::CompilerError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: Option<Span>,
    pub severity: Severity,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
            severity: Severity::Error,
        }
    }
}

impl From<TypeError> for Diagnostic {
    fn from(value: TypeError) -> Self {
        Diagnostic::error(value.to_string(), Some(value.span))
    }
}

impl From<CompilerError> for Diagnostic {
    fn from(value: CompilerError) -> Self {
        Diagnostic::error(value.to_string(), value.span())
    }
}

pub struct Diagnostics {
    entries: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.entries.push(diagnostic);
    }

    pub fn extend<I>(&mut self, diagnostics: I)
    where
        I: IntoIterator<Item = Diagnostic>,
    {
        self.entries.extend(diagnostics);
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.entries.iter()
    }
}
