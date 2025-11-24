use thiserror::Error;

use crate::parser::ParserError;

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error(transparent)]
    Parser(#[from] ParserError),
    #[error("IO error: {0}")]
    Io(String),
}

impl CompilerError {
    pub fn span(&self) -> Option<crate::span::Span> {
        match self {
            CompilerError::Parser(err) => err.span(),
            CompilerError::Io(_) => None,
        }
    }
}
