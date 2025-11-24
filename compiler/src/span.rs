//! Shared source span utilities.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Span {
    pub line: u32,
    pub column: u32,
}
