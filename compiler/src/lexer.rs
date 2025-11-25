//! Lexical analysis for Scribe source files.

use std::collections::VecDeque;

use thiserror::Error;

use crate::span::Span;

/// Keywords recognized by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    Async,
    Let,
    Mutable,
    Module,
    Use,
    As,
    Record,
    If,
    Else,
    Elif,
    Then,
    For,
    Each,
    In,
    To,
    While,
    Return,
    Start,
    Await,
    End,
    Is,
    Otherwise,
    Break,
    Continue,
    And,
    Or,
    Not,
    True,
    False,
    Inclusive,
    Than,
    Equal,
}

impl Keyword {
    fn from_ident(ident: &str) -> Option<Self> {
        match ident {
            "fn" => Some(Keyword::Fn),
            "async" => Some(Keyword::Async),
            "let" => Some(Keyword::Let),
            "mutable" => Some(Keyword::Mutable),
            "module" => Some(Keyword::Module),
            "use" => Some(Keyword::Use),
            "as" => Some(Keyword::As),
            "record" => Some(Keyword::Record),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "elif" => Some(Keyword::Elif),
            "then" => Some(Keyword::Then),
            "for" => Some(Keyword::For),
            "each" => Some(Keyword::Each),
            "in" => Some(Keyword::In),
            "to" => Some(Keyword::To),
            "while" => Some(Keyword::While),
            "return" => Some(Keyword::Return),
            "start" => Some(Keyword::Start),
            "await" => Some(Keyword::Await),
            "end" => Some(Keyword::End),
            "is" => Some(Keyword::Is),
            "otherwise" => Some(Keyword::Otherwise),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "and" => Some(Keyword::And),
            "or" => Some(Keyword::Or),
            "not" => Some(Keyword::Not),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "inclusive" => Some(Keyword::Inclusive),
            "than" => Some(Keyword::Than),
            "equal" => Some(Keyword::Equal),
            _ => None,
        }
    }
}

/// Token classifications emitted by the lexer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    Identifier(String),
    NumberLiteral(String),
    TextLiteral(String),
    Colon,
    Comma,
    Dot,
    Arrow,
    Equals,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Newline,
    Indent,
    Dedent,
    Eof,
}

/// Concrete token with its span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn eof(line: u32, column: u32) -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span { line, column },
        }
    }
}

/// Stateful lexer that yields tokens on demand.
pub struct Lexer<'a> {
    source: &'a str,
    cursor: usize,
    line: u32,
    column: u32,
    indent_stack: Vec<usize>,
    pending: VecDeque<Token>,
    after_newline: bool,
    current_indent: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            cursor: 0,
            line: 1,
            column: 1,
            indent_stack: vec![0],
            pending: VecDeque::new(),
            after_newline: true,
            current_indent: 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        loop {
            if let Some(token) = self.pending.pop_front() {
                return Ok(token);
            }

            if self.cursor >= self.source.len() {
                if self.after_newline {
                    self.adjust_indent()?;
                    self.after_newline = false;
                    continue;
                }

                if self.indent_stack.len() > 1 {
                    let token = self.make_token_at(TokenKind::Dedent, 1);
                    self.indent_stack.pop();
                    return Ok(token);
                }

                return Ok(Token::eof(self.line, self.column));
            }

            let ch = self.peek_char().expect("checked bounds");

            if ch == '\n' {
                let span = self.span();
                self.bump_char();
                self.after_newline = true;
                self.current_indent = 0;
                return Ok(Token {
                    kind: TokenKind::Newline,
                    span,
                });
            }

            if self.after_newline {
                match ch {
                    ' ' => {
                        self.bump_char();
                        self.current_indent += 1;
                        continue;
                    }
                    '\t' => {
                        return Err(LexerError::TabIndentation { line: self.line });
                    }
                    '\r' => {
                        self.bump_char();
                        continue;
                    }
                    '#' => {
                        self.skip_comment();
                        continue;
                    }
                    _ => {
                        self.adjust_indent()?;
                        self.after_newline = false;
                        continue;
                    }
                }
            }

            match ch {
                ' ' | '\r' => {
                    self.bump_char();
                    continue;
                }
                '\t' => {
                    return Err(LexerError::TabCharacter {
                        line: self.line,
                        column: self.column,
                    });
                }
                '#' => {
                    self.skip_comment();
                    continue;
                }
                '"' => return self.read_string(),
                '0'..='9' => return Ok(self.read_number()),
                'a'..='z' | 'A'..='Z' | '_' => return Ok(self.read_identifier()),
                '(' => return Ok(self.simple_token(TokenKind::LParen)),
                ')' => return Ok(self.simple_token(TokenKind::RParen)),
                '[' => return Ok(self.simple_token(TokenKind::LBracket)),
                ']' => return Ok(self.simple_token(TokenKind::RBracket)),
                '{' => return Ok(self.simple_token(TokenKind::LBrace)),
                '}' => return Ok(self.simple_token(TokenKind::RBrace)),
                ':' => return Ok(self.simple_token(TokenKind::Colon)),
                ',' => return Ok(self.simple_token(TokenKind::Comma)),
                '.' => return Ok(self.simple_token(TokenKind::Dot)),
                '+' => return Ok(self.simple_token(TokenKind::Plus)),
                '-' => {
                    let span = self.span();
                    self.bump_char();
                    if self.peek_char() == Some('>') {
                        self.bump_char();
                        return Ok(Token {
                            kind: TokenKind::Arrow,
                            span,
                        });
                    }
                    return Ok(Token {
                        kind: TokenKind::Minus,
                        span,
                    });
                }
                '*' => return Ok(self.simple_token(TokenKind::Star)),
                '/' => return Ok(self.simple_token(TokenKind::Slash)),
                '=' => {
                    let span = self.span();
                    self.bump_char();
                    if self.peek_char() == Some('=') {
                        self.bump_char();
                        return Ok(Token {
                            kind: TokenKind::EqualEqual,
                            span,
                        });
                    }
                    return Ok(Token {
                        kind: TokenKind::Equals,
                        span,
                    });
                }
                '!' => {
                    let span = self.span();
                    self.bump_char();
                    if self.peek_char() == Some('=') {
                        self.bump_char();
                        return Ok(Token {
                            kind: TokenKind::BangEqual,
                            span,
                        });
                    }
                    return Ok(Token {
                        kind: TokenKind::Bang,
                        span,
                    });
                }
                '<' => {
                    let span = self.span();
                    self.bump_char();
                    if self.peek_char() == Some('=') {
                        self.bump_char();
                        return Ok(Token {
                            kind: TokenKind::LessEqual,
                            span,
                        });
                    }
                    return Ok(Token {
                        kind: TokenKind::Less,
                        span,
                    });
                }
                '>' => {
                    let span = self.span();
                    self.bump_char();
                    if self.peek_char() == Some('=') {
                        self.bump_char();
                        return Ok(Token {
                            kind: TokenKind::GreaterEqual,
                            span,
                        });
                    }
                    return Ok(Token {
                        kind: TokenKind::Greater,
                        span,
                    });
                }
                _ => {
                    let span = self.span();
                    let bad = self.bump_char().unwrap();
                    return Err(LexerError::UnexpectedCharacter {
                        ch: bad,
                        line: span.line,
                        column: span.column,
                    });
                }
            }
        }
    }

    fn read_identifier(&mut self) -> Token {
        let span = self.span();
        let mut ident = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }

        if let Some(keyword) = Keyword::from_ident(&ident) {
            Token {
                kind: TokenKind::Keyword(keyword),
                span,
            }
        } else {
            Token {
                kind: TokenKind::Identifier(ident),
                span,
            }
        }
    }

    fn read_number(&mut self) -> Token {
        let span = self.span();
        let mut literal = String::new();
        while let Some(ch) = self.peek_char() {
            if ch.is_ascii_digit() || ch == '_' {
                literal.push(ch);
                self.bump_char();
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::NumberLiteral(literal),
            span,
        }
    }

    fn read_string(&mut self) -> Result<Token, LexerError> {
        let span = self.span();
        self.bump_char(); // opening quote
        let mut value = String::new();
        while let Some(ch) = self.peek_char() {
            match ch {
                '"' => {
                    self.bump_char();
                    return Ok(Token {
                        kind: TokenKind::TextLiteral(value),
                        span,
                    });
                }
                '\\' => {
                    self.bump_char();
                    let escaped = self
                        .peek_char()
                        .ok_or(LexerError::UnterminatedString { line: span.line })?;
                    let resolved = match escaped {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => {
                            return Err(LexerError::InvalidEscape {
                                line: span.line,
                                escape: other,
                            });
                        }
                    };
                    self.bump_char();
                    value.push(resolved);
                }
                '\n' => {
                    return Err(LexerError::UnterminatedString { line: span.line });
                }
                _ => {
                    value.push(ch);
                    self.bump_char();
                }
            }
        }

        Err(LexerError::UnterminatedString { line: span.line })
    }

    fn simple_token(&mut self, kind: TokenKind) -> Token {
        let token = self.make_token(kind);
        self.bump_char();
        token
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            span: self.span(),
        }
    }

    fn make_token_at(&self, kind: TokenKind, column: u32) -> Token {
        Token {
            kind,
            span: Span {
                line: self.line,
                column,
            },
        }
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            column: self.column,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.cursor..].chars().next()
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.cursor += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch == '\n' {
                break;
            }
            self.bump_char();
        }
    }

    fn adjust_indent(&mut self) -> Result<(), LexerError> {
        let current = *self.indent_stack.last().expect("stack never empty");
        if self.current_indent > current {
            self.indent_stack.push(self.current_indent);
            self.pending
                .push_back(self.make_token_at(TokenKind::Indent, 1));
        } else if self.current_indent < current {
            while let Some(&last) = self.indent_stack.last() {
                if self.current_indent < last {
                    self.indent_stack.pop();
                    self.pending
                        .push_back(self.make_token_at(TokenKind::Dedent, 1));
                } else {
                    break;
                }
            }
            if self.current_indent != *self.indent_stack.last().unwrap() {
                return Err(LexerError::InvalidDedent { line: self.line });
            }
        }
        self.current_indent = 0;
        Ok(())
    }
}

/// Errors that can arise during lexing.
#[derive(Debug, Error, Clone, PartialEq)]
pub enum LexerError {
    #[error("tabs are not allowed for indentation on line {line}")]
    TabIndentation { line: u32 },
    #[error("tabs are not allowed inside code at {line}:{column}")]
    TabCharacter { line: u32, column: u32 },
    #[error("unterminated string literal on line {line}")]
    UnterminatedString { line: u32 },
    #[error("invalid string escape '{escape}' on line {line}")]
    InvalidEscape { line: u32, escape: char },
    #[error("indentation level does not match any previous indent on line {line}")]
    InvalidDedent { line: u32 },
    #[error("unexpected character '{ch}' at {line}:{column}")]
    UnexpectedCharacter { ch: char, line: u32, column: u32 },
}
