//! Parser that consumes lexer tokens and builds the Scribe AST.

use std::collections::VecDeque;

use thiserror::Error;

use crate::ast::{
    BinaryOp, Block, ComparisonOp, Expression, ExpressionKind, ForIterator, ForStatement, Function,
    FunctionSignature, Identifier, IfStatement, LetStatement, Literal, LogicalOp, Module,
    ModuleDecl, ModulePath, Parameter, RecordDef, RecordField, Statement, TypeExpr, UnaryOp,
    UseStatement, WhileStatement,
};
use crate::lexer::{Keyword, Lexer, LexerError, Token, TokenKind};
use crate::span::Span;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    buffer: VecDeque<Token>,
    eof_seen: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source),
            buffer: VecDeque::new(),
            eof_seen: false,
        }
    }

    /// Parse a full module consisting of optional module decl, use statements, records, and function declarations.
    pub fn parse_module(&mut self) -> Result<Module, ParserError> {
        let mut module_decl = None;
        let mut uses = Vec::new();
        let mut records = Vec::new();
        let mut functions = Vec::new();

        // Parse optional module declaration
        self.skip_newlines()?;
        if matches!(self.peek_kind()?, TokenKind::Keyword(Keyword::Module)) {
            module_decl = Some(self.parse_module_decl()?);
            self.skip_newlines()?;
        }

        // Parse use statements
        while matches!(self.peek_kind()?, TokenKind::Keyword(Keyword::Use)) {
            uses.push(self.parse_use_statement()?);
            self.skip_newlines()?;
        }

        // Parse records and functions
        loop {
            self.skip_newlines()?;
            match self.peek_kind()? {
                TokenKind::Eof => break,
                TokenKind::Keyword(Keyword::Record) => {
                    records.push(self.parse_record_def()?);
                }
                _ => {
                    functions.push(self.parse_function()?);
                }
            }
        }

        Ok(Module {
            module_decl,
            uses,
            records,
            functions,
        })
    }

    fn parse_module_decl(&mut self) -> Result<ModuleDecl, ParserError> {
        let start_token = self.advance()?; // consume 'module'
        let path = self.parse_module_path()?;
        self.expect_newline()?;
        Ok(ModuleDecl {
            path,
            span: start_token.span,
        })
    }

    fn parse_use_statement(&mut self) -> Result<UseStatement, ParserError> {
        let start_token = self.advance()?; // consume 'use'
        let path = self.parse_module_path()?;

        let alias = if self.match_keyword(Keyword::As)? {
            let (ident, _) = self.expect_identifier()?;
            Some(ident)
        } else {
            None
        };

        self.expect_newline()?;

        Ok(UseStatement {
            path,
            alias,
            span: start_token.span,
        })
    }

    fn parse_module_path(&mut self) -> Result<ModulePath, ParserError> {
        let mut segments = Vec::new();
        // Module paths can contain keywords as segments (e.g., module my.module)
        let (first, _) = self.expect_identifier_or_keyword()?;
        segments.push(first);

        while self.match_symbol(TokenKind::Dot)? {
            let (segment, _) = self.expect_identifier_or_keyword()?;
            segments.push(segment);
        }

        Ok(ModulePath::new(segments))
    }

    fn parse_record_def(&mut self) -> Result<RecordDef, ParserError> {
        let start = self.expect_keyword(Keyword::Record)?;
        let (name, _) = self.expect_identifier()?;

        // Records can be defined with inline fields or block fields
        // Inline: `record Point(x: number, y: number)`
        // Block: `record Point:\n    x: number\n    y: number`
        let fields = if self.match_symbol(TokenKind::LParen)? {
            // Inline style
            let fields = self.parse_record_fields_inline()?;
            self.expect_symbol(TokenKind::RParen, "closing ')' for record")?;
            self.skip_newlines()?;
            fields
        } else if self.match_symbol(TokenKind::Colon)? {
            // Block style
            self.parse_record_fields_block()?
        } else {
            // Empty record
            Vec::new()
        };

        Ok(RecordDef {
            name,
            fields,
            span: start.span,
        })
    }

    fn parse_record_fields_inline(&mut self) -> Result<Vec<RecordField>, ParserError> {
        let mut fields = Vec::new();
        loop {
            if matches!(self.peek_kind()?, TokenKind::RParen) {
                break;
            }
            let (name, _) = self.expect_identifier()?;
            self.expect_symbol(TokenKind::Colon, "':' in record field")?;
            let ty = self.parse_type_expr()?;
            fields.push(RecordField { name, ty });

            if !self.match_symbol(TokenKind::Comma)? {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_record_fields_block(&mut self) -> Result<Vec<RecordField>, ParserError> {
        let mut fields = Vec::new();
        self.skip_newlines()?;

        // Expect indent
        if !matches!(self.peek_kind()?, TokenKind::Indent) {
            return Ok(fields);
        }
        self.advance()?;

        loop {
            self.skip_newlines()?;
            match self.peek_kind()? {
                TokenKind::Dedent | TokenKind::Eof => {
                    if matches!(self.peek_kind()?, TokenKind::Dedent) {
                        self.advance()?;
                    }
                    break;
                }
                _ => {
                    let (name, _) = self.expect_identifier()?;
                    self.expect_symbol(TokenKind::Colon, "':' in record field")?;
                    let ty = self.parse_type_expr()?;
                    fields.push(RecordField { name, ty });
                    self.skip_newlines()?;
                }
            }
        }
        Ok(fields)
    }

    fn parse_function(&mut self) -> Result<Function, ParserError> {
        let mut is_async = false;
        if self.match_keyword(Keyword::Async)? {
            is_async = true;
        }

        self.expect_keyword(Keyword::Fn)?;
        let (name, name_span) = self.expect_identifier()?;
        self.expect_symbol(TokenKind::LParen, "opening '(' for parameters")?;
        let params = self.parse_params()?;
        self.expect_symbol(TokenKind::RParen, "closing ')' for parameters")?;

        let return_type = if self.match_symbol(TokenKind::Arrow)? {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect_block_introducer()?;
        let body = self.parse_block("function body")?;

        Ok(Function {
            name,
            signature: FunctionSignature {
                params,
                return_type,
            },
            body,
            is_async,
            span: name_span,
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut params = Vec::new();
        loop {
            match self.peek_kind()? {
                TokenKind::RParen => break,
                _ => {
                    let param = self.parse_param()?;
                    params.push(param);
                    if !self.match_symbol(TokenKind::Comma)? {
                        break;
                    }
                }
            }
        }
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Parameter, ParserError> {
        let (name, _) = self.expect_identifier()?;
        self.expect_symbol(TokenKind::Colon, "':' in parameter declaration")?;
        let ty = self.parse_type_expr()?;
        Ok(Parameter { name, ty })
    }

    fn parse_block(&mut self, context: &'static str) -> Result<Block, ParserError> {
        self.expect_newline()?;
        self.expect_symbol(TokenKind::Indent, "indented block")?;
        let mut statements = Vec::new();
        loop {
            match self.peek_kind()? {
                TokenKind::Dedent => {
                    self.advance()?;
                    break;
                }
                TokenKind::Eof => {
                    return Err(ParserError::UnexpectedEof { context });
                }
                TokenKind::Newline => {
                    self.advance()?;
                }
                _ => statements.push(self.parse_statement()?),
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek_kind()? {
            TokenKind::Keyword(Keyword::Let) => self.parse_let_statement(),
            TokenKind::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenKind::Keyword(Keyword::For) => self.parse_for_statement(),
            TokenKind::Keyword(Keyword::While) => self.parse_while_statement(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_statement(),
            TokenKind::Keyword(Keyword::Continue) => self.parse_continue_statement(),
            _ => {
                if self.is_assignment_start()? {
                    self.parse_assignment_statement()
                } else {
                    self.parse_expression_statement()
                }
            }
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.expect_keyword(Keyword::Let)?;
        let mutable = self.match_keyword(Keyword::Mutable)?;
        let (name, _) = self.expect_identifier()?;
        let ty = if self.match_symbol(TokenKind::Colon)? {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let value = if self.match_symbol(TokenKind::Equals)? {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.expect_statement_terminator("end of let statement")?;
        Ok(Statement::Let(LetStatement {
            name,
            mutable,
            ty,
            value,
            span: start.span,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.expect_keyword(Keyword::Return)?;
        let value = match self.peek_kind()? {
            TokenKind::Newline | TokenKind::Dedent | TokenKind::Eof => None,
            _ => Some(self.parse_expression()?),
        };
        self.expect_statement_terminator("end of return statement")?;
        Ok(Statement::Return {
            value,
            span: start.span,
        })
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expression()?;
        self.expect_block_introducer()?;
        let then_block = self.parse_block("if body")?;

        self.skip_newlines()?;
        let else_block = if self.match_keyword(Keyword::Else)? {
            self.expect_block_introducer()?;
            Some(self.parse_block("else body")?)
        } else {
            None
        };

        Ok(Statement::If(IfStatement {
            condition,
            then_block,
            else_block,
            span: start.span,
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.expect_keyword(Keyword::For)?;
        let (variable, _) = self.expect_identifier()?;

        // Check for `in` keyword
        let iterator = if self.match_keyword(Keyword::In)? {
            // Could be `for item in collection:` or `for i in start to end:`
            let first_expr = self.parse_comparison()?;  // Parse low precedence expression to stop at `to`
            
            // Check if next is `to` keyword (range iteration)
            if self.peek_kind()? == TokenKind::Keyword(Keyword::To) {
                self.advance()?;  // consume 'to'
                let end_expr = self.parse_expression()?;
                let inclusive = self.match_keyword(Keyword::Inclusive)?;
                ForIterator::Range {
                    start: first_expr,
                    end: end_expr,
                    inclusive,
                }
            } else {
                // Collection iteration
                ForIterator::Collection(first_expr)
            }
        } else {
            // Old syntax: `for variable start to end:` (no `in` keyword)
            // For now, return error - the new syntax requires `in`
            return Err(ParserError::UnexpectedToken {
                expected: "in keyword for loop variable".to_string(),
                found: self.peek_kind()?,
                span: Span::default(),
            });
        };

        self.expect_block_introducer()?;
        let body = self.parse_block("for body")?;

        Ok(Statement::For(ForStatement {
            variable,
            iterator,
            body,
            span: start.span,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParserError> {
        let start = self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expression()?;
        self.expect_block_introducer()?;
        let body = self.parse_block("while body")?;

        Ok(Statement::While(WhileStatement {
            condition,
            body,
            span: start.span,
        }))
    }

    fn parse_break_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.expect_keyword(Keyword::Break)?;
        self.expect_statement_terminator("end of break statement")?;
        Ok(Statement::Break { span: token.span })
    }

    fn parse_continue_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.expect_keyword(Keyword::Continue)?;
        self.expect_statement_terminator("end of continue statement")?;
        Ok(Statement::Continue { span: token.span })
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement, ParserError> {
        let (name, span) = self.expect_identifier()?;
        self.expect_symbol(TokenKind::Equals, "'=' in assignment")?;
        let value = self.parse_expression()?;
        self.expect_statement_terminator("end of assignment")?;
        Ok(Statement::Assignment {
            target: name,
            value,
            span,
        })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expr = self.parse_expression()?;
        self.expect_statement_terminator("end of expression statement")?;
        Ok(Statement::Expression(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_logical_and()?;
        while self.match_keyword(Keyword::Or)? {
            let left = expr;
            let right = self.parse_logical_and()?;
            let span = left.span;
            expr = Expression::new(
                ExpressionKind::Logical {
                    op: LogicalOp::Or,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_equality()?;
        while self.match_keyword(Keyword::And)? {
            let left = expr;
            let right = self.parse_equality()?;
            let span = left.span;
            expr = Expression::new(
                ExpressionKind::Logical {
                    op: LogicalOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_comparison()?;
        loop {
            match self.peek_kind()? {
                TokenKind::EqualEqual => {
                    self.advance()?;
                    let left = expr;
                    let right = self.parse_comparison()?;
                    let span = left.span;
                    expr = Expression::new(
                        ExpressionKind::Comparison {
                            op: ComparisonOp::Equal,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        span,
                    );
                }
                TokenKind::BangEqual => {
                    self.advance()?;
                    let left = expr;
                    let right = self.parse_comparison()?;
                    let span = left.span;
                    expr = Expression::new(
                        ExpressionKind::Comparison {
                            op: ComparisonOp::NotEqual,
                            left: Box::new(left),
                            right: Box::new(right),
                        },
                        span,
                    );
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_term()?;
        loop {
            let op = match self.peek_kind()? {
                TokenKind::Less => Some(ComparisonOp::Less),
                TokenKind::LessEqual => Some(ComparisonOp::LessEqual),
                TokenKind::Greater => Some(ComparisonOp::Greater),
                TokenKind::GreaterEqual => Some(ComparisonOp::GreaterEqual),
                _ => None,
            };
            let op = match op {
                Some(op) => op,
                None => break,
            };
            self.advance()?;
            let left = expr;
            let right = self.parse_term()?;
            let span = left.span;
            expr = Expression::new(
                ExpressionKind::Comparison {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, ParserError> {
        self.parse_binary_chain(
            |parser| parser.parse_factor(),
            &[TokenKind::Plus, TokenKind::Minus],
            |kind| match kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            },
        )
    }

    fn parse_factor(&mut self) -> Result<Expression, ParserError> {
        self.parse_binary_chain(
            |parser| parser.parse_unary_expression(),
            &[TokenKind::Star, TokenKind::Slash],
            |kind| match kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                _ => unreachable!(),
            },
        )
    }

    fn parse_binary_chain<F>(
        &mut self,
        mut next: F,
        operators: &[TokenKind],
        map: impl Fn(TokenKind) -> BinaryOp,
    ) -> Result<Expression, ParserError>
    where
        F: FnMut(&mut Self) -> Result<Expression, ParserError>,
    {
        let mut expr = next(self)?;
        loop {
            let op_kind = match self.peek_kind()? {
                kind if operators.iter().any(|expected| *expected == kind) => kind,
                _ => break,
            };
            self.advance()?;
            let right = next(self)?;
            let left = expr;
            let span = left.span;
            expr = Expression::new(
                ExpressionKind::Binary {
                    op: map(op_kind),
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError> {
        if self.match_symbol(TokenKind::Minus)? {
            let expr = self.parse_unary_expression()?;
            let span = expr.span;
            return Ok(Expression::new(
                ExpressionKind::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        if self.match_keyword(Keyword::Not)? || self.match_symbol(TokenKind::Bang)? {
            let expr = self.parse_unary_expression()?;
            let span = expr.span;
            return Ok(Expression::new(
                ExpressionKind::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                },
                span,
            ));
        }
        // Handle await expression
        if self.match_keyword(Keyword::Await)? {
            let expr = self.parse_unary_expression()?;
            let span = expr.span;
            return Ok(Expression::new(
                ExpressionKind::Await(Box::new(expr)),
                span,
            ));
        }
        // Handle start (spawn) expression
        if self.match_keyword(Keyword::Start)? {
            let expr = self.parse_unary_expression()?;
            let span = expr.span;
            return Ok(Expression::new(
                ExpressionKind::Start(Box::new(expr)),
                span,
            ));
        }
        self.parse_call_expression()
    }

    fn parse_call_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            if self.match_symbol(TokenKind::LParen)? {
                // Could be function call or record construction
                // Record construction uses named args: RecordName(field = value)
                // Function call uses positional args: func(arg1, arg2)
                let (args, is_record) = self.parse_call_or_record_args()?;
                self.expect_symbol(TokenKind::RParen, "closing ')' in call")?;
                let span = expr.span;

                if is_record {
                    // Extract record name from identifier expression
                    if let ExpressionKind::Identifier(name) = expr.kind {
                        expr = Expression::new(
                            ExpressionKind::RecordConstruct {
                                name,
                                fields: args
                                    .into_iter()
                                    .map(|(name, value)| (name.unwrap(), value))
                                    .collect(),
                            },
                            span,
                        );
                    } else {
                        // Not an identifier, treat as regular call
                        expr = Expression::new(
                            ExpressionKind::Call {
                                callee: Box::new(expr),
                                arguments: args.into_iter().map(|(_, v)| v).collect(),
                            },
                            span,
                        );
                    }
                } else {
                    expr = Expression::new(
                        ExpressionKind::Call {
                            callee: Box::new(expr),
                            arguments: args.into_iter().map(|(_, v)| v).collect(),
                        },
                        span,
                    );
                }
            } else if self.match_symbol(TokenKind::Dot)? {
                // Field access
                let (field, _) = self.expect_identifier()?;
                let span = expr.span;
                expr = Expression::new(
                    ExpressionKind::FieldAccess {
                        object: Box::new(expr),
                        field,
                    },
                    span,
                );
            } else if self.match_symbol(TokenKind::LBracket)? {
                // Index access
                let index = self.parse_expression()?;
                self.expect_symbol(TokenKind::RBracket, "closing ']' in index")?;
                let span = expr.span;
                expr = Expression::new(
                    ExpressionKind::Index {
                        object: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                );
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse arguments that could be either function call args or record construction fields
    /// Returns (args, is_record) where is_record is true if any arg has `name = value` form
    fn parse_call_or_record_args(
        &mut self,
    ) -> Result<(Vec<(Option<Identifier>, Expression)>, bool), ParserError> {
        let mut args = Vec::new();
        let mut is_record = false;

        if self.peek_kind()? == TokenKind::RParen {
            return Ok((args, false));
        }

        loop {
            // Check if this is a named argument: `name = value`
            let has_name = if let TokenKind::Identifier(_) = self.peek_kind()? {
                matches!(self.peek_kind_at(1)?, Some(TokenKind::Equals))
            } else {
                false
            };

            if has_name {
                let (name, _) = self.expect_identifier()?;
                self.expect_symbol(TokenKind::Equals, "'=' in named argument")?;
                let value = self.parse_expression()?;
                args.push((Some(name), value));
                is_record = true;
            } else {
                let value = self.parse_expression()?;
                args.push((None, value));
            }

            if !self.match_symbol(TokenKind::Comma)? {
                break;
            }
        }

        Ok((args, is_record))
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.advance()?;
        match token.kind {
            TokenKind::Identifier(name) => Ok(Expression::new(
                ExpressionKind::Identifier(Identifier(name)),
                token.span,
            )),
            TokenKind::NumberLiteral(value) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Number(value)),
                token.span,
            )),
            TokenKind::TextLiteral(value) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Text(value)),
                token.span,
            )),
            TokenKind::Keyword(Keyword::True) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Bool(true)),
                token.span,
            )),
            TokenKind::Keyword(Keyword::False) => Ok(Expression::new(
                ExpressionKind::Literal(Literal::Bool(false)),
                token.span,
            )),
            TokenKind::LParen => {
                let expr = self.parse_expression()?;
                self.expect_symbol(TokenKind::RParen, "closing ')' in expression")?;
                Ok(Expression::new(
                    ExpressionKind::Grouped(Box::new(expr)),
                    token.span,
                ))
            }
            TokenKind::LBracket => {
                // Array literal
                let mut elements = Vec::new();
                if self.peek_kind()? != TokenKind::RBracket {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.match_symbol(TokenKind::Comma)? {
                            break;
                        }
                    }
                }
                self.expect_symbol(TokenKind::RBracket, "closing ']' in array")?;
                Ok(Expression::new(
                    ExpressionKind::ArrayLiteral(elements),
                    token.span,
                ))
            }
            _ => Err(ParserError::unexpected(token, "expression")),
        }
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParserError> {
        let (ident, _) = self.expect_identifier()?;
        Ok(TypeExpr::Named(ident))
    }

    fn expect_block_introducer(&mut self) -> Result<(), ParserError> {
        if self.match_symbol(TokenKind::Colon)? {
            Ok(())
        } else if self.match_keyword(Keyword::Then)? {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken {
                expected: "':' or 'then'".to_string(),
                found: self.peek_kind()?,
                span: self.peek_span().unwrap_or_default(),
            })
        }
    }

    fn expect_statement_terminator(&mut self, context: &str) -> Result<(), ParserError> {
        match self.peek_kind()? {
            TokenKind::Newline => {
                self.advance()?;
                Ok(())
            }
            TokenKind::Dedent | TokenKind::Eof => Ok(()),
            _ => Err(ParserError::UnexpectedToken {
                expected: context.to_string(),
                found: self.peek_kind()?,
                span: self.peek_span().unwrap_or_default(),
            }),
        }
    }

    fn is_assignment_start(&mut self) -> Result<bool, ParserError> {
        if let TokenKind::Identifier(_) = self.peek_kind()? {
            if let Some(TokenKind::Equals) = self.peek_kind_at(1)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn skip_newlines(&mut self) -> Result<(), ParserError> {
        while self.peek_kind()? == TokenKind::Newline {
            self.advance()?;
        }
        Ok(())
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Result<bool, ParserError> {
        if let TokenKind::Keyword(k) = self.peek_kind()? {
            if k == keyword {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<Token, ParserError> {
        let token = self.advance()?;
        match &token.kind {
            TokenKind::Keyword(k) if *k == keyword => Ok(token),
            _ => Err(ParserError::unexpected(
                token,
                &format!("keyword {:?}", keyword),
            )),
        }
    }

    fn expect_identifier(&mut self) -> Result<(Identifier, Span), ParserError> {
        let token = self.advance()?;
        match token.kind.clone() {
            TokenKind::Identifier(name) => Ok((Identifier(name), token.span)),
            TokenKind::Keyword(k) => Err(ParserError::unexpected(
                token,
                &format!("identifier, found keyword {:?}", k),
            )),
            _ => Err(ParserError::unexpected(token, "identifier")),
        }
    }

    /// Parse an identifier or a keyword (keywords can be used as identifiers in certain contexts like module paths)
    fn expect_identifier_or_keyword(&mut self) -> Result<(Identifier, Span), ParserError> {
        let token = self.advance()?;
        match token.kind.clone() {
            TokenKind::Identifier(name) => Ok((Identifier(name), token.span)),
            TokenKind::Keyword(k) => Ok((Identifier(format!("{:?}", k).to_lowercase()), token.span)),
            _ => Err(ParserError::unexpected(token, "identifier")),
        }
    }

    fn expect_symbol(&mut self, expected: TokenKind, context: &str) -> Result<Token, ParserError> {
        let token = self.advance()?;
        if token.kind == expected {
            Ok(token)
        } else {
            Err(ParserError::unexpected(token, context))
        }
    }

    fn match_symbol(&mut self, expected: TokenKind) -> Result<bool, ParserError> {
        if self.peek_kind()? == expected {
            self.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn expect_newline(&mut self) -> Result<(), ParserError> {
        let token = self.advance()?;
        if token.kind == TokenKind::Newline {
            Ok(())
        } else {
            Err(ParserError::unexpected(token, "newline"))
        }
    }

    fn peek_kind(&mut self) -> Result<TokenKind, ParserError> {
        Ok(self
            .peek_token(0)?
            .map(|t| t.kind.clone())
            .unwrap_or(TokenKind::Eof))
    }

    fn peek_kind_at(&mut self, offset: usize) -> Result<Option<TokenKind>, ParserError> {
        Ok(self.peek_token(offset)?.map(|t| t.kind.clone()))
    }

    fn peek_span(&mut self) -> Option<Span> {
        self.peek_token(0).ok().flatten().map(|t| t.span)
    }

    fn peek_token(&mut self, offset: usize) -> Result<Option<&Token>, ParserError> {
        self.ensure_buffer(offset)?;
        Ok(self.buffer.get(offset))
    }

    fn advance(&mut self) -> Result<Token, ParserError> {
        self.ensure_buffer(0)?;
        if let Some(token) = self.buffer.pop_front() {
            Ok(token)
        } else {
            Ok(Token::eof(0, 0))
        }
    }

    fn ensure_buffer(&mut self, offset: usize) -> Result<(), ParserError> {
        while self.buffer.len() <= offset && !self.eof_seen {
            let token = self.lexer.next_token()?;
            if matches!(token.kind, TokenKind::Eof) {
                self.eof_seen = true;
            }
            self.buffer.push_back(token);
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error(transparent)]
    Lexer(#[from] LexerError),
    #[error("unexpected token {found:?} at {span:?}, expected {expected}")]
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        span: Span,
    },
    #[error("unexpected end of input while parsing {context}")]
    UnexpectedEof { context: &'static str },
}

impl ParserError {
    fn unexpected(token: Token, expected: &str) -> Self {
        ParserError::UnexpectedToken {
            expected: expected.to_string(),
            found: token.kind,
            span: token.span,
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            ParserError::UnexpectedToken { span, .. } => Some(*span),
            ParserError::UnexpectedEof { .. } => None,
            ParserError::Lexer(_) => None,
        }
    }
}
