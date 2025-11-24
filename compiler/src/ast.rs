//! Abstract syntax tree definitions for the Scribe language.

use crate::span::Span;

/// A full module (single source file) AST.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Module {
    /// Module declaration (optional)
    pub module_decl: Option<ModuleDecl>,
    /// Use/import statements at the top of the file
    pub uses: Vec<UseStatement>,
    /// Record type definitions
    pub records: Vec<RecordDef>,
    /// Functions declared in the module.
    pub functions: Vec<Function>,
}

/// Record type definition: `record Name: field1: Type1, field2: Type2`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDef {
    pub name: Identifier,
    pub fields: Vec<RecordField>,
    pub span: Span,
}

/// A field in a record definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub name: Identifier,
    pub ty: TypeExpr,
}

/// Module declaration: `module path.to.module`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDecl {
    pub path: ModulePath,
    pub span: Span,
}

/// Use/import statement: `use path.to.module` or `use path.to.module as alias`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseStatement {
    pub path: ModulePath,
    pub alias: Option<Identifier>,
    pub span: Span,
}

/// Module path: `std.io.file` represented as a list of identifiers
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePath {
    pub segments: Vec<Identifier>,
}

impl ModulePath {
    pub fn new(segments: Vec<Identifier>) -> Self {
        Self { segments }
    }

    pub fn to_string(&self) -> String {
        self.segments
            .iter()
            .map(|s| s.0.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }
}

/// Function declaration node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: Identifier,
    pub signature: FunctionSignature,
    pub body: Block,
    pub is_async: bool,
    pub span: Span,
}

/// Function signature: parameter list + optional return type.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: TypeExpr,
}

/// Type expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    /// Named type (e.g. `int32`, `Result`).
    Named(Identifier),
}

impl Default for TypeExpr {
    fn default() -> Self {
        TypeExpr::Named(Identifier::new("unit"))
    }
}

/// Block is an ordered list of statements within a consistent indentation level.
pub type Block = Vec<Statement>;

/// Statement forms supported by the parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Assignment {
        target: Identifier,
        value: Expression,
        span: Span,
    },
    Expression(Expression),
    Return {
        value: Option<Expression>,
        span: Span,
    },
    If(IfStatement),
    For(ForStatement),
    While(WhileStatement),
    Break { span: Span },
    Continue { span: Span },
}

/// `for` loop: `for i in start to end:` or `for item in collection:`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForStatement {
    pub variable: Identifier,
    pub iterator: ForIterator,
    pub body: Block,
    pub span: Span,
}

/// Iterator expression in a for loop
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForIterator {
    /// Range: `start to end` (exclusive) or `start to end inclusive`
    Range {
        start: Expression,
        end: Expression,
        inclusive: bool,
    },
    /// Collection iteration: `in collection`
    Collection(Expression),
}

/// `while` loop
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

/// `let` bindings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub name: Identifier,
    pub mutable: bool,
    pub ty: Option<TypeExpr>,
    pub value: Option<Expression>,
    pub span: Span,
}

/// `if` statement with optional else branch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

/// Expressions with attached span information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Expression variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionKind {
    Identifier(Identifier),
    Literal(Literal),
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Grouped(Box<Expression>),
    Logical {
        op: LogicalOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Comparison {
        op: ComparisonOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Record construction: `RecordName(field1 = value1, field2 = value2)`
    RecordConstruct {
        name: Identifier,
        fields: Vec<(Identifier, Expression)>,
    },
    /// Field access: `expr.field`
    FieldAccess {
        object: Box<Expression>,
        field: Identifier,
    },
    /// Array literal: `[1, 2, 3]`
    ArrayLiteral(Vec<Expression>),
    /// Index access: `arr[idx]`
    Index {
        object: Box<Expression>,
        index: Box<Expression>,
    },
    /// Await expression: `await expr`
    Await(Box<Expression>),
    /// Start expression (spawn): `start expr`
    Start(Box<Expression>),
}

/// Literal values.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(String),
    Text(String),
    Bool(bool),
}

/// Binary operator kinds.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Unary operator kinds.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

/// Logical operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

/// Comparison operators.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

/// Identifier helper.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(name: impl Into<String>) -> Self {
        Identifier(name.into())
    }
}
