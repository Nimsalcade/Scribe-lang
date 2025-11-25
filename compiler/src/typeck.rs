use std::collections::HashMap;

use crate::ast::{
    Block, Expression, ExpressionKind, ForIterator, ForStatement, Function, IfStatement,
    LetStatement, Literal, Module, Statement, TypeExpr, UnaryOp, WhileStatement,
};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Number,
    Bool,
    Text,
    Unit,
    Unknown,
    Record(String), // Record type identified by name
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "number"),
            Type::Bool => write!(f, "bool"),
            Type::Text => write!(f, "text"),
            Type::Unit => write!(f, "unit"),
            Type::Unknown => write!(f, "unknown"),
            Type::Record(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    UndefinedVariable { name: String },
    UndefinedFunction { name: String },
    UndefinedRecord { name: String },
    TypeMismatch { expected: Type, found: Type },
    MissingReturnValue { expected: Type },
    UnexpectedReturnValue,
    NonBooleanCondition,
    NonNumericOperand,
    ArgumentCountMismatch { expected: usize, found: usize },
    MissingRecordField { record: String, field: String },
    UnexpectedRecordField { record: String, field: String },
}

pub fn check_module(module: &Module) -> Result<(), Vec<TypeError>> {
    let mut checker = Checker::new();
    
    // First pass: collect function signatures and record definitions
    for function in &module.functions {
        let param_types = function
            .signature
            .params
            .iter()
            .map(|param| checker.resolve_type_expr(&param.ty))
            .collect();
        let return_type = function
            .signature
            .return_type
            .as_ref()
            .map(|ty| checker.resolve_type_expr(ty))
            .unwrap_or(Type::Unit);
        
        checker.functions.insert(
            function.name.0.clone(),
            FunctionSignatureInfo {
                param_types,
                return_type,
            },
        );
    }
    
    for record in &module.records {
        let fields = record
            .fields
            .iter()
            .map(|field| (field.name.0.clone(), checker.resolve_type_expr(&field.ty)))
            .collect();
        checker.records.insert(
            record.name.0.clone(),
            RecordInfo { fields },
        );
    }
    
    // Second pass: type-check function bodies
    for function in &module.functions {
        checker.check_function(function);
    }
    
    if checker.errors.is_empty() {
        Ok(())
    } else {
        Err(checker.errors)
    }
}

/// Stores information about a function signature
#[derive(Debug, Clone)]
struct FunctionSignatureInfo {
    param_types: Vec<Type>,
    return_type: Type,
}

/// Stores information about a record type
#[derive(Debug, Clone)]
struct RecordInfo {
    fields: HashMap<String, Type>,
}

struct Checker {
    scopes: Vec<HashMap<String, Type>>,
    current_return: Type,
    errors: Vec<TypeError>,
    functions: HashMap<String, FunctionSignatureInfo>,
    records: HashMap<String, RecordInfo>,
}

impl Checker {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            current_return: Type::Unit,
            errors: Vec::new(),
            functions: HashMap::new(),
            records: HashMap::new(),
        }
    }

    fn check_function(&mut self, function: &Function) {
        self.scopes.clear();
        self.push_scope();
        self.current_return = function
            .signature
            .return_type
            .as_ref()
            .map(|ty| self.resolve_type_expr(ty))
            .unwrap_or(Type::Unit);

        for param in &function.signature.params {
            let ty = self.resolve_type_expr(&param.ty);
            self.define(param.name.0.clone(), ty);
        }

        self.check_block(&function.body);
        self.scopes.clear();
    }

    fn check_block(&mut self, block: &Block) {
        self.push_scope();
        for stmt in block {
            self.check_statement(stmt);
        }
        self.pop_scope();
    }

    fn check_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Let(stmt) => self.check_let(stmt),
            Statement::Assignment {
                target,
                value,
                span,
            } => {
                let value_ty = self.check_expression(value);
                match self.lookup(&target.0) {
                    Some(target_ty) => {
                        self.ensure_assignable(target_ty.clone(), value_ty, value.span)
                    }
                    None => self.errors.push(TypeError {
                        span: *span,
                        kind: TypeErrorKind::UndefinedVariable {
                            name: target.0.clone(),
                        },
                    }),
                }
            }
            Statement::Expression(expr) => {
                self.check_expression(expr);
            }
            Statement::Return { value, span } => self.check_return(value.as_ref(), *span),
            Statement::If(if_stmt) => self.check_if(if_stmt),
            Statement::For(for_stmt) => self.check_for(for_stmt),
            Statement::While(while_stmt) => self.check_while(while_stmt),
            Statement::Break { .. } | Statement::Continue { .. } => {
                // Break/continue are valid in loops - we'd need loop context to validate
            }
        }
    }

    fn check_let(&mut self, stmt: &LetStatement) {
        let annotated = stmt.ty.as_ref().map(|ty| self.resolve_type_expr(ty));
        let value_ty = stmt
            .value
            .as_ref()
            .map(|expr| self.check_expression(expr))
            .unwrap_or(Type::Unknown);

        if let Some(annot_ty) = annotated.clone() {
            self.ensure_assignable(annot_ty.clone(), value_ty.clone(), stmt.span);
            self.define(stmt.name.0.clone(), annot_ty);
        } else {
            self.define(stmt.name.0.clone(), value_ty);
        }
    }

    fn check_return(&mut self, value: Option<&Expression>, span: Span) {
        match (self.current_return.clone(), value) {
            (Type::Unit, None) => {}
            (Type::Unit, Some(_)) => self.errors.push(TypeError {
                span,
                kind: TypeErrorKind::UnexpectedReturnValue,
            }),
            (expected, None) => self.errors.push(TypeError {
                span,
                kind: TypeErrorKind::MissingReturnValue { expected },
            }),
            (expected, Some(expr)) => {
                let value_ty = self.check_expression(expr);
                self.ensure_assignable(expected, value_ty, expr.span);
            }
        }
    }

    fn check_if(&mut self, if_stmt: &IfStatement) {
        let cond_ty = self.check_expression(&if_stmt.condition);
        if !matches!(cond_ty, Type::Bool | Type::Unknown) {
            self.errors.push(TypeError {
                span: if_stmt.condition.span,
                kind: TypeErrorKind::NonBooleanCondition,
            });
        }
        self.check_block(&if_stmt.then_block);
        if let Some(else_block) = &if_stmt.else_block {
            self.check_block(else_block);
        }
    }

    fn check_for(&mut self, for_stmt: &ForStatement) {
        self.push_scope();

        // Define the loop variable
        let var_type = match &for_stmt.iterator {
            ForIterator::Range { start, end, .. } => {
                // Check that start and end are numeric
                let start_ty = self.check_expression(start);
                let end_ty = self.check_expression(end);
                self.ensure_numeric(start_ty, start.span);
                self.ensure_numeric(end_ty, end.span);
                Type::Number
            }
            ForIterator::Collection(expr) => {
                // For now, assume collection iteration yields Unknown type
                self.check_expression(expr);
                Type::Unknown
            }
        };

        self.define(for_stmt.variable.0.clone(), var_type);
        self.check_block(&for_stmt.body);
        self.pop_scope();
    }

    fn check_while(&mut self, while_stmt: &WhileStatement) {
        let cond_ty = self.check_expression(&while_stmt.condition);
        if !matches!(cond_ty, Type::Bool | Type::Unknown) {
            self.errors.push(TypeError {
                span: while_stmt.condition.span,
                kind: TypeErrorKind::NonBooleanCondition,
            });
        }
        self.push_scope();
        self.check_block(&while_stmt.body);
        self.pop_scope();
    }

    fn check_expression(&mut self, expr: &Expression) -> Type {
        match &expr.kind {
            ExpressionKind::Identifier(ident) => {
                self.lookup(&ident.0).cloned().unwrap_or_else(|| {
                    self.errors.push(TypeError {
                        span: expr.span,
                        kind: TypeErrorKind::UndefinedVariable {
                            name: ident.0.clone(),
                        },
                    });
                    Type::Unknown
                })
            }
            ExpressionKind::Literal(lit) => match lit {
                Literal::Number(_) => Type::Number,
                Literal::Text(_) => Type::Text,
                Literal::Bool(_) => Type::Bool,
            },
            ExpressionKind::Call { callee, arguments } => {
                // Check all arguments
                let arg_types: Vec<Type> = arguments
                    .iter()
                    .map(|arg| self.check_expression(arg))
                    .collect();
                
                // Look up the function to validate it exists and has the right arity
                match &callee.kind {
                    ExpressionKind::Identifier(ident) => {
                        if let Some(fn_info) = self.functions.get(&ident.0) {
                            // Check argument count
                            if fn_info.param_types.len() != arg_types.len() {
                                self.errors.push(TypeError {
                                    span: expr.span,
                                    kind: TypeErrorKind::ArgumentCountMismatch {
                                        expected: fn_info.param_types.len(),
                                        found: arg_types.len(),
                                    },
                                });
                                return Type::Unknown;
                            }
                            
                            // Check argument types
                            for (expected, actual) in fn_info.param_types.iter().zip(arg_types.iter()) {
                                if !self.types_compatible(expected, actual) {
                                    self.errors.push(TypeError {
                                        span: expr.span,
                                        kind: TypeErrorKind::TypeMismatch {
                                            expected: expected.clone(),
                                            found: actual.clone(),
                                        },
                                    });
                                }
                            }
                            
                            fn_info.return_type.clone()
                        } else {
                            self.errors.push(TypeError {
                                span: expr.span,
                                kind: TypeErrorKind::UndefinedFunction {
                                    name: ident.0.clone(),
                                },
                            });
                            Type::Unknown
                        }
                    }
                    _ => {
                        self.errors.push(TypeError {
                            span: callee.span,
                            kind: TypeErrorKind::UndefinedFunction {
                                name: "<unknown>".to_string(),
                            },
                        });
                        Type::Unknown
                    }
                }
            }
            ExpressionKind::Binary { op, left, right } => {
                let left_ty = self.check_expression(left);
                let right_ty = self.check_expression(right);
                match op {
                    crate::ast::BinaryOp::Add
                    | crate::ast::BinaryOp::Sub
                    | crate::ast::BinaryOp::Mul
                    | crate::ast::BinaryOp::Div => {
                        self.ensure_numeric(left_ty.clone(), left.span);
                        self.ensure_numeric(right_ty.clone(), right.span);
                        if left_ty == right_ty {
                            left_ty
                        } else {
                            Type::Unknown
                        }
                    }
                }
            }
            ExpressionKind::Unary { op, expr } => {
                let ty = self.check_expression(expr);
                match op {
                    UnaryOp::Negate => {
                        self.ensure_numeric(ty.clone(), expr.span);
                        ty
                    }
                    UnaryOp::Not => {
                        self.ensure_boolean(ty.clone(), expr.span);
                        Type::Bool
                    }
                }
            }
            ExpressionKind::Grouped(inner) => self.check_expression(inner),
            ExpressionKind::Logical { left, right, .. } => {
                let lhs = self.check_expression(left);
                let rhs = self.check_expression(right);
                self.ensure_boolean(lhs, left.span);
                self.ensure_boolean(rhs, right.span);
                Type::Bool
            }
            ExpressionKind::Comparison { left, right, op } => {
                let lhs = self.check_expression(left);
                let rhs = self.check_expression(right);
                // Equality (==, !=) can compare any types; relational (<, <=, >, >=) require numeric
                match op {
                    crate::ast::ComparisonOp::Equal | crate::ast::ComparisonOp::NotEqual => {
                        // Allow any types, but they should match
                        self.ensure_assignable(lhs, rhs, right.span);
                    }
                    _ => {
                        self.ensure_numeric(lhs, left.span);
                        self.ensure_numeric(rhs, right.span);
                    }
                }
                Type::Bool
            }
            ExpressionKind::RecordConstruct { name, fields } => {
                // Clone the record info to avoid borrow issues
                let record_info = self.records.get(&name.0).cloned();
                
                if let Some(record_info) = record_info {
                    // Check all provided field values
                    let mut provided_fields = std::collections::HashSet::new();
                    
                    for (field_name, value) in fields {
                        provided_fields.insert(field_name.0.clone());
                        
                        // Check if this field exists
                        if !record_info.fields.contains_key(&field_name.0) {
                            self.errors.push(TypeError {
                                span: expr.span,
                                kind: TypeErrorKind::UnexpectedRecordField {
                                    record: name.0.clone(),
                                    field: field_name.0.clone(),
                                },
                            });
                        }
                        
                        // Check the field type
                        let value_ty = self.check_expression(value);
                        if let Some(expected_ty) = record_info.fields.get(&field_name.0) {
                            if !self.types_compatible(expected_ty, &value_ty) {
                                self.errors.push(TypeError {
                                    span: value.span,
                                    kind: TypeErrorKind::TypeMismatch {
                                        expected: expected_ty.clone(),
                                        found: value_ty,
                                    },
                                });
                            }
                        }
                    }
                    
                    // Check for missing fields
                    for required_field in record_info.fields.keys() {
                        if !provided_fields.contains(required_field) {
                            self.errors.push(TypeError {
                                span: expr.span,
                                kind: TypeErrorKind::MissingRecordField {
                                    record: name.0.clone(),
                                    field: required_field.clone(),
                                },
                            });
                        }
                    }
                    
                    Type::Record(name.0.clone())
                } else {
                    self.errors.push(TypeError {
                        span: expr.span,
                        kind: TypeErrorKind::UndefinedRecord {
                            name: name.0.clone(),
                        },
                    });
                    Type::Unknown
                }
            }
            ExpressionKind::FieldAccess { object, .. } => {
                self.check_expression(object);
                // Return Unknown since we don't track record types yet
                Type::Unknown
            }
            ExpressionKind::ArrayLiteral(elements) => {
                // Check all elements have the same type
                let mut elem_type = Type::Unknown;
                for elem in elements {
                    let ty = self.check_expression(elem);
                    if elem_type == Type::Unknown {
                        elem_type = ty;
                    } else if ty != Type::Unknown {
                        self.ensure_assignable(elem_type.clone(), ty, elem.span);
                    }
                }
                // Return Unknown since we don't have array types yet
                Type::Unknown
            }
            ExpressionKind::Index { object, index } => {
                self.check_expression(object);
                let idx_ty = self.check_expression(index);
                self.ensure_numeric(idx_ty, index.span);
                Type::Unknown
            }
            ExpressionKind::Await(inner) => {
                // Await returns the inner future's result type
                self.check_expression(inner)
            }
            ExpressionKind::Start(inner) => {
                // Start spawns a task and returns a handle (Unknown for now)
                self.check_expression(inner);
                Type::Unknown
            }
        }
    }

    fn ensure_assignable(&mut self, expected: Type, found: Type, span: Span) {
        if expected == Type::Unknown || found == Type::Unknown {
            return;
        }
        if expected != found {
            self.errors.push(TypeError {
                span,
                kind: TypeErrorKind::TypeMismatch { expected, found },
            });
        }
    }

    fn ensure_numeric(&mut self, ty: Type, span: Span) {
        if !matches!(ty, Type::Number | Type::Unknown) {
            self.errors.push(TypeError {
                span,
                kind: TypeErrorKind::NonNumericOperand,
            });
        }
    }

    fn ensure_boolean(&mut self, ty: Type, span: Span) {
        if !matches!(ty, Type::Bool | Type::Unknown) {
            self.errors.push(TypeError {
                span,
                kind: TypeErrorKind::NonBooleanCondition,
            });
        }
    }

    fn resolve_type_expr(&self, ty: &TypeExpr) -> Type {
        match ty {
            TypeExpr::Named(ident) => match ident.0.to_lowercase().as_str() {
                "number" | "int32" | "int64" | "float64" => Type::Number,
                "text" | "string" => Type::Text,
                "bool" => Type::Bool,
                _ => {
                    // Check if it's a record name
                    if self.records.contains_key(&ident.0) {
                        Type::Record(ident.0.clone())
                    } else {
                        Type::Unknown
                    }
                }
            },
        }
    }
    
    /// Check if two types are compatible for assignment/comparison
    fn types_compatible(&self, expected: &Type, found: &Type) -> bool {
        if expected == &Type::Unknown || found == &Type::Unknown {
            return true;
        }
        expected == found
    }

    fn define(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

impl std::fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeErrorKind::UndefinedVariable { name } => {
                write!(f, "use of undefined variable '{name}'")
            }
            TypeErrorKind::UndefinedFunction { name } => {
                write!(f, "call to undefined function '{name}'")
            }
            TypeErrorKind::UndefinedRecord { name } => {
                write!(f, "use of undefined record type '{name}'")
            }
            TypeErrorKind::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {expected}, found {found}")
            }
            TypeErrorKind::MissingReturnValue { expected } => {
                write!(f, "missing return value of type {expected}")
            }
            TypeErrorKind::UnexpectedReturnValue => {
                write!(f, "function does not return a value but returned one")
            }
            TypeErrorKind::NonBooleanCondition => {
                write!(f, "condition must evaluate to a bool")
            }
            TypeErrorKind::NonNumericOperand => write!(f, "operand must be numeric"),
            TypeErrorKind::ArgumentCountMismatch { expected, found } => {
                write!(f, "argument count mismatch: expected {expected}, found {found}")
            }
            TypeErrorKind::MissingRecordField { record, field } => {
                write!(f, "record '{record}' is missing required field '{field}'")
            }
            TypeErrorKind::UnexpectedRecordField { record, field } => {
                write!(f, "record '{record}' does not have field '{field}'")
            }
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
