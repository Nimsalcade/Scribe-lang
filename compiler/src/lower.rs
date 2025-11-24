use std::collections::HashMap;

use crate::ast::{
    Block, ComparisonOp, Expression, ExpressionKind, ForIterator, ForStatement, Function,
    IfStatement, Literal, LogicalOp, Module, Statement, TypeExpr, UnaryOp, WhileStatement,
};
use crate::ir::{
    BlockId, FunctionBuilder, IrBinaryOp, IrCompareOp, IrModule, Terminator, Value, ValueId,
};
use crate::typeck::Type;

#[derive(Clone)]
struct Env {
    scopes: Vec<HashMap<String, ValueId>>,
}

impl Env {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, value: ValueId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    fn assign(&mut self, name: &str, value: ValueId) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return;
            }
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value);
        }
    }

    fn lookup(&self, name: &str) -> Option<ValueId> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(*value);
            }
        }
        None
    }
}

pub fn lower_module(module: &Module) -> IrModule {
    let mut ir = IrModule::new();
    for function in &module.functions {
        let func_ir = lower_function(function);
        ir.add_function(func_ir);
    }
    ir
}

fn lower_function(function: &Function) -> crate::ir::IrFunction {
    let param_types = function
        .signature
        .params
        .iter()
        .map(|param| resolve_type(&param.ty))
        .collect();
    let mut builder = FunctionBuilder::new(function.name.0.clone(), param_types);
    let entry = builder.entry();

    let mut env = Env::new();
    for (idx, param) in function.signature.params.iter().enumerate() {
        env.insert(param.name.0.clone(), ValueId(idx as u32));
    }

    let exit_block = lower_block(&function.body, &mut builder, entry, &mut env);
    if let Some(block) = exit_block {
        builder.terminate(block, Terminator::Return(None));
    }

    builder.build()
}

fn lower_block(
    statements: &Block,
    builder: &mut FunctionBuilder,
    start_block: BlockId,
    env: &mut Env,
) -> Option<BlockId> {
    env.push_scope();
    let mut current_block = Some(start_block);
    for stmt in statements {
        if let Some(block_id) = current_block {
            current_block = lower_statement(stmt, builder, block_id, env);
        } else {
            break;
        }
    }
    env.pop_scope();
    current_block
}

fn lower_statement(
    statement: &Statement,
    builder: &mut FunctionBuilder,
    block: BlockId,
    env: &mut Env,
) -> Option<BlockId> {
    match statement {
        Statement::Let(stmt) => {
            let value_id = stmt
                .value
                .as_ref()
                .map(|expr| lower_expression(expr, builder, block, env))
                .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0)));
            env.insert(stmt.name.0.clone(), value_id);
            Some(block)
        }
        Statement::Assignment { target, value, .. } => {
            let value_id = lower_expression(value, builder, block, env);
            env.assign(&target.0, value_id);
            Some(block)
        }
        Statement::Expression(expr) => {
            lower_expression(expr, builder, block, env);
            Some(block)
        }
        Statement::Return { value, .. } => {
            let result = value
                .as_ref()
                .map(|expr| lower_expression(expr, builder, block, env));
            builder.terminate(block, Terminator::Return(result));
            None
        }
        Statement::If(if_stmt) => lower_if(if_stmt, builder, block, env),
        Statement::For(for_stmt) => lower_for(for_stmt, builder, block, env),
        Statement::While(while_stmt) => lower_while(while_stmt, builder, block, env),
        Statement::Break { .. } | Statement::Continue { .. } => {
            // Break/continue require loop context tracking - for now, just continue
            Some(block)
        }
    }
}

fn lower_if(
    if_stmt: &IfStatement,
    builder: &mut FunctionBuilder,
    current_block: BlockId,
    env: &Env,
) -> Option<BlockId> {
    let cond = lower_expression(&if_stmt.condition, builder, current_block, env);

    let then_block = builder.append_block();
    let else_block = builder.append_block();
    builder.terminate(
        current_block,
        Terminator::Branch {
            cond,
            then_bb: then_block,
            else_bb: else_block,
        },
    );

    let mut merge_needed = false;
    let merge_block = builder.append_block();

    let mut then_env = env.clone();
    if let Some(exit) = lower_block(&if_stmt.then_block, builder, then_block, &mut then_env) {
        builder.terminate(exit, Terminator::Jump(merge_block));
        merge_needed = true;
    }

    if let Some(else_block_ast) = &if_stmt.else_block {
        let mut else_env = env.clone();
        if let Some(exit) = lower_block(else_block_ast, builder, else_block, &mut else_env) {
            builder.terminate(exit, Terminator::Jump(merge_block));
            merge_needed = true;
        }
    } else {
        builder.terminate(else_block, Terminator::Jump(merge_block));
        merge_needed = true;
    }

    if merge_needed {
        Some(merge_block)
    } else {
        None
    }
}

fn lower_for(
    for_stmt: &ForStatement,
    builder: &mut FunctionBuilder,
    current_block: BlockId,
    env: &Env,
) -> Option<BlockId> {
    let mut env = env.clone();
    env.push_scope();

    match &for_stmt.iterator {
        ForIterator::Range {
            start,
            end,
            inclusive,
        } => {
            // Initialize loop variable
            let start_val = lower_expression(start, builder, current_block, &env);
            let end_val = lower_expression(end, builder, current_block, &env);
            env.insert(for_stmt.variable.0.clone(), start_val);

            // Create blocks
            let cond_block = builder.append_block();
            let body_block = builder.append_block();
            let incr_block = builder.append_block();
            let exit_block = builder.append_block();

            // Jump to condition check
            builder.terminate(current_block, Terminator::Jump(cond_block));

            // Condition block: check if i < end (or i <= end for inclusive)
            let loop_var = env.lookup(&for_stmt.variable.0).unwrap_or(start_val);
            let cmp_op = if *inclusive {
                IrCompareOp::LessEqual
            } else {
                IrCompareOp::Less
            };
            let cond = builder.emit_compare(cond_block, cmp_op, loop_var, end_val);
            builder.terminate(
                cond_block,
                Terminator::Branch {
                    cond,
                    then_bb: body_block,
                    else_bb: exit_block,
                },
            );

            // Body block
            let mut body_env = env.clone();
            if let Some(body_exit) = lower_block(&for_stmt.body, builder, body_block, &mut body_env)
            {
                builder.terminate(body_exit, Terminator::Jump(incr_block));
            }

            // Increment block: i = i + 1
            let one = builder.emit_const(incr_block, Value::Number(1.0));
            let current_val = body_env
                .lookup(&for_stmt.variable.0)
                .unwrap_or(start_val);
            let new_val = builder.emit_binary(incr_block, IrBinaryOp::Add, current_val, one);
            // Note: In a proper implementation, we'd need phi nodes or memory for the loop variable
            // For now, we just jump back to condition
            let _ = new_val;
            builder.terminate(incr_block, Terminator::Jump(cond_block));

            Some(exit_block)
        }
        ForIterator::Collection(expr) => {
            // Collection iteration is more complex - for now, just execute body once
            lower_expression(expr, builder, current_block, &env);
            let exit = lower_block(&for_stmt.body, builder, current_block, &mut env);
            exit
        }
    }
}

fn lower_while(
    while_stmt: &WhileStatement,
    builder: &mut FunctionBuilder,
    current_block: BlockId,
    env: &Env,
) -> Option<BlockId> {
    let cond_block = builder.append_block();
    let body_block = builder.append_block();
    let exit_block = builder.append_block();

    // Jump to condition check
    builder.terminate(current_block, Terminator::Jump(cond_block));

    // Condition block
    let cond = lower_expression(&while_stmt.condition, builder, cond_block, env);
    builder.terminate(
        cond_block,
        Terminator::Branch {
            cond,
            then_bb: body_block,
            else_bb: exit_block,
        },
    );

    // Body block
    let mut body_env = env.clone();
    if let Some(body_exit) = lower_block(&while_stmt.body, builder, body_block, &mut body_env) {
        builder.terminate(body_exit, Terminator::Jump(cond_block));
    }

    Some(exit_block)
}

fn lower_expression(
    expr: &Expression,
    builder: &mut FunctionBuilder,
    block: BlockId,
    env: &Env,
) -> ValueId {
    match &expr.kind {
        ExpressionKind::Identifier(ident) => env
            .lookup(&ident.0)
            .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0))),
        ExpressionKind::Literal(lit) => {
            let value = match lit {
                Literal::Number(num) => Value::Number(num.parse().unwrap_or(0.0)),
                Literal::Bool(b) => Value::Bool(*b),
                Literal::Text(text) => Value::Text(text.clone()),
            };
            builder.emit_const(block, value)
        }
        ExpressionKind::Binary { op, left, right } => {
            let lhs = lower_expression(left, builder, block, env);
            let rhs = lower_expression(right, builder, block, env);
            let ir_op = match op {
                crate::ast::BinaryOp::Add => IrBinaryOp::Add,
                crate::ast::BinaryOp::Sub => IrBinaryOp::Sub,
                crate::ast::BinaryOp::Mul => IrBinaryOp::Mul,
                crate::ast::BinaryOp::Div => IrBinaryOp::Div,
            };
            builder.emit_binary(block, ir_op, lhs, rhs)
        }
        ExpressionKind::Unary { op, expr } => {
            let value = lower_expression(expr, builder, block, env);
            match op {
                UnaryOp::Negate => {
                    let neg_one = builder.emit_const(block, Value::Number(-1.0));
                    builder.emit_binary(block, IrBinaryOp::Mul, value, neg_one)
                }
                UnaryOp::Not => {
                    let false_val = builder.emit_const(block, Value::Bool(false));
                    builder.emit_compare(block, IrCompareOp::Equal, value, false_val)
                }
            }
        }
        ExpressionKind::Call { callee, arguments } => {
            let args: Vec<_> = arguments
                .iter()
                .map(|arg| lower_expression(arg, builder, block, env))
                .collect();
            if let ExpressionKind::Identifier(name) = &callee.kind {
                // Check for intrinsic functions
                if is_intrinsic(&name.0) {
                    builder
                        .emit_intrinsic(block, name.0.clone(), args, false)
                        .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0)))
                } else {
                    builder
                        .emit_call(block, name.0.clone(), args, true)
                        .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0)))
                }
            } else {
                lower_expression(callee, builder, block, env)
            }
        }
        ExpressionKind::Grouped(inner) => lower_expression(inner, builder, block, env),
        ExpressionKind::Logical { op, left, right } => {
            let lhs = lower_expression(left, builder, block, env);
            let rhs = lower_expression(right, builder, block, env);
            let ir_op = match op {
                LogicalOp::And => IrBinaryOp::And,
                LogicalOp::Or => IrBinaryOp::Or,
            };
            builder.emit_binary(block, ir_op, lhs, rhs)
        }
        ExpressionKind::Comparison { op, left, right } => {
            let lhs = lower_expression(left, builder, block, env);
            let rhs = lower_expression(right, builder, block, env);
            let cmp_op = match op {
                ComparisonOp::Equal => IrCompareOp::Equal,
                ComparisonOp::NotEqual => IrCompareOp::NotEqual,
                ComparisonOp::Less => IrCompareOp::Less,
                ComparisonOp::LessEqual => IrCompareOp::LessEqual,
                ComparisonOp::Greater => IrCompareOp::Greater,
                ComparisonOp::GreaterEqual => IrCompareOp::GreaterEqual,
            };
            builder.emit_compare(block, cmp_op, lhs, rhs)
        }
        ExpressionKind::RecordConstruct { fields, .. } => {
            // For now, just evaluate all field values and return 0
            // Proper record support would need struct types in IR
            for (_, value) in fields {
                lower_expression(value, builder, block, env);
            }
            builder.emit_const(block, Value::Number(0.0))
        }
        ExpressionKind::FieldAccess { object, .. } => {
            // For now, just evaluate the object and return 0
            lower_expression(object, builder, block, env);
            builder.emit_const(block, Value::Number(0.0))
        }
        ExpressionKind::ArrayLiteral(elements) => {
            // For now, just evaluate all elements and return 0
            for elem in elements {
                lower_expression(elem, builder, block, env);
            }
            builder.emit_const(block, Value::Number(0.0))
        }
        ExpressionKind::Index { object, index } => {
            // For now, just evaluate object and index and return 0
            lower_expression(object, builder, block, env);
            lower_expression(index, builder, block, env);
            builder.emit_const(block, Value::Number(0.0))
        }
        ExpressionKind::Await(inner) => {
            // Emit await as an intrinsic call
            let inner_val = lower_expression(inner, builder, block, env);
            builder
                .emit_intrinsic(block, "await".to_string(), vec![inner_val], false)
                .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0)))
        }
        ExpressionKind::Start(inner) => {
            // Emit start/spawn as an intrinsic call
            let inner_val = lower_expression(inner, builder, block, env);
            builder
                .emit_intrinsic(block, "spawn".to_string(), vec![inner_val], false)
                .unwrap_or_else(|| builder.emit_const(block, Value::Number(0.0)))
        }
    }
}

fn resolve_type(ty: &TypeExpr) -> Type {
    match ty {
        TypeExpr::Named(ident) => match ident.0.to_lowercase().as_str() {
            "number" | "int32" | "int64" | "float64" => Type::Number,
            "text" | "string" => Type::Text,
            "bool" => Type::Bool,
            _ => Type::Unknown,
        },
    }
}

/// Check if a function name is an intrinsic
fn is_intrinsic(name: &str) -> bool {
    matches!(
        name,
        "print"
            | "println"
            | "file_read"
            | "file_write"
            | "file_append"
            | "file_exists"
            | "http_get"
            | "http_post"
            | "http_put"
            | "http_delete"
            | "time_sleep"
            | "time_now"
            | "await"
            | "spawn"
            | "println_number"
            | "print_number"
    )
}
