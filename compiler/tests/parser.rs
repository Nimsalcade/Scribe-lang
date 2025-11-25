use scribe_compiler::ast::{ExpressionKind, ForIterator, Literal, Statement};
use scribe_compiler::parser::Parser;

#[test]
fn parses_use_statements() {
    let source = r#"use std.io
use http.server as server
use json

fn main() -> number:
    return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse module with uses");

    assert_eq!(module.uses.len(), 3);

    // First use: std.io
    assert_eq!(module.uses[0].path.segments.len(), 2);
    assert_eq!(module.uses[0].path.segments[0].0, "std");
    assert_eq!(module.uses[0].path.segments[1].0, "io");
    assert!(module.uses[0].alias.is_none());

    // Second use: http.server as server
    assert_eq!(module.uses[1].path.segments.len(), 2);
    assert_eq!(module.uses[1].path.segments[0].0, "http");
    assert_eq!(module.uses[1].path.segments[1].0, "server");
    assert!(module.uses[1].alias.is_some());
    assert_eq!(module.uses[1].alias.as_ref().unwrap().0, "server");

    // Third use: json (single segment)
    assert_eq!(module.uses[2].path.segments.len(), 1);
    assert_eq!(module.uses[2].path.segments[0].0, "json");

    assert_eq!(module.functions.len(), 1);
}

#[test]
fn parses_module_declaration() {
    let source = r#"module my.cool.module

fn main() -> number:
    return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse module with decl");

    assert!(module.module_decl.is_some());
    let decl = module.module_decl.as_ref().unwrap();
    assert_eq!(decl.path.segments.len(), 3);
    assert_eq!(decl.path.to_string(), "my.cool.module");
}

#[test]
fn parses_multiple_functions() {
    let source = r#"fn add(a: number, b: number) -> number:
    return a

fn main() -> int32:
    let mutable total: int32 = 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse module");
    assert_eq!(module.functions.len(), 2);

    let add_fn = &module.functions[0];
    assert_eq!(add_fn.name.0, "add");
    assert_eq!(add_fn.signature.params.len(), 2);
    assert!(add_fn.signature.return_type.is_some());
    assert_eq!(add_fn.body.len(), 1);
    match &add_fn.body[0] {
        Statement::Return {
            value: Some(expr), ..
        } => match &expr.kind {
            ExpressionKind::Identifier(ident) => assert_eq!(ident.0, "a"),
            other => panic!("expected identifier expression, got {:?}", other),
        },
        other => panic!("expected return statement, got {:?}", other),
    }

    let main_fn = &module.functions[1];
    assert_eq!(main_fn.name.0, "main");
    assert!(main_fn.signature.params.is_empty());
    assert_eq!(main_fn.body.len(), 1);
    match &main_fn.body[0] {
        Statement::Let(let_stmt) => {
            assert!(let_stmt.mutable);
            assert!(let_stmt.value.is_some());
        }
        other => panic!("expected let statement, got {:?}", other),
    }
}

#[test]
fn parses_async_function_flag() {
    let source = "async fn fetch() -> Result:\n    return 1\n";
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse async function");
    assert_eq!(module.functions.len(), 1);
    assert!(module.functions[0].is_async);
    match &module.functions[0].body[0] {
        Statement::Return {
            value: Some(expr), ..
        } => match &expr.kind {
            ExpressionKind::Literal(Literal::Number(n)) => assert_eq!(n, "1"),
            _ => panic!("expected numeric literal"),
        },
        other => panic!("expected return statement, got {:?}", other),
    }
}

#[test]
fn parses_if_with_logical_ops() {
    let source = "fn main() -> int32:\n    if true and not false:\n        return 1\n    else:\n        return 0\n";
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse if");
    let func = &module.functions[0];
    assert_eq!(func.body.len(), 1);
    match &func.body[0] {
        Statement::If(stmt) => {
            assert!(stmt.else_block.is_some());
        }
        other => panic!("expected if statement, got {:?}", other),
    }
}

#[test]
fn parses_for_range_loop() {
    let source = r#"fn main() -> number:
    for i in 0 to 10:
        return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse for range loop");
    assert_eq!(module.functions.len(), 1);
    
    let func = &module.functions[0];
    assert_eq!(func.body.len(), 1);
    
    match &func.body[0] {
        Statement::For(for_stmt) => {
            assert_eq!(for_stmt.variable.0, "i");
            match &for_stmt.iterator {
                ForIterator::Range { start, end, inclusive } => {
                    assert!(!inclusive);
                    // Verify start is 0
                    match &start.kind {
                        ExpressionKind::Literal(Literal::Number(n)) => assert_eq!(n, "0"),
                        _ => panic!("expected numeric literal for range start"),
                    }
                    // Verify end is 10
                    match &end.kind {
                        ExpressionKind::Literal(Literal::Number(n)) => assert_eq!(n, "10"),
                        _ => panic!("expected numeric literal for range end"),
                    }
                }
                other => panic!("expected Range iterator, got {:?}", other),
            }
        }
        other => panic!("expected For statement, got {:?}", other),
    }
}

#[test]
fn parses_for_range_loop_inclusive() {
    let source = r#"fn main() -> number:
    for i in 0 to 10 inclusive:
        return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse for range loop inclusive");
    assert_eq!(module.functions.len(), 1);
    
    let func = &module.functions[0];
    match &func.body[0] {
        Statement::For(for_stmt) => {
            match &for_stmt.iterator {
                ForIterator::Range { inclusive, .. } => {
                    assert!(inclusive);
                }
                other => panic!("expected Range iterator, got {:?}", other),
            }
        }
        other => panic!("expected For statement, got {:?}", other),
    }
}

#[test]
fn parses_for_collection_loop() {
    let source = r#"fn main() -> number:
    for each item in items:
        return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse for collection loop");
    assert_eq!(module.functions.len(), 1);
    
    let func = &module.functions[0];
    match &func.body[0] {
        Statement::For(for_stmt) => {
            assert_eq!(for_stmt.variable.0, "item");
            match &for_stmt.iterator {
                ForIterator::Collection(expr) => {
                    match &expr.kind {
                        ExpressionKind::Identifier(ident) => assert_eq!(ident.0, "items"),
                        _ => panic!("expected identifier expression for collection"),
                    }
                }
                other => panic!("expected Collection iterator, got {:?}", other),
            }
        }
        other => panic!("expected For statement, got {:?}", other),
    }
}

#[test]
fn parses_for_collection_loop_without_each() {
    let source = r#"fn main() -> number:
    for item in items:
        return 0
"#;
    let mut parser = Parser::new(source);
    let module = parser.parse_module().expect("parse for collection loop without each");
    assert_eq!(module.functions.len(), 1);
    
    let func = &module.functions[0];
    match &func.body[0] {
        Statement::For(for_stmt) => {
            assert_eq!(for_stmt.variable.0, "item");
            match &for_stmt.iterator {
                ForIterator::Collection(_) => { /* OK */ }
                other => panic!("expected Collection iterator, got {:?}", other),
            }
        }
        other => panic!("expected For statement, got {:?}", other),
    }
}
