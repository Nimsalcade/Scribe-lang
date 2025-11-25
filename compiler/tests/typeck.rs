use scribe_compiler::{parser::Parser, typeck};

fn parse_module(source: &str) -> scribe_compiler::ast::Module {
    let mut parser = Parser::new(source);
    parser.parse_module().expect("parse module")
}

#[test]
fn typechecks_simple_function() {
    let module = parse_module(
        "fn add(a: number, b: number) -> number:\n    let sum = a + b\n    return sum\n",
    );
    assert!(typeck::check_module(&module).is_ok());
}

#[test]
fn detects_mismatched_return_type() {
    let module = parse_module("fn greet() -> number:\n    return \"hi\"\n");
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}

#[test]
fn typechecks_boolean_logic() {
    let module =
        parse_module("fn check(a: bool, b: bool) -> bool:\n    return a and (not b) or (a == b)\n");
    assert!(typeck::check_module(&module).is_ok());
}

#[test]
fn detects_non_boolean_condition() {
    let module = parse_module(
        "fn bad() -> int32:\n    if 1:\n        return 0\n    else:\n        return 1\n",
    );
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}

#[test]
fn typechecks_function_call() {
    let module = parse_module(
        r#"fn add(a: number, b: number) -> number:
    return a + b

fn main() -> number:
    let result = add(1, 2)
    return result
"#,
    );
    assert!(typeck::check_module(&module).is_ok());
}

#[test]
fn detects_undefined_function_call() {
    let module = parse_module(
        r#"fn main() -> number:
    let result = undefined_func(1, 2)
    return result
"#,
    );
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}

#[test]
fn detects_function_argument_count_mismatch() {
    let module = parse_module(
        r#"fn add(a: number, b: number) -> number:
    return a + b

fn main() -> number:
    let result = add(1)
    return result
"#,
    );
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}

#[test]
fn typechecks_record_construction() {
    let module = parse_module(
        r#"record Point(x: number, y: number)

fn main() -> number:
    let p = Point(x = 0, y = 0)
    return 0
"#,
    );
    assert!(typeck::check_module(&module).is_ok());
}

#[test]
fn detects_missing_record_field() {
    let module = parse_module(
        r#"record Point(x: number, y: number)

fn main() -> number:
    let p = Point(x = 0)
    return 0
"#,
    );
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}

#[test]
fn detects_unexpected_record_field() {
    let module = parse_module(
        r#"record Point(x: number, y: number)

fn main() -> number:
    let p = Point(x = 0, y = 0, z = 0)
    return 0
"#,
    );
    let result = typeck::check_module(&module);
    assert!(result.is_err());
}
