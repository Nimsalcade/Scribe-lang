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
