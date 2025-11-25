use scribe_compiler::{lower, parser::Parser};

fn parse(source: &str) -> scribe_compiler::ast::Module {
    let mut parser = Parser::new(source);
    parser.parse_module().expect("parse module")
}

#[test]
fn lowers_simple_function_to_ir() {
    let module =
        parse("fn add(a: number, b: number) -> number:\n    let sum = a + b\n    return sum\n");
    let ir = lower::lower_module(&module);
    assert_eq!(ir.functions.len(), 1);
    let func = &ir.functions[0];
    assert_eq!(func.blocks.len(), 1);
    assert!(func.blocks[0].terminator.is_some());
    assert!(
        func.blocks[0].instructions.len() >= 1,
        "expected at least one instruction, got {}",
        func.blocks[0].instructions.len()
    );
}

#[test]
fn lowers_if_statement() {
    let module = parse(
        "fn clamp(a: number) -> number:\n    if a > 0:\n        return a\n    else:\n        return 0\n",
    );
    let ir = lower::lower_module(&module);
    let func = &ir.functions[0];
    assert!(func.blocks.len() >= 3);
}

#[test]
fn async_function_has_flag_set() {
    let module = parse("async fn timer() -> number:\n    return 0\n");
    let ir = lower::lower_module(&module);
    assert_eq!(ir.functions.len(), 1);
    let func = &ir.functions[0];
    assert!(func.is_async, "expected async flag to be set on async function");
}

#[test]
fn regular_function_has_flag_unset() {
    let module = parse("fn timer() -> number:\n    return 0\n");
    let ir = lower::lower_module(&module);
    assert_eq!(ir.functions.len(), 1);
    let func = &ir.functions[0];
    assert!(!func.is_async, "expected async flag to be unset on regular function");
}

#[test]
fn async_function_with_await_compiles() {
    let module = parse(
        "async fn countdown() -> number:\n    await println(42)\n    return 0\n",
    );
    let ir = lower::lower_module(&module);
    assert_eq!(ir.functions.len(), 1);
    let func = &ir.functions[0];
    assert!(func.is_async);
    assert!(func.blocks.len() >= 1);
    // Check that await was lowered to an intrinsic
    let has_await_intrinsic = func.blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| {
            matches!(instr, scribe_compiler::ir::Instruction::Intrinsic { name, .. } if name == "await")
        })
    });
    assert!(
        has_await_intrinsic,
        "expected await to be lowered to an intrinsic"
    );
}
