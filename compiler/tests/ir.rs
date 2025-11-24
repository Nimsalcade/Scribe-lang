use scribe_compiler::ir::{FunctionBuilder, IrBinaryOp, IrModule, Terminator, Value};
use scribe_compiler::typeck::Type;

#[test]
fn builds_ir_function_with_blocks() {
    let mut builder = FunctionBuilder::new("add", vec![Type::Number, Type::Number]);
    let entry = builder.entry();
    let lhs = builder.emit_const(entry, Value::Number(1.0));
    let rhs = builder.emit_const(entry, Value::Number(2.0));
    let sum = builder.emit_binary(entry, IrBinaryOp::Add, lhs, rhs);
    builder.terminate(entry, Terminator::Return(Some(sum)));

    let function = builder.build();
    assert_eq!(function.blocks.len(), 1);
    assert_eq!(function.blocks[0].instructions.len(), 3);
    assert!(matches!(
        function.blocks[0].terminator,
        Some(Terminator::Return(_))
    ));

    let mut module = IrModule::new();
    module.add_function(function);
    assert_eq!(module.functions.len(), 1);
}
