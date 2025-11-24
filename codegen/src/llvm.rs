use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::{
    CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FloatValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::OptimizationLevel;

use scribe_compiler::ir::{
    Instruction, IrBinaryOp, IrCompareOp, IrFunction, IrModule, Terminator, Value, ValueId,
};

use crate::{Backend, CodegenArtifact, CodegenError};

/// Stores both float values and string pointers for intrinsic arguments
enum LlvmValue<'ctx> {
    Float(FloatValue<'ctx>),
    Ptr(PointerValue<'ctx>),
}

pub struct LlvmBackend {
    target_machine: TargetMachine,
}

impl LlvmBackend {
    pub fn new(target_triple: Option<&str>) -> Result<Self, CodegenError> {
        Target::initialize_all(&InitializationConfig::default());
        let triple = target_triple
            .map(TargetTriple::create)
            .unwrap_or_else(TargetMachine::get_default_triple);
        let target = Target::from_triple(&triple)
            .map_err(|e| CodegenError::Llvm(format!("unknown target: {e}")))?;
        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| CodegenError::Llvm("failed to create target machine".into()))?;
        Ok(Self {
            target_machine: machine,
        })
    }
}

impl Backend for LlvmBackend {
    fn emit(&mut self, module_name: &str, ir: &IrModule) -> Result<CodegenArtifact, CodegenError> {
        let context = Context::create();
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        for function in &ir.functions {
            lower_function(&context, &module, &builder, function)?;
        }

        let buffer = self
            .target_machine
            .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
            .map_err(|e| CodegenError::Llvm(format!("failed to emit object: {e}")))?;

        Ok(CodegenArtifact {
            object: buffer.as_slice().to_vec(),
        })
    }
}

fn lower_function<'ctx>(
    context: &'ctx Context,
    module: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    function: &IrFunction,
) -> Result<(), CodegenError> {
    let float_ty = context.f64_type();
    let i32_ty = context.i32_type();

    // For main function, use i32 return type for C compatibility
    let is_main = function.name == "main";

    let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
        function.params.iter().map(|_| float_ty.into()).collect();

    let llvm_function = if is_main {
        // main() returns i32 for C ABI compatibility
        let fn_type = i32_ty.fn_type(&param_types, false);
        module.add_function("main", fn_type, Some(Linkage::External))
    } else {
        let fn_type = float_ty.fn_type(&param_types, false);
        module.add_function(&function.name, fn_type, None)
    };

    let mut block_map = HashMap::new();
    for block in &function.blocks {
        let name = format!("bb{}", block.id.0);
        let llvm_block = context.append_basic_block(llvm_function, &name);
        block_map.insert(block.id, llvm_block);
    }

    let mut float_values: HashMap<ValueId, FloatValue<'ctx>> = HashMap::new();
    let mut ptr_values: HashMap<ValueId, PointerValue<'ctx>> = HashMap::new();

    for (idx, arg) in llvm_function.get_param_iter().enumerate() {
        let value = arg.into_float_value();
        float_values.insert(ValueId(idx as u32), value);
    }

    for block in &function.blocks {
        let llvm_block = block_map
            .get(&block.id)
            .copied()
            .ok_or_else(|| CodegenError::Unsupported("missing block".into()))?;
        builder.position_at_end(llvm_block);

        for instr in &block.instructions {
            match instr {
                Instruction::Const { dest, value } => match value {
                    Value::Number(v) => {
                        float_values.insert(*dest, float_ty.const_float(*v));
                    }
                    Value::Bool(true) => {
                        float_values.insert(*dest, float_ty.const_float(1.0));
                    }
                    Value::Bool(false) => {
                        float_values.insert(*dest, float_ty.const_float(0.0));
                    }
                    Value::Text(text) => {
                        let global_str = builder.build_global_string_ptr(text, "str");
                        ptr_values.insert(*dest, global_str.as_pointer_value());
                    }
                },
                Instruction::Binary { dest, op, lhs, rhs } => {
                    let left = get_float_value(*lhs, &float_values)?;
                    let right = get_float_value(*rhs, &float_values)?;
                    let result = match op {
                        IrBinaryOp::Add => builder.build_float_add(left, right, "add"),
                        IrBinaryOp::Sub => builder.build_float_sub(left, right, "sub"),
                        IrBinaryOp::Mul => builder.build_float_mul(left, right, "mul"),
                        IrBinaryOp::Div => builder.build_float_div(left, right, "div"),
                        IrBinaryOp::And => {
                            let zero = context.f64_type().const_float(0.0);
                            let lhs_bool =
                                builder.build_float_compare(FloatPredicate::ONE, left, zero, "lhs");
                            let rhs_bool = builder.build_float_compare(
                                FloatPredicate::ONE,
                                right,
                                zero,
                                "rhs",
                            );
                            let result = builder.build_and(lhs_bool, rhs_bool, "and");
                            builder.build_unsigned_int_to_float(result, context.f64_type(), "andf")
                        }
                        IrBinaryOp::Or => {
                            let zero = context.f64_type().const_float(0.0);
                            let lhs_bool =
                                builder.build_float_compare(FloatPredicate::ONE, left, zero, "lhs");
                            let rhs_bool = builder.build_float_compare(
                                FloatPredicate::ONE,
                                right,
                                zero,
                                "rhs",
                            );
                            let result = builder.build_or(lhs_bool, rhs_bool, "or");
                            builder.build_unsigned_int_to_float(result, context.f64_type(), "orf")
                        }
                    };
                    float_values.insert(*dest, result);
                }
                Instruction::Call { .. } => {
                    return Err(CodegenError::Unsupported(
                        "function calls are not yet supported".into(),
                    ))
                }
                Instruction::Compare { dest, op, lhs, rhs } => {
                    let left = get_float_value(*lhs, &float_values)?;
                    let right = get_float_value(*rhs, &float_values)?;
                    let predicate = match op {
                        IrCompareOp::Equal => FloatPredicate::OEQ,
                        IrCompareOp::NotEqual => FloatPredicate::ONE,
                        IrCompareOp::Less => FloatPredicate::OLT,
                        IrCompareOp::LessEqual => FloatPredicate::OLE,
                        IrCompareOp::Greater => FloatPredicate::OGT,
                        IrCompareOp::GreaterEqual => FloatPredicate::OGE,
                    };
                    let cmp = builder.build_float_compare(predicate, left, right, "cmp");
                    let result =
                        builder.build_unsigned_int_to_float(cmp, context.f64_type(), "cmpf");
                    float_values.insert(*dest, result);
                }
                Instruction::Intrinsic { dest, name, args, .. } => {
                    if let (Some(dest_id), Some(result)) = (dest, emit_intrinsic(context, module, builder, name, args, &ptr_values, &float_values)?) {
                        float_values.insert(*dest_id, result);
                    }
                }
            }
        }

        if let Some(term) = block.terminator.clone() {
            match term {
                Terminator::Return(value) => {
                    if is_main {
                        // For main, convert float to i32
                        let float_val = match value {
                            Some(id) => get_float_value(id, &float_values)?,
                            None => float_ty.const_float(0.0),
                        };
                        let int_val = builder.build_float_to_signed_int(float_val, i32_ty, "ret_i32");
                        builder.build_return(Some(&int_val));
                    } else {
                        let ret_val = match value {
                            Some(id) => get_float_value(id, &float_values)?,
                            None => float_ty.const_float(0.0),
                        };
                        builder.build_return(Some(&ret_val));
                    }
                }
                Terminator::Jump(target) => {
                    let bb = block_map
                        .get(&target)
                        .copied()
                        .ok_or_else(|| CodegenError::Unsupported("missing jump target".into()))?;
                    builder.build_unconditional_branch(bb);
                }
                Terminator::Branch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    let cond_value = get_float_value(cond, &float_values)?;
                    let zero = float_ty.const_float(0.0);
                    let cmp =
                        builder.build_float_compare(FloatPredicate::ONE, cond_value, zero, "cond");
                    let then_block = block_map
                        .get(&then_bb)
                        .copied()
                        .ok_or_else(|| CodegenError::Unsupported("missing then block".into()))?;
                    let else_block = block_map
                        .get(&else_bb)
                        .copied()
                        .ok_or_else(|| CodegenError::Unsupported("missing else block".into()))?;
                    builder.build_conditional_branch(cmp, then_block, else_block);
                }
            }
        }
    }

    Ok(())
}

fn get_float_value<'ctx>(
    id: ValueId,
    values: &HashMap<ValueId, FloatValue<'ctx>>,
) -> Result<FloatValue<'ctx>, CodegenError> {
    values
        .get(&id)
        .copied()
        .ok_or(CodegenError::MissingValue(id.0))
}

fn emit_intrinsic<'ctx>(
    context: &'ctx Context,
    module: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    name: &str,
    args: &[ValueId],
    ptr_values: &HashMap<ValueId, PointerValue<'ctx>>,
    float_values: &HashMap<ValueId, FloatValue<'ctx>>,
) -> Result<Option<FloatValue<'ctx>>, CodegenError> {
    let float_ty = context.f64_type();
    let i32_ty = context.i32_type();
    let i8_ptr_ty = context.i8_type().ptr_type(AddressSpace::default());
    let void_ty = context.void_type();

    match name {
        "print" => {
            // Declare scribe_print
            let print_fn = module.get_function("scribe_print").unwrap_or_else(|| {
                let fn_type = void_ty.fn_type(&[i8_ptr_ty.into()], false);
                module.add_function("scribe_print", fn_type, Some(Linkage::External))
            });

            for arg_id in args {
                if let Some(ptr) = ptr_values.get(arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [(*ptr).into()];
                    builder.build_call(print_fn, &call_args, "print_call");
                }
            }
            Ok(None)
        }
        "println" => {
            // Declare scribe_println
            let println_fn = module.get_function("scribe_println").unwrap_or_else(|| {
                let fn_type = void_ty.fn_type(&[i8_ptr_ty.into()], false);
                module.add_function("scribe_println", fn_type, Some(Linkage::External))
            });

            for arg_id in args {
                if let Some(ptr) = ptr_values.get(arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [(*ptr).into()];
                    builder.build_call(println_fn, &call_args, "println_call");
                }
            }
            Ok(None)
        }
        "println_number" => {
            // Declare scribe_println_number
            let println_num_fn = module.get_function("scribe_println_number").unwrap_or_else(|| {
                let fn_type = void_ty.fn_type(&[float_ty.into()], false);
                module.add_function("scribe_println_number", fn_type, Some(Linkage::External))
            });

            if let Some(&arg_id) = args.first() {
                if let Some(&val) = float_values.get(&arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [val.into()];
                    builder.build_call(println_num_fn, &call_args, "println_num_call");
                }
            }
            Ok(None)
        }
        "time_sleep" => {
            // Declare scribe_time_sleep
            let sleep_fn = module.get_function("scribe_time_sleep").unwrap_or_else(|| {
                let fn_type = void_ty.fn_type(&[float_ty.into()], false);
                module.add_function("scribe_time_sleep", fn_type, Some(Linkage::External))
            });

            if let Some(&arg_id) = args.first() {
                if let Some(&val) = float_values.get(&arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [val.into()];
                    builder.build_call(sleep_fn, &call_args, "sleep_call");
                }
            }
            Ok(None)
        }
        "time_now" => {
            // Declare scribe_time_now
            let now_fn = module.get_function("scribe_time_now").unwrap_or_else(|| {
                let fn_type = float_ty.fn_type(&[], false);
                module.add_function("scribe_time_now", fn_type, Some(Linkage::External))
            });

            let result = builder.build_call(now_fn, &[], "now_call");
            let float_result = result
                .try_as_basic_value()
                .left()
                .and_then(|v| v.into_float_value().into());
            Ok(float_result)
        }
        "file_read" => {
            // Declare scribe_file_read
            let read_fn = module.get_function("scribe_file_read").unwrap_or_else(|| {
                let fn_type = i8_ptr_ty.fn_type(&[i8_ptr_ty.into()], false);
                module.add_function("scribe_file_read", fn_type, Some(Linkage::External))
            });

            if let Some(&arg_id) = args.first() {
                if let Some(&ptr) = ptr_values.get(&arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [ptr.into()];
                    builder.build_call(read_fn, &call_args, "file_read_call");
                }
            }
            Ok(None)
        }
        "file_write" => {
            // Declare scribe_file_write
            let write_fn = module.get_function("scribe_file_write").unwrap_or_else(|| {
                let fn_type = i32_ty.fn_type(&[i8_ptr_ty.into(), i8_ptr_ty.into()], false);
                module.add_function("scribe_file_write", fn_type, Some(Linkage::External))
            });

            if args.len() >= 2 {
                if let (Some(&path), Some(&data)) = (ptr_values.get(&args[0]), ptr_values.get(&args[1])) {
                    let call_args: [BasicMetadataValueEnum; 2] = [path.into(), data.into()];
                    builder.build_call(write_fn, &call_args, "file_write_call");
                }
            }
            Ok(None)
        }
        "http_get" => {
            // Declare scribe_http_get - returns a struct, but we'll just call it
            let get_fn = module.get_function("scribe_http_get").unwrap_or_else(|| {
                // For simplicity, return i32 (status code)
                let fn_type = i32_ty.fn_type(&[i8_ptr_ty.into()], false);
                module.add_function("scribe_http_get", fn_type, Some(Linkage::External))
            });

            if let Some(&arg_id) = args.first() {
                if let Some(&ptr) = ptr_values.get(&arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [ptr.into()];
                    builder.build_call(get_fn, &call_args, "http_get_call");
                }
            }
            Ok(None)
        }
        "string_length" => {
            // Declare scribe_string_length
            let len_fn = module.get_function("scribe_string_length").unwrap_or_else(|| {
                let fn_type = float_ty.fn_type(&[i8_ptr_ty.into()], false);
                module.add_function("scribe_string_length", fn_type, Some(Linkage::External))
            });

            if let Some(&arg_id) = args.first() {
                if let Some(&ptr) = ptr_values.get(&arg_id) {
                    let call_args: [BasicMetadataValueEnum; 1] = [ptr.into()];
                    let result = builder.build_call(len_fn, &call_args, "str_len_call");
                    let float_result = result
                        .try_as_basic_value()
                        .left()
                        .and_then(|v| v.into_float_value().into());
                    return Ok(float_result);
                }
            }
            Ok(None)
        }
        "string_concat" => {
            // Declare scribe_string_concat
            let concat_fn = module.get_function("scribe_string_concat").unwrap_or_else(|| {
                let fn_type = i8_ptr_ty.fn_type(&[i8_ptr_ty.into(), i8_ptr_ty.into()], false);
                module.add_function("scribe_string_concat", fn_type, Some(Linkage::External))
            });

            if args.len() >= 2 {
                if let (Some(&a), Some(&b)) = (ptr_values.get(&args[0]), ptr_values.get(&args[1])) {
                    let call_args: [BasicMetadataValueEnum; 2] = [a.into(), b.into()];
                    builder.build_call(concat_fn, &call_args, "str_concat_call");
                }
            }
            Ok(None)
        }
        "await" | "spawn" => {
            // These are runtime operations - for now, just return the argument
            if let Some(&arg_id) = args.first() {
                if let Some(&val) = float_values.get(&arg_id) {
                    return Ok(Some(val));
                }
            }
            Ok(None)
        }
        _ => {
            // Unknown intrinsic - try to call it as an external function
            Ok(None)
        }
    }
}
