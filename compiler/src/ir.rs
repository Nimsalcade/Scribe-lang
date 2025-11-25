use crate::typeck::Type;

#[derive(Debug, Default, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
}

impl IrModule {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: IrFunction) {
        self.functions.push(function);
    }
}

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<Type>,
    pub blocks: Vec<BasicBlock>,
    pub is_async: bool,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            terminator: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Const {
        dest: ValueId,
        value: Value,
    },
    Binary {
        dest: ValueId,
        op: IrBinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    Call {
        dest: Option<ValueId>,
        callee: String,
        args: Vec<ValueId>,
    },
    Compare {
        dest: ValueId,
        op: IrCompareOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    /// Intrinsic function call (e.g., print, file.read, http.get)
    Intrinsic {
        dest: Option<ValueId>,
        name: String,
        args: Vec<ValueId>,
    },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(Option<ValueId>),
    Jump(BlockId),
    Branch {
        cond: ValueId,
        then_bb: BlockId,
        else_bb: BlockId,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ValueId(pub u32);

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Text(String),
}

#[derive(Debug, Copy, Clone)]
pub enum IrBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug, Copy, Clone)]
pub enum IrCompareOp {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

pub struct FunctionBuilder {
    func: IrFunction,
    next_block: u32,
    next_value: u32,
}

impl FunctionBuilder {
    pub fn new(name: impl Into<String>, params: Vec<Type>) -> Self {
        Self::new_with_async(name, params, false)
    }

    pub fn new_with_async(name: impl Into<String>, params: Vec<Type>, is_async: bool) -> Self {
        let entry = BasicBlock::new(BlockId(0));
        let param_count = params.len() as u32;
        Self {
            func: IrFunction {
                name: name.into(),
                params,
                blocks: vec![entry],
                is_async,
            },
            next_block: 1,
            next_value: param_count,
        }
    }

    pub fn entry(&self) -> BlockId {
        BlockId(0)
    }

    pub fn append_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.func.blocks.push(BasicBlock::new(id));
        id
    }

    pub fn emit_const(&mut self, block: BlockId, value: Value) -> ValueId {
        let dest = self.next_value_id();
        self.block_mut(block)
            .instructions
            .push(Instruction::Const { dest, value });
        dest
    }

    pub fn emit_binary(
        &mut self,
        block: BlockId,
        op: IrBinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> ValueId {
        let dest = self.next_value_id();
        self.block_mut(block)
            .instructions
            .push(Instruction::Binary { dest, op, lhs, rhs });
        dest
    }

    pub fn emit_compare(
        &mut self,
        block: BlockId,
        op: IrCompareOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> ValueId {
        let dest = self.next_value_id();
        self.block_mut(block)
            .instructions
            .push(Instruction::Compare { dest, op, lhs, rhs });
        dest
    }

    pub fn emit_call(
        &mut self,
        block: BlockId,
        callee: impl Into<String>,
        args: Vec<ValueId>,
        capture_result: bool,
    ) -> Option<ValueId> {
        let dest = capture_result.then(|| self.next_value_id());
        self.block_mut(block).instructions.push(Instruction::Call {
            dest,
            callee: callee.into(),
            args,
        });
        dest
    }

    pub fn emit_intrinsic(
        &mut self,
        block: BlockId,
        name: impl Into<String>,
        args: Vec<ValueId>,
        capture_result: bool,
    ) -> Option<ValueId> {
        let dest = capture_result.then(|| self.next_value_id());
        self.block_mut(block)
            .instructions
            .push(Instruction::Intrinsic {
                dest,
                name: name.into(),
                args,
            });
        dest
    }

    pub fn terminate(&mut self, block: BlockId, terminator: Terminator) {
        let blk = self.block_mut(block);
        blk.terminator = Some(terminator);
    }

    pub fn build(self) -> IrFunction {
        self.func
    }

    fn block_mut(&mut self, block: BlockId) -> &mut BasicBlock {
        self.func
            .blocks
            .iter_mut()
            .find(|b| b.id == block)
            .expect("invalid block id")
    }

    fn next_value_id(&mut self) -> ValueId {
        let id = ValueId(self.next_value);
        self.next_value += 1;
        id
    }
}
