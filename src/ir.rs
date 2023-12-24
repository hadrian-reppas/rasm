use crate::resolve::{FunctionId, StackId, StaticId, StringId};

pub type ValueId = usize;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub struct Function {
    pub function_params: usize,
    pub stack_variables: usize,
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub params: Vec<ValueId>,
    pub instrs: Vec<Instr>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Call {
        target: ValueId,
        function: ValueId,
        args: Vec<ValueId>,
    },
    StackLoad {
        target: ValueId,
        slot: StackId,
    },
    StackStore {
        slot: StackId,
        value: ValueId,
    },
    StackAddr {
        target: ValueId,
        slot: StackId,
    },
    StaticLoad {
        target: ValueId,
        static_: StaticId,
    },
    StaticStore {
        static_: StaticId,
        value: ValueId,
    },
    StaticAddr {
        target: ValueId,
        static_: StaticId,
    },
    String {
        target: ValueId,
        string: StringId,
    },
    Function {
        target: ValueId,
        function: FunctionId,
    },
    Const {
        target: ValueId,
        value: i64,
    },
    Unary {
        target: ValueId,
        value: ValueId,
        op: UnaryOp,
    },
    Binary {
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: BinaryOp,
    },
    Cmp {
        target: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: CmpOp,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogicalNot,
    Deref,
    Test,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    ArithmeticShr,
    LogicalShr,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return(ValueId),
    Jump {
        block: BlockId,
        args: Vec<ValueId>,
    },
    Branch {
        test: ValueId,
        if_block: BlockId,
        if_args: Vec<ValueId>,
        else_block: BlockId,
        else_args: Vec<ValueId>,
    },
}

impl Terminator {
    pub fn push_jump_value(&mut self, value: ValueId) {
        let Terminator::Jump { args, .. } = self else {
            panic!()
        };
        args.push(value);
    }

    pub fn push_branch_if_value(&mut self, value: ValueId) {
        let Terminator::Branch { if_args, .. } = self else {
            panic!()
        };
        if_args.push(value);
    }

    pub fn push_branch_else_value(&mut self, value: ValueId) {
        let Terminator::Branch { else_args, .. } = self else {
            panic!()
        };
        else_args.push(value);
    }
}
