use crate::resolve::{FunctionId, StackId, StaticId, StringId};

pub type ValueId = usize;
pub type BlockId = usize;

#[derive(Debug, Clone)]
pub struct Function {
    pub function_params: usize,
    pub stack_variables: usize,
    pub blocks: Vec<BasicBlock>,
}

impl Function {
    pub fn print(&self) {
        for (id, block) in self.blocks.iter().enumerate() {
            block.print(id);
        }
    }
}

fn print_tuple(values: &[ValueId]) {
    print!("(");
    for (i, arg) in values.iter().enumerate() {
        if i == 0 {
            print!("v{arg}");
        } else {
            print!(", v{arg}")
        }
    }
    print!(")");
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub params: Vec<ValueId>,
    pub instrs: Vec<Instr>,
    pub terminator: Terminator,
}

impl BasicBlock {
    fn print(&self, id: usize) {
        print!("block{id}");
        print_tuple(&self.params);
        println!();
        for instr in &self.instrs {
            instr.print();
        }
        self.terminator.print();
    }
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
    Load {
        target: ValueId,
        addr: ValueId,
    },
    Store {
        addr: ValueId,
        value: ValueId,
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

impl Instr {
    fn print(&self) {
        match self {
            Instr::Call {
                target,
                function,
                args,
            } => {
                print!("    v{target} = call v{function} ");
                print_tuple(args);
                println!();
            }
            Instr::StackLoad { target, slot } => println!("    v{target} = stack_load {slot}"),
            Instr::StackStore { slot, value } => println!("    stack_store {slot} v{value}"),
            Instr::StackAddr { target, slot } => println!("    v{target} = stack_addr {slot}"),
            Instr::StaticLoad { target, static_ } => {
                println!("    v{target} = static_load {static_}")
            }
            Instr::StaticStore { static_, value } => {
                println!("    static_store {static_} v{value}")
            }
            Instr::StaticAddr { target, static_ } => {
                println!("    v{target} = static_addr {static_}")
            }
            Instr::Load { target, addr } => println!("    v{target} = load v{addr}"),
            Instr::Store { addr, value } => println!("    store v{addr} v{value}"),
            Instr::String { target, string } => println!("    v{target} = string {string}"),
            Instr::Function { target, function } => println!("    v{target} = function {function}"),
            Instr::Const { target, value } => println!("    v{target} = const {value}"),
            Instr::Unary { target, value, op } => {
                println!("    v{target} = {} v{value}", op.abbrev())
            }
            Instr::Binary {
                target,
                lhs,
                rhs,
                op,
            } => println!("    v{target} = {} v{lhs} v{rhs}", op.abbrev()),
            Instr::Cmp {
                target,
                lhs,
                rhs,
                op,
            } => println!("    v{target} = {} v{lhs} v{rhs}", op.abbrev()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    BitNot,
    LogicalNot,
    Test,
}

impl UnaryOp {
    fn abbrev(self) -> &'static str {
        match self {
            UnaryOp::Neg => "neg",
            UnaryOp::BitNot => "bit_not",
            UnaryOp::LogicalNot => "logical_not",
            UnaryOp::Test => "test",
        }
    }
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

impl BinaryOp {
    fn abbrev(self) -> &'static str {
        match self {
            BinaryOp::Add => "add",
            BinaryOp::Sub => "sub",
            BinaryOp::Mul => "mul",
            BinaryOp::Div => "div",
            BinaryOp::Mod => "mod",
            BinaryOp::Shl => "shl",
            BinaryOp::ArithmeticShr => "arithmetic_shr",
            BinaryOp::LogicalShr => "logical_shr",
            BinaryOp::BitAnd => "bit_and",
            BinaryOp::BitXor => "bit_xor",
            BinaryOp::BitOr => "bit_or",
        }
    }
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

impl CmpOp {
    fn abbrev(self) -> &'static str {
        match self {
            CmpOp::Lt => "lt",
            CmpOp::Le => "le",
            CmpOp::Gt => "gt",
            CmpOp::Ge => "ge",
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
        }
    }
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
    fn print(&self) {
        match self {
            Terminator::Return(value) => println!("    return v{value}"),
            Terminator::Jump { block, args } => {
                print!("    jump block{block} ");
                print_tuple(args);
                println!();
            }
            Terminator::Branch {
                test,
                if_block,
                if_args,
                else_block,
                else_args,
            } => {
                print!("    branch v{test} block{if_block} ");
                print_tuple(if_args);
                print!(" block{else_block} ");
                print_tuple(else_args);
                println!();
            }
        }
    }

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

    pub fn get_jump_value(&self, index: usize) -> ValueId {
        let Terminator::Jump { args, .. } = self else {
            panic!()
        };
        args[index]
    }

    pub fn get_branch_if_value(&self, index: usize) -> ValueId {
        let Terminator::Branch { if_args, .. } = self else {
            panic!()
        };
        if_args[index]
    }

    pub fn get_branch_else_value(&self, index: usize) -> ValueId {
        let Terminator::Branch { else_args, .. } = self else {
            panic!()
        };
        else_args[index]
    }
}
