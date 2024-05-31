use crate::ast::{AssignOp, BinaryOp, UnaryOp};
use crate::error::Span;
use crate::resolve::{FunctionId, GlobalId, Local, StackId, StringId, TransientId};

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Global(Global),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub span: Span,
    pub id: FunctionId,
    pub params: Vec<Local>,
    pub block: Block,
    pub transient_locals: u32,
    pub stack_locals: u32,
    pub global_dependencies: Vec<GlobalId>,
    pub function_dependencies: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub name: String,
    pub span: Span,
    pub id: GlobalId,
    pub expr: Expr,
    pub transient_locals: u32,
    pub stack_locals: u32,
    pub global_dependencies: Vec<GlobalId>,
    pub function_dependencies: Vec<FunctionId>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(StringId),
    Int(i64),
    Global(GlobalId),
    Function(FunctionId),
    Stack(StackId),
    Transient(TransientId),
    Block(Block),
    AddrOf(AddrOfExpr),
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Assign {
        target: AssignTargetExpr,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: AssignOp,
        target: AssignTargetExpr,
        rhs: Box<Expr>,
    },
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    If {
        test: Box<Expr>,
        if_block: Block,
        else_ifs: Vec<ElseIf>,
        else_block: Option<Block>,
    },
    For {
        init: Option<ForInit>,
        test: Option<Box<Expr>>,
        update: Option<Box<Expr>>,
        block: Block,
    },
    Return(Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum AddrOfExpr {
    Global(GlobalId),
    Stack(StackId),
    Index { target: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug, Clone)]
pub enum AssignTargetExpr {
    Global(GlobalId),
    Stack(StackId),
    Transient(TransientId),
    Deref(Box<Expr>),
    Index { target: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug, Clone)]
pub struct ElseIf {
    pub test: Expr,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let { id: Local, expr: Expr },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Let { id: Local, expr: Box<Expr> },
    Expr(Box<Expr>),
}
