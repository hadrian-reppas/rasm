use crate::ast::{AssignOp, BinaryOp, UnaryOp};
use crate::resolve::{FunctionId, GlobalId, Local, StackId, TransientId};

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        name: String,
        id: FunctionId,
        params: Vec<Local>,
        block: Block,
        stack_locals: u32,
    },
    Global {
        name: String,
        id: GlobalId,
        expr: Expr,
        stack_locals: u32,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Global(GlobalId),
    Function(FunctionId),
    Stack(StackId),
    Transient(TransientId),
    Block(Block),
    AddrOf(PlaceExpr),
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
        target: PlaceExpr,
        rhs: Box<Expr>,
    },
    AssignOp {
        op: AssignOp,
        target: PlaceExpr,
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
}

#[derive(Debug, Clone)]
pub enum PlaceExpr {
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
    Let {
        id: Local,
        expr: Expr,
    },
    Return(Option<Expr>),
    For {
        init: Option<ForInit>,
        test: Option<Expr>,
        update: Option<Expr>,
        block: Block,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Let { id: Local, expr: Expr },
    Expr(Expr),
}
