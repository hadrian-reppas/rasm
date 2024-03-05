use std::collections::HashSet;

use crate::ast::{AssignOp, BinaryOp, UnaryOp};
use crate::error::Span;
use crate::resolve::{FunctionId, Local, StackId, StaticId, StringId, TransientId};

#[derive(Debug, Clone)]
pub struct Function {
    pub prefix: Vec<String>,
    pub name: String,
    pub span: Span,
    pub id: FunctionId,
    pub params: Vec<Local>,
    pub block: Block,
    pub transient_locals: usize,
    pub stack_locals: usize,
    pub static_dependencies: HashSet<StaticId>,
    pub function_dependencies: HashSet<FunctionId>,
}

impl Function {
    pub fn path(&self) -> String {
        let mut path = self.prefix.join(".");
        path.push('.');
        path.push_str(&self.name);
        path
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    pub prefix: Vec<String>,
    pub name: String,
    pub span: Span,
    pub id: StaticId,
    pub expr: Expr,
    pub transient_locals: usize,
    pub stack_locals: usize,
    pub static_dependencies: HashSet<StaticId>,
    pub function_dependencies: HashSet<FunctionId>,
}

impl Static {
    pub fn path(&self) -> String {
        let mut path = self.prefix.join(".");
        path.push('.');
        path.push_str(&self.name);
        path
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(StringId),
    Int(i64),
    Static(StaticId),
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
        else_block: Option<Block>,
    },
    For {
        init: Option<Box<Expr>>,
        test: Option<Box<Expr>>,
        update: Option<Box<Expr>>,
        block: Block,
    },
    Return(Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum AddrOfExpr {
    Static(StaticId),
    Stack(StackId),
    Index { target: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug, Clone)]
pub enum AssignTargetExpr {
    Static(StaticId),
    Stack(StackId),
    Transient(TransientId),
    Deref(Box<Expr>),
    Index { target: Box<Expr>, index: Box<Expr> },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Expr>,
    pub expr: Option<Box<Expr>>,
}
