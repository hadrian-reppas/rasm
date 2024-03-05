use std::cell::Cell;

use crate::error::Span;
use crate::resolve::{FunctionId, LocalId, StaticId, Variable};

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        name: Name,
        params: Vec<Name>,
        block: Block,
        id: Option<FunctionId>,
    },
    Static {
        name: Name,
        expr: Expr,
        id: Option<StaticId>,
    },
    Mod {
        name: Name,
        items: Vec<Item>,
    },
    Use {
        with_crate: bool,
        tree: UseTree,
        done: Cell<bool>,
    },
}

#[derive(Debug, Clone)]
pub struct UseTree {
    pub prefix: Vec<Name>,
    pub kind: UseTreeKind,
}

#[derive(Debug, Clone)]
pub enum UseTreeKind {
    Simple,
    Nested(Vec<UseTree>),
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Path {
        with_crate: bool,
        prefix: Vec<Name>,
        name: Name,
        variable: Option<Variable>,
    },
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
pub enum PlaceExpr {
    Path {
        with_crate: bool,
        prefix: Vec<Name>,
        name: Name,
        variable: Option<Variable>,
    },
    Deref(Box<Expr>),
    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },
}

impl TryFrom<Expr> for PlaceExpr {
    type Error = ();
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Path {
                with_crate,
                prefix,
                name,
                variable,
            } => Ok(PlaceExpr::Path {
                with_crate,
                prefix,
                name,
                variable,
            }),
            Expr::Unary {
                op: UnaryOp::Deref,
                expr,
            } => Ok(PlaceExpr::Deref(expr)),
            Expr::Index { target, index } => Ok(PlaceExpr::Index { target, index }),
            _ => Err(()),
        }
    }
}

impl Expr {
    pub fn ends_with_block(&self) -> bool {
        matches!(self, Expr::Block(_) | Expr::If { .. } | Expr::For { .. })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    ArithmeticShr,
    LogicalShr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    BitNot,
    LogicalNot,
    Deref,
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    ArithmeticShr,
    LogicalShr,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: Name,
        expr: Expr,
        local_id: Option<LocalId>,
    },
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum ForInit {
    Let {
        name: Name,
        expr: Box<Expr>,
        local_id: Option<LocalId>,
    },
    Expr(Box<Expr>),
}
