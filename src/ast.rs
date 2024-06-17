use crate::error::Span;
use crate::resolve::{LocalId, Variable};

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        name: Name,
        params: Vec<Name>,
        block: Block,
    },
    Static {
        name: Name,
        expr: Expr,
    },
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: String,
    pub span: Span,
    pub variable_id: Option<Variable>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    String(String),
    Int(i64),
    Name(Name),
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
    Name(Name),
    Deref(Box<Expr>),
    Index { target: Box<Expr>, index: Box<Expr> },
}

impl TryFrom<Expr> for PlaceExpr {
    type Error = ();
    fn try_from(expr: Expr) -> Result<Self, Self::Error> {
        match expr {
            Expr::Name(name) => Ok(PlaceExpr::Name(name)),
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
        matches!(self, Expr::Block(_) | Expr::If { .. })
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
