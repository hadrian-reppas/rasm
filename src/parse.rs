use std::cell::{Cell, RefCell};
use std::collections::HashSet;

use crate::ast::{
    AssignOp, BinaryOp, Block, Expr, ForInit, Item, Name, PlaceExpr, Stmt, UnaryOp, UseTree,
    UseTreeKind,
};
use crate::error::Error;
use crate::io::{load_source, SourcePath};
use crate::lex::{Lexer, Token, TokenKind};

struct Parser<'a> {
    peek0: Token,
    peek1: Token,
    peek2: Token,
    lexer: Lexer,
    paths: &'a RefCell<HashSet<SourcePath>>,
    lambdas: Vec<Item>,
}

pub fn parse(
    code: &'static str,
    path: SourcePath,
    paths: &RefCell<HashSet<SourcePath>>,
) -> Result<Vec<Item>, Error> {
    let mut parser = Parser::new(code, path, paths)?;
    parser.items()
}

fn parse_mod(
    path: SourcePath,
    name: &Name,
    paths: &RefCell<HashSet<SourcePath>>,
) -> Result<Vec<Item>, Error> {
    let new_path = path.push(name)?;

    if paths.borrow().contains(&new_path) {
        return Err(Error::new(
            name.span,
            format!("{new_path:?} is already in module tree"),
        ));
    }
    paths.borrow_mut().insert(new_path);

    let code = load_source(new_path)?;
    parse(code, new_path, paths)
}

impl<'a> Parser<'a> {
    fn new(
        code: &'static str,
        path: SourcePath,
        paths: &'a RefCell<HashSet<SourcePath>>,
    ) -> Result<Self, Error> {
        let mut lexer = Lexer::new(code, path);
        Ok(Parser {
            peek0: lexer.next()?,
            peek1: lexer.next()?,
            peek2: lexer.next()?,
            lexer,
            paths,
            lambdas: Vec::new(),
        })
    }

    fn peek(&self) -> Token {
        self.peek0
    }

    fn peek1(&self) -> Token {
        self.peek1
    }

    fn peek2(&self) -> Token {
        self.peek2
    }

    fn next(&mut self) -> Result<Token, Error> {
        let next;
        (next, self.peek0, self.peek1, self.peek2) =
            (self.peek0, self.peek1, self.peek2, self.lexer.next()?);
        Ok(next)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        if self.peek().kind == kind {
            Ok(self.next()?)
        } else {
            Err(Error::new(self.peek().span, "unexpected token"))
        }
    }

    fn at_eof(&self) -> bool {
        self.peek0.kind == TokenKind::Eof
    }

    fn sequence<T>(
        &mut self,
        parse_item: impl Fn(&mut Self) -> Result<T, Error>,
        sep: TokenKind,
        end: TokenKind,
    ) -> Result<Vec<T>, Error> {
        if self.peek().kind == end {
            self.expect(end)?;
            return Ok(Vec::new());
        }
        let mut items = vec![parse_item(self)?];
        while self.peek().kind != end {
            self.expect(sep)?;
            items.push(parse_item(self)?);
        }
        self.expect(end)?;
        Ok(items)
    }

    fn name(&mut self) -> Result<Name, Error> {
        let token = self.expect(TokenKind::Name)?;
        Ok(Name {
            name: token.span.text.to_string(),
            span: token.span,
        })
    }

    fn path(&mut self) -> Result<(Vec<Name>, Name), Error> {
        let mut path = vec![self.name()?];
        while self.peek().kind == TokenKind::ColonColon {
            self.expect(TokenKind::ColonColon)?;
            path.push(self.name()?);
        }
        let last = path.pop().unwrap();
        Ok((path, last))
    }

    fn push_lambda(&mut self, name: Name, params: Vec<Name>, body: Expr) {
        self.lambdas.push(Item::Function {
            name,
            params,
            block: Block {
                stmts: Vec::new(),
                expr: Some(Box::new(body)),
            },
            id: None,
        });
    }

    fn items(&mut self) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        while !self.at_eof() {
            items.push(self.item()?);
            items.append(&mut self.lambdas);
        }
        Ok(items)
    }

    fn item(&mut self) -> Result<Item, Error> {
        match self.peek().kind {
            TokenKind::Fn => {
                self.expect(TokenKind::Fn)?;
                let name = self.name()?;
                self.expect(TokenKind::LeftParen)?;
                let params = self.sequence(Self::param, TokenKind::Comma, TokenKind::RightParen)?;
                if self.peek().kind == TokenKind::Arrow {
                    self.expect(TokenKind::Arrow)?;
                    self.ty()?;
                }
                let block = self.block(true)?;
                Ok(Item::Function {
                    name,
                    params,
                    block,
                    id: None,
                })
            }
            TokenKind::Let => {
                self.expect(TokenKind::Let)?;
                let name = self.param()?;
                self.expect(TokenKind::Assign)?;
                let expr = self.expr(BindingPower::Start, false)?;
                self.expect(TokenKind::Semi)?;
                Ok(Item::Static {
                    name,
                    expr,
                    id: None,
                })
            }
            TokenKind::Mod => {
                self.expect(TokenKind::Mod)?;
                let name = self.name()?;
                self.expect(TokenKind::Semi)?;
                Ok(Item::Mod {
                    items: parse_mod(self.lexer.path, &name, &self.paths)?,
                    name,
                })
            }
            TokenKind::Use => {
                self.expect(TokenKind::Use)?;
                let with_crate = if self.peek().kind == TokenKind::Crate {
                    self.expect(TokenKind::Crate)?;
                    self.expect(TokenKind::ColonColon)?;
                    true
                } else {
                    false
                };
                let tree = self.use_tree()?;
                self.expect(TokenKind::Semi)?;
                Ok(Item::Use {
                    with_crate,
                    tree,
                    done: Cell::new(false),
                })
            }
            TokenKind::Intrinsic => {
                self.expect(TokenKind::Intrinsic)?;
                let name = self.name()?;
                self.expect(TokenKind::Semi)?;
                Ok(Item::Intrinsic { name, id: None })
            }
            _ => Err(Error::new(
                self.peek().span,
                "expected `fn`, `let`, `use`, `mod` or `intrinsic`",
            )),
        }
    }

    fn param(&mut self) -> Result<Name, Error> {
        let name = self.name();
        if self.peek().kind == TokenKind::Colon {
            self.expect(TokenKind::Colon)?;
            self.ty()?;
        }
        name
    }

    fn ty(&mut self) -> Result<(), Error> {
        match self.peek().kind {
            TokenKind::Name => {
                self.expect(TokenKind::Name)?;
                if self.peek().kind == TokenKind::Lt {
                    self.expect(TokenKind::Lt)?;
                    self.sequence(Self::ty, TokenKind::Comma, TokenKind::Gt)?;
                }
                Ok(())
            }
            TokenKind::Star => {
                self.expect(TokenKind::Star)?;
                self.ty()
            }
            TokenKind::Fn => {
                self.expect(TokenKind::Fn)?;
                self.expect(TokenKind::LeftParen)?;
                self.sequence(Self::ty, TokenKind::Comma, TokenKind::RightParen)?;
                if self.peek().kind == TokenKind::Arrow {
                    self.expect(TokenKind::Arrow)?;
                    self.ty()?;
                }
                Ok(())
            }
            _ => Err(Error::new(self.peek().span, "expected type")),
        }
    }

    fn use_tree(&mut self) -> Result<UseTree, Error> {
        let mut prefix = vec![self.name()?];
        while self.peek().kind == TokenKind::ColonColon {
            self.expect(TokenKind::ColonColon)?;
            if self.peek().kind == TokenKind::Name {
                prefix.push(self.name()?);
            } else {
                self.expect(TokenKind::LeftBrace)?;
                let nested =
                    self.sequence(Self::use_tree, TokenKind::Comma, TokenKind::RightBrace)?;
                return Ok(UseTree {
                    prefix,
                    kind: UseTreeKind::Nested(nested),
                });
            }
        }
        Ok(UseTree {
            prefix,
            kind: UseTreeKind::Simple,
        })
    }

    fn block(&mut self, allow_return: bool) -> Result<Block, Error> {
        self.expect(TokenKind::LeftBrace)?;
        let mut stmts = Vec::new();

        while self.peek().kind != TokenKind::RightBrace {
            match self.peek().kind {
                TokenKind::Let => {
                    self.expect(TokenKind::Let)?;
                    let name = self.param()?;
                    self.expect(TokenKind::Assign)?;
                    let expr = self.expr(BindingPower::Start, allow_return)?;
                    self.expect(TokenKind::Semi)?;
                    stmts.push(Stmt::Let {
                        name,
                        expr,
                        local_id: None,
                    });
                }
                TokenKind::Semi => {
                    self.expect(TokenKind::Semi)?;
                }
                _ => {
                    let expr = self.expr(BindingPower::Start, allow_return)?;
                    match self.peek().kind {
                        TokenKind::Semi => {
                            self.expect(TokenKind::Semi)?;
                            stmts.push(Stmt::Expr(expr));
                        }
                        TokenKind::RightBrace => {
                            self.expect(TokenKind::RightBrace)?;
                            return Ok(Block {
                                stmts,
                                expr: Some(Box::new(expr)),
                            });
                        }
                        _ => {
                            if expr.ends_with_block() {
                                stmts.push(Stmt::Expr(expr));
                            } else {
                                return Err(Error::new(
                                    self.peek().span,
                                    "expected `;` after expression",
                                ));
                            }
                        }
                    }
                }
            }
        }

        self.expect(TokenKind::RightBrace)?;
        Ok(Block { stmts, expr: None })
    }

    fn expr(&mut self, bp: BindingPower, allow_return: bool) -> Result<Expr, Error> {
        macro_rules! uop {
            ($tok:ident, $op:ident) => {{
                self.expect(TokenKind::$tok)?;
                let expr = self.expr(BindingPower::Prefix, allow_return)?;
                Expr::Unary {
                    op: UnaryOp::$op,
                    expr: Box::new(expr),
                }
            }};
        }

        let mut lhs = match self.peek().kind {
            TokenKind::Int => Expr::Int(self.expect(TokenKind::Int)?.span.text.parse().unwrap()),
            TokenKind::Char => {
                Expr::Int(parse_char_literal(self.expect(TokenKind::Char)?.span.text) as i64)
            }
            TokenKind::True => {
                self.expect(TokenKind::True)?;
                Expr::Int(1)
            }
            TokenKind::False => {
                self.expect(TokenKind::False)?;
                Expr::Int(0)
            }
            TokenKind::Null => {
                self.expect(TokenKind::Null)?;
                Expr::Int(0)
            }
            TokenKind::String => Expr::String(parse_string_literal(
                self.expect(TokenKind::String)?.span.text,
            )),
            TokenKind::Name => {
                let (prefix, name) = self.path()?;
                Expr::Path {
                    with_crate: false,
                    prefix,
                    name,
                    variable: None,
                }
            }
            TokenKind::Crate => {
                self.expect(TokenKind::Crate)?;
                self.expect(TokenKind::ColonColon)?;
                let (prefix, name) = self.path()?;
                Expr::Path {
                    with_crate: true,
                    prefix,
                    name,
                    variable: None,
                }
            }
            TokenKind::LeftBrace => Expr::Block(self.block(allow_return)?),
            TokenKind::LeftParen => {
                self.expect(TokenKind::LeftParen)?;
                let expr = self.expr(BindingPower::Start, allow_return)?;
                self.expect(TokenKind::RightParen)?;
                expr
            }
            TokenKind::If => self.if_expr(allow_return)?,
            TokenKind::Return => {
                let token = self.expect(TokenKind::Return)?;
                if !allow_return {
                    return Err(Error::new(
                        token.span,
                        "`return` not allowed in static initializers",
                    ));
                }
                let expr = if self.peek().kind.is_expr_start() {
                    Some(Box::new(self.expr(BindingPower::Start, allow_return)?))
                } else {
                    None
                };
                Expr::Return(expr)
            }
            TokenKind::For => {
                self.expect(TokenKind::For)?;
                self.expect(TokenKind::LeftParen)?;
                let init = match self.peek().kind {
                    TokenKind::Semi => {
                        self.expect(TokenKind::Semi)?;
                        None
                    }
                    TokenKind::Let => {
                        self.expect(TokenKind::Let)?;
                        let name = self.param()?;
                        self.expect(TokenKind::Assign)?;
                        let expr = self.expr(BindingPower::Start, allow_return)?;
                        self.expect(TokenKind::Semi)?;
                        Some(ForInit::Let {
                            name,
                            expr: Box::new(expr),
                            local_id: None,
                        })
                    }
                    _ => {
                        let expr = self.expr(BindingPower::Start, allow_return)?;
                        self.expect(TokenKind::Semi)?;
                        Some(ForInit::Expr(Box::new(expr)))
                    }
                };
                let test = if self.peek().kind == TokenKind::Semi {
                    self.expect(TokenKind::Semi)?;
                    None
                } else {
                    let test = self.expr(BindingPower::Start, allow_return)?;
                    self.expect(TokenKind::Semi)?;
                    Some(Box::new(test))
                };
                let update = if self.peek().kind == TokenKind::RightParen {
                    self.expect(TokenKind::RightParen)?;
                    None
                } else {
                    let test = self.expr(BindingPower::Start, allow_return)?;
                    self.expect(TokenKind::RightParen)?;
                    Some(Box::new(test))
                };
                let block = self.block(allow_return)?;
                Expr::For {
                    init,
                    test,
                    update,
                    block,
                }
            }
            TokenKind::Dash => uop!(Dash, Negate),
            TokenKind::Tilde => uop!(Tilde, BitNot),
            TokenKind::Bang => uop!(Bang, LogicalNot),
            TokenKind::Star => uop!(Star, Deref),
            TokenKind::And => {
                let and = self.expect(TokenKind::And)?;
                let expr = self.expr(BindingPower::Prefix, allow_return)?;
                if let Ok(place) = expr.try_into() {
                    if matches!(place, PlaceExpr::Deref(_)) {
                        return Err(Error::new(
                            and.span,
                            "cannot take address of deref expression",
                        ));
                    }
                    Expr::AddrOf(place)
                } else {
                    return Err(Error::new(and.span, "target is not a place expression"));
                }
            }
            TokenKind::Or => {
                let token = self.expect(TokenKind::Or)?;
                let params = self.sequence(Self::param, TokenKind::Comma, TokenKind::Or)?;
                let body = self.expr(BindingPower::Start, true)?;
                let name = Name {
                    name: format!("$lambda_{}_{}", token.span.line, token.span.column),
                    span: token.span,
                };
                self.push_lambda(name.clone(), params, body);
                Expr::Path {
                    with_crate: false,
                    prefix: Vec::new(),
                    name,
                    variable: None,
                }
            }
            _ => {
                return Err(Error::new(
                    self.peek().span,
                    "unexpected token in expression",
                ))
            }
        };

        while let Some(info) = self.peek_op(bp) {
            lhs = match info {
                OpInfo::Binary(op, new_bp) => {
                    for _ in 0..op.num_tokens() {
                        self.next()?;
                    }
                    let rhs = self.expr(new_bp, allow_return)?;
                    Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                }
                OpInfo::AssignOp(op) => {
                    let assign = self.next()?;
                    let rhs = self.expr(BindingPower::Assign, allow_return)?;
                    let Ok(target) = lhs.try_into() else {
                        return Err(Error::new(assign.span, "target is not a place expression"));
                    };
                    Expr::AssignOp {
                        op,
                        target,
                        rhs: Box::new(rhs),
                    }
                }
                OpInfo::Assign => {
                    let assign = self.expect(TokenKind::Assign)?;
                    let rhs = self.expr(BindingPower::Assign, allow_return)?;
                    let Ok(target) = lhs.try_into() else {
                        return Err(Error::new(assign.span, "target is not a place expression"));
                    };
                    Expr::Assign {
                        target,
                        rhs: Box::new(rhs),
                    }
                }
                OpInfo::Call => {
                    self.expect(TokenKind::LeftParen)?;
                    let args = self.sequence(
                        |parser| parser.expr(BindingPower::Start, allow_return),
                        TokenKind::Comma,
                        TokenKind::RightParen,
                    )?;
                    Expr::Call {
                        func: Box::new(lhs),
                        args,
                    }
                }
                OpInfo::Index => {
                    self.expect(TokenKind::LeftBrack)?;
                    let index = self.expr(BindingPower::Start, allow_return)?;
                    self.expect(TokenKind::RightBrack)?;
                    Expr::Index {
                        target: Box::new(lhs),
                        index: Box::new(index),
                    }
                }
            };
        }

        Ok(lhs)
    }

    fn peek_op(&self, bp: BindingPower) -> Option<OpInfo> {
        macro_rules! bop {
            ($op:ident, $bp:ident) => {
                if BindingPower::$bp <= bp {
                    None
                } else {
                    Some(OpInfo::Binary(BinaryOp::$op, BindingPower::$bp))
                }
            };
        }

        macro_rules! assign_op {
            ($op:ident) => {
                if BindingPower::Assign < bp {
                    None
                } else {
                    Some(OpInfo::AssignOp(AssignOp::$op))
                }
            };
        }

        match self.peek().kind {
            TokenKind::Star => bop!(Mul, Product),
            TokenKind::Slash => bop!(Div, Product),
            TokenKind::Percent => bop!(Mod, Product),
            TokenKind::Plus => bop!(Add, Sum),
            TokenKind::Dash => bop!(Sub, Sum),
            TokenKind::Shl => bop!(Shl, Shift),
            TokenKind::Lt => bop!(Lt, Cmp),
            TokenKind::Le => bop!(Le, Cmp),
            TokenKind::Gt => {
                if self.peek1().kind == TokenKind::Gt
                    && self.peek().span.adjacent(self.peek1().span)
                    && self.peek2().kind == TokenKind::Gt
                    && self.peek1().span.adjacent(self.peek2().span)
                {
                    bop!(LogicalShr, Shift)
                } else if self.peek1().kind == TokenKind::Gt
                    && self.peek().span.adjacent(self.peek1().span)
                {
                    bop!(ArithmeticShr, Shift)
                } else {
                    bop!(Gt, Cmp)
                }
            }
            TokenKind::Ge => bop!(Ge, Cmp),
            TokenKind::Eq => bop!(Eq, Eq),
            TokenKind::Ne => bop!(Ne, Eq),
            TokenKind::And => bop!(BitAnd, BitAnd),
            TokenKind::Xor => bop!(BitXor, BitXor),
            TokenKind::Or => bop!(BitOr, BitOr),
            TokenKind::AndAnd => bop!(LogicalAnd, LogicalAnd),
            TokenKind::OrOr => bop!(LogicalOr, LogicalOr),
            TokenKind::Assign => {
                if BindingPower::Assign < bp {
                    None
                } else {
                    Some(OpInfo::Assign)
                }
            }
            TokenKind::AddAssign => assign_op!(Add),
            TokenKind::SubAssign => assign_op!(Sub),
            TokenKind::MulAssign => assign_op!(Mul),
            TokenKind::DivAssign => assign_op!(Div),
            TokenKind::ModAssign => assign_op!(Mod),
            TokenKind::ShlAssign => assign_op!(Shl),
            TokenKind::ArithmeticShrAssign => assign_op!(ArithmeticShr),
            TokenKind::LogicalShrAssign => assign_op!(LogicalShr),
            TokenKind::AndAssign => assign_op!(BitAnd),
            TokenKind::XorAssign => assign_op!(BitXor),
            TokenKind::OrAssign => assign_op!(BitOr),
            TokenKind::LeftParen => Some(OpInfo::Call),
            TokenKind::LeftBrack => Some(OpInfo::Index),
            _ => None,
        }
    }

    fn if_expr(&mut self, allow_return: bool) -> Result<Expr, Error> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LeftParen)?;
        let test = self.expr(BindingPower::Start, allow_return)?;
        self.expect(TokenKind::RightParen)?;
        let if_block = self.block(allow_return)?;

        let else_block = if self.peek().kind == TokenKind::Else {
            self.expect(TokenKind::Else)?;
            if self.peek().kind == TokenKind::If {
                let expr = self.if_expr(allow_return)?;
                Some(Block {
                    stmts: Vec::new(),
                    expr: Some(Box::new(expr)),
                })
            } else {
                Some(self.block(allow_return)?)
            }
        } else {
            None
        };

        Ok(Expr::If {
            test: Box::new(test),
            if_block,
            else_block,
        })
    }
}

#[derive(Debug, Clone, Copy)]
enum OpInfo {
    Binary(BinaryOp, BindingPower),
    AssignOp(AssignOp),
    Assign,
    Call,
    Index,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BindingPower {
    Start,
    Assign,
    LogicalOr,
    LogicalAnd,
    Cmp,
    Eq,
    BitOr,
    BitXor,
    BitAnd,
    Shift,
    Sum,
    Product,
    Prefix,
}

fn parse_char_literal(literal: &str) -> char {
    handle_escapes(&literal[1..literal.len() - 1])
        .chars()
        .next()
        .unwrap()
}

fn parse_string_literal(literal: &str) -> String {
    handle_escapes(&literal[1..literal.len() - 1])
}

fn handle_escapes(inner: &str) -> String {
    let mut chars = inner.chars();
    let mut string = String::new();
    while let Some(c) = chars.next() {
        if c == '\\' {
            let unescaped = match chars.next().unwrap() {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                '\'' => '\'',
                '"' => '"',
                _ => unreachable!(),
            };
            string.push(unescaped);
        } else {
            string.push(c);
        }
    }
    string
}
