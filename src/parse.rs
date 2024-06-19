use crate::ast::{
    AssignOp, BinaryOp, Block, ElseIf, Expr, ForInit, Item, Name, PlaceExpr, Stmt, UnaryOp,
};
use crate::error::Error;
use crate::lex::{Lexer, Token, TokenKind};

struct Parser {
    peek: Token,
    lexer: Lexer,
}

pub fn parse(code: &'static str) -> Result<Vec<Item>, Error> {
    let mut parser = Parser::new(code)?;
    parser.items()
}

impl Parser {
    fn new(code: &'static str) -> Result<Self, Error> {
        let mut lexer = Lexer::new(code);
        Ok(Parser {
            peek: lexer.next()?,
            lexer,
        })
    }

    fn peek(&self) -> Token {
        self.peek
    }

    fn next(&mut self) -> Result<Token, Error> {
        let next = self.peek;
        self.peek = self.lexer.next()?;
        Ok(next)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, Error> {
        if self.peek().kind == kind {
            Ok(self.next()?)
        } else {
            Err(Error {
                msg: "unexpected token".to_string(),
                span: self.peek().span,
            })
        }
    }

    fn at_eof(&self) -> bool {
        self.peek.kind == TokenKind::Eof
    }

    fn name(&mut self) -> Result<Name, Error> {
        let token = self.expect(TokenKind::Name)?;
        Ok(Name {
            name: token.span.text.to_string(),
            span: token.span,
            variable_id: None,
        })
    }

    fn items(&mut self) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        while !self.at_eof() {
            items.push(self.item()?);
        }
        Ok(items)
    }

    fn item(&mut self) -> Result<Item, Error> {
        match self.peek().kind {
            TokenKind::Fn => {
                self.expect(TokenKind::Fn)?;
                let name = self.name()?;
                self.expect(TokenKind::LeftParen)?;
                let params = if self.peek().kind == TokenKind::RightParen {
                    self.expect(TokenKind::RightParen)?;
                    Vec::new()
                } else {
                    let mut params = vec![self.name()?];
                    while self.peek().kind == TokenKind::Comma {
                        self.expect(TokenKind::Comma)?;
                        params.push(self.name()?);
                    }
                    self.expect(TokenKind::RightParen)?;
                    params
                };
                let block = self.block(true)?;
                Ok(Item::Function {
                    name,
                    params,
                    block,
                })
            }
            TokenKind::Let => {
                self.expect(TokenKind::Let)?;
                let name = self.name()?;
                self.expect(TokenKind::Assign)?;
                let expr = self.expr(BindingPower::Start, false)?;
                self.expect(TokenKind::Semi)?;
                Ok(Item::Static { name, expr })
            }
            _ => Err(Error {
                msg: "expected `fn` or `let`".to_string(),
                span: self.peek().span,
            }),
        }
    }

    fn block(&mut self, allow_return: bool) -> Result<Block, Error> {
        self.expect(TokenKind::LeftBrace)?;
        let mut stmts = Vec::new();

        while self.peek().kind != TokenKind::RightBrace {
            match self.peek().kind {
                TokenKind::Let => {
                    self.expect(TokenKind::Let)?;
                    let name = self.name()?;
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
                                return Err(Error {
                                    msg: "expected `;` after expression".to_string(),
                                    span: self.peek().span,
                                });
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
            TokenKind::Name => Expr::Name(self.name()?),
            TokenKind::LeftBrace => Expr::Block(self.block(allow_return)?),
            TokenKind::LeftParen => {
                self.expect(TokenKind::LeftParen)?;
                let expr = self.expr(BindingPower::Start, allow_return)?;
                self.expect(TokenKind::RightParen)?;
                expr
            }
            TokenKind::If => {
                let (test, if_block) = self.if_test_and_block(allow_return)?;
                let mut else_ifs = Vec::new();
                let mut else_block = None;
                while self.peek().kind == TokenKind::Else {
                    self.expect(TokenKind::Else)?;
                    if self.peek().kind == TokenKind::If {
                        let (test, block) = self.if_test_and_block(allow_return)?;
                        else_ifs.push(ElseIf { test, block });
                    } else {
                        else_block = Some(self.block(allow_return)?);
                        break;
                    }
                }
                Expr::If {
                    test: Box::new(test),
                    if_block,
                    else_ifs,
                    else_block,
                }
            }
            TokenKind::Return => {
                let token = self.expect(TokenKind::Return)?;
                if !allow_return {
                    return Err(Error {
                        msg: "`return` not allowed in static initializers".to_string(),
                        span: token.span,
                    });
                }
                let expr = if self.peek().kind.is_expr_start() {
                    None
                } else {
                    Some(Box::new(self.expr(BindingPower::Start, allow_return)?))
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
                        let name = self.name()?;
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
                        return Err(Error {
                            msg: "cannot take address of deref expression".to_string(),
                            span: and.span,
                        });
                    }
                    Expr::AddrOf(place)
                } else {
                    return Err(Error {
                        msg: "target is not an lvalue".to_string(),
                        span: and.span,
                    });
                }
            }
            _ => {
                return Err(Error {
                    msg: "unexpected token in expression".to_string(),
                    span: self.peek().span,
                })
            }
        };

        while let Some(info) = self.peek_op(bp) {
            lhs = match info {
                OpInfo::Binary(op, new_bp) => {
                    self.next()?;
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
                        return Err(Error {
                            msg: "target is not an lvalue".to_string(),
                            span: assign.span,
                        });
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
                        return Err(Error {
                            msg: "target is not an lvalue".to_string(),
                            span: assign.span,
                        });
                    };
                    Expr::Assign {
                        target,
                        rhs: Box::new(rhs),
                    }
                }
                OpInfo::Call => {
                    self.expect(TokenKind::LeftParen)?;
                    let args = if self.peek().kind == TokenKind::RightParen {
                        Vec::new()
                    } else {
                        let mut args = vec![self.expr(BindingPower::Start, allow_return)?];
                        while self.peek().kind != TokenKind::RightParen {
                            self.expect(TokenKind::Comma)?;
                            args.push(self.expr(BindingPower::Start, allow_return)?);
                        }
                        args
                    };
                    self.expect(TokenKind::RightParen)?;
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
            TokenKind::ArithmeticShr => bop!(ArithmeticShr, Shift),
            TokenKind::LogicalShr => bop!(LogicalShr, Shift),
            TokenKind::Lt => bop!(Lt, Cmp),
            TokenKind::Le => bop!(Le, Cmp),
            TokenKind::Gt => bop!(Gt, Cmp),
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

    fn if_test_and_block(&mut self, allow_return: bool) -> Result<(Expr, Block), Error> {
        self.expect(TokenKind::If)?;
        self.expect(TokenKind::LeftParen)?;
        let test = self.expr(BindingPower::Start, allow_return)?;
        self.expect(TokenKind::RightParen)?;
        let then = self.block(allow_return)?;
        Ok((test, then))
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
