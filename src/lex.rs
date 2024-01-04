use crate::error::{Error, Span};

pub struct Lexer {
    suffix: &'static str,
    code: &'static str,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(code: &'static str) -> Self {
        Lexer {
            suffix: code,
            code,
            line: 1,
            column: 0,
        }
    }

    fn make_span(&mut self, len: usize) -> Span {
        let span = Span {
            text: &self.suffix[..len],
            code: self.code,
            line: self.line,
            column: self.column,
        };

        self.suffix = &self.suffix[len..];
        for c in span.text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        span
    }

    fn make_token(&mut self, kind: TokenKind, len: usize) -> Token {
        Token {
            span: self.make_span(len),
            kind,
        }
    }

    fn consume_whitespace(&mut self) {
        fn whitespace(c: char) -> bool {
            c.is_ascii_whitespace()
        }

        let mut len = 0;
        while self.suffix[len..].starts_with(whitespace) || self.suffix[len..].starts_with("//") {
            if self.suffix[len..].starts_with(whitespace) {
                len += 1;
            } else {
                len = self.comment_len(len);
            }
        }

        self.make_span(len);
    }

    fn comment_len(&mut self, mut len: usize) -> usize {
        for c in self.suffix[len..].chars() {
            if c == '\n' {
                return len + 1;
            }
            len += c.len_utf8();
        }

        len
    }

    fn unexpected_character(&mut self) -> Error {
        let c = self.suffix.chars().next().unwrap();
        let span = self.make_span(c.len_utf8());
        Error::new(span, format!("unexpected character {c:?}"))
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        self.consume_whitespace();

        if self.suffix.starts_with(is_name_start) {
            return Ok(self.name_or_keyword());
        } else if is_int_start(self.suffix) {
            return self.int_literal();
        } else if self.suffix.starts_with('\'') {
            return self.char_literal();
        } else if self.suffix.starts_with('"') {
            return self.string_literal();
        }

        macro_rules! symbol_tokens {
            ($(($str:expr, $kind:ident)),* $(,)?) => {
                if self.suffix.is_empty() {
                    Ok(self.make_token(TokenKind::Eof, 0))
                } $(else if self.suffix.starts_with($str) {
                    Ok(self.make_token(TokenKind::$kind, $str.len()))
                })* else {
                    Err(self.unexpected_character())
                }
            };
        }

        symbol_tokens!(
            (">>>=", LogicalShrAssign),
            (">>>", LogicalShr),
            ("<<=", ShlAssign),
            (">>=", ArithmeticShrAssign),
            ("+=", AddAssign),
            ("-=", SubAssign),
            ("*=", MulAssign),
            ("/=", DivAssign),
            ("%=", ModAssign),
            ("&=", AndAssign),
            ("^=", XorAssign),
            ("|=", OrAssign),
            ("==", Eq),
            ("!=", Ne),
            ("<<", Shl),
            (">>", ArithmeticShr),
            ("&&", AndAnd),
            ("||", OrOr),
            ("<=", Le),
            (">=", Ge),
            ("<", Lt),
            (">", Gt),
            (";", Semi),
            (",", Comma),
            ("[", LeftBrack),
            ("]", RightBrack),
            ("(", LeftParen),
            (")", RightParen),
            ("{", LeftBrace),
            ("}", RightBrace),
            ("-", Dash),
            ("~", Tilde),
            ("!", Bang),
            ("*", Star),
            ("/", Slash),
            ("%", Percent),
            ("+", Plus),
            ("&", And),
            ("^", Xor),
            ("|", Or),
            ("=", Assign),
        )
    }

    fn name_or_keyword(&mut self) -> Token {
        let mut len = 1;
        while self.suffix[len..].starts_with(is_name_char) {
            len += 1;
        }

        let mut token = self.make_token(TokenKind::Name, len);

        match token.span.text {
            "let" => token.kind = TokenKind::Let,
            "fn" => token.kind = TokenKind::Fn,
            "return" => token.kind = TokenKind::Return,
            "if" => token.kind = TokenKind::If,
            "else" => token.kind = TokenKind::Else,
            "for" => token.kind = TokenKind::For,
            "true" => token.kind = TokenKind::True,
            "false" => token.kind = TokenKind::False,
            "null" => token.kind = TokenKind::Null,
            _ => {}
        }

        token
    }

    fn int_literal(&mut self) -> Result<Token, Error> {
        fn digit(c: char) -> bool {
            c.is_ascii_digit()
        }

        let mut len = usize::from(self.suffix.starts_with('-'));

        while self.suffix[len..].starts_with(digit) {
            len += 1;
        }

        let token = self.make_token(TokenKind::Int, len);

        if token.span.text.parse::<i64>().is_ok() {
            Ok(token)
        } else {
            Err(Error::new(
                token.span,
                format!("int literal must be between {} and {}", i64::MIN, i64::MAX),
            ))
        }
    }

    fn char_len(&mut self, len: usize, in_string: bool) -> Result<Option<usize>, Error> {
        match self.suffix[len..].chars().next() {
            None | Some('\n') => {
                if in_string {
                    Err(Error::new(self.make_span(1), "unterminated string literal"))
                } else {
                    Err(Error::new(self.make_span(1), "unterminated char literal"))
                }
            }
            Some('\\') => match self.suffix[len..].chars().nth(1) {
                Some('n' | 't' | '\\') => Ok(Some(len + 2)),
                Some('"') if in_string => Ok(Some(len + 2)),
                Some('\'') if !in_string => Ok(Some(len + 2)),
                c => {
                    self.make_span(len);
                    let span_len = if c.is_some() { 2 } else { 1 };
                    Err(Error::new(
                        self.make_span(span_len),
                        "invalid escape sequence",
                    ))
                }
            },
            Some('"') if in_string => Ok(None),
            Some('\'') if !in_string => Ok(None),
            Some(c) => Ok(Some(len + c.len_utf8())),
        }
    }

    fn char_literal(&mut self) -> Result<Token, Error> {
        let Some(len) = self.char_len(1, false)? else {
            return Err(Error::new(self.make_span(2), "empty char literal"));
        };

        if self.suffix[len..].starts_with('\'') {
            Ok(self.make_token(TokenKind::Char, len + 1))
        } else {
            Err(Error::new(self.make_span(len), "unterminated char literal"))
        }
    }

    fn string_literal(&mut self) -> Result<Token, Error> {
        let mut len = 1;
        while let Some(char_len) = self.char_len(len, true)? {
            len = char_len;
        }
        Ok(self.make_token(TokenKind::String, len + 1))
    }
}

fn is_name_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_name_char(c: char) -> bool {
    is_name_start(c) || c.is_ascii_digit()
}

fn is_int_start(suffix: &str) -> bool {
    fn digit(c: char) -> bool {
        c.is_ascii_digit()
    }
    suffix.starts_with(digit) || suffix.starts_with('-') && suffix[1..].starts_with(digit)
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Special
    Name,
    Int,
    Char,
    String,

    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    For,
    True,
    False,
    Null,

    // Symbols
    Semi,
    Comma,
    LeftBrack,
    RightBrack,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Dash,
    Tilde,
    Bang,
    Star,
    Slash,
    Percent,
    Plus,
    Shl,
    ArithmeticShr,
    LogicalShr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
    Xor,
    Or,
    AndAnd,
    OrOr,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    ShlAssign,
    ArithmeticShrAssign,
    LogicalShrAssign,
    AndAssign,
    XorAssign,
    OrAssign,

    Eof,
}

impl TokenKind {
    pub fn is_expr_start(self) -> bool {
        matches!(
            self,
            TokenKind::Int
                | TokenKind::Char
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null
                | TokenKind::String
                | TokenKind::Name
                | TokenKind::LeftBrace
                | TokenKind::LeftParen
                | TokenKind::If
                | TokenKind::Return
                | TokenKind::For
                | TokenKind::Dash
                | TokenKind::Tilde
                | TokenKind::Bang
                | TokenKind::Star
                | TokenKind::And
        )
    }
}
