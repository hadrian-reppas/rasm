#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub text: &'static str,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn empty() -> Self {
        Span {
            text: "",
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}
