#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub text: &'static str,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}
