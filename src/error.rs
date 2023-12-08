use termion::{color, style};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub text: &'static str,
    pub code: &'static str,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn empty() -> Self {
        Span {
            text: "",
            code: "",
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

impl Error {
    pub fn print(&self) {
        println!(
            "{}{}error:{} {}{}",
            style::Bold,
            color::Fg(color::Red),
            color::Fg(color::Reset),
            self.msg,
            style::Reset
        );
        if self.span.line != 0 {
            let line = self.span.code.lines().nth(self.span.line - 1).unwrap();
            let number = format!("{}", self.span.line);
            println!(
                "{}{}{number} |{}{} {line}",
                style::Bold,
                color::Fg(color::Blue),
                color::Fg(color::Reset),
                style::Reset
            );
            let ws: String = line[..self.span.column]
                .chars()
                .map(|c| if c == '\t' { '\t' } else { ' ' })
                .collect();
            let carets = "^".repeat(self.span.text.chars().count());
            println!(
                "{}   {}{}{}{}{}{}",
                " ".repeat(number.len()),
                ws,
                style::Bold,
                color::Fg(color::Red),
                carets,
                color::Fg(color::Reset),
                style::Reset
            );
        }
    }
}
