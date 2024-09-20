use termion::{color, style};

use crate::io::SourcePath;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub text: &'static str,
    pub code: &'static str,
    pub path: SourcePath,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn adjacent(self, other: Span) -> bool {
        self.line == other.line && self.column + self.text.len() == other.column
    }
}

#[derive(Debug)]
pub struct Error {
    span: Span,
    msg: String,
}

impl Error {
    pub fn new(span: Span, msg: impl ToString) -> Self {
        Error {
            span,
            msg: msg.to_string(),
        }
    }

    pub fn msg(msg: impl ToString) -> Self {
        let span = Span {
            text: "",
            code: "",
            path: SourcePath::Std(&[]),
            line: 0,
            column: 0,
        };
        Error {
            span,
            msg: msg.to_string(),
        }
    }

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
