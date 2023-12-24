use std::process::ExitCode;

mod ast;
<<<<<<< HEAD
mod codegen;
mod error;
mod lex;
=======
mod builtins;
mod codegen;
mod error;
mod ir;
mod lex;
mod lower;
>>>>>>> dbdeb72 (start ir)
mod parse;
mod resolve;
mod resolved;
mod toposort;

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let input_file_path = args.next().unwrap();
    let maybe_out = args.next();
    let output_file_path = maybe_out.as_deref().unwrap_or("out");

    match compile(&input_file_path, output_file_path) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            error.print();
            ExitCode::FAILURE
        }
    }
}

fn compile(input_file_path: &str, output_file_path: &str) -> Result<(), error::Error> {
    let code = std::fs::read_to_string(input_file_path)
        .map_err(|_| error::Error {
            msg: format!("cannot read file {input_file_path:?}"),
            span: error::Span::empty(),
        })?
        .leak();

    let items = parse::parse(code)?;
    let resolved = resolve::resolve(items)?;
    let order = toposort::static_initialization_order(&resolved)?;
    codegen::compile(&resolved, &order, output_file_path)
}
