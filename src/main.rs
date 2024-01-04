use std::process::ExitCode;

mod ast;
mod builtins;
mod error;
mod lex;
mod parse;
mod resolve;
mod resolved;
mod toposort;

fn main() -> ExitCode {
    let mut args = std::env::args().skip(1);
    let maybe_input = args.next();
    let maybe_out = args.next();
    let output_file = maybe_out.as_deref().unwrap_or("out");

    match compile(maybe_input.as_deref(), output_file) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            error.print();
            ExitCode::FAILURE
        }
    }
}

fn compile(maybe_input: Option<&str>, output_file: &str) -> Result<(), error::Error> {
    let Some(input_file) = maybe_input else {
        return Err(error::Error::msg("no input file"));
    };

    let code = std::fs::read_to_string(input_file)
        .map_err(|_| error::Error::msg("cannot read file {input_file:?}"))?
        .leak();

    let items = parse::parse(code)?;
    let resolved = resolve::resolve(items)?;
    let order = toposort::static_initialization_order(&resolved)?;

    todo!()
}
