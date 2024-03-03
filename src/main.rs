use std::process::{Command, ExitCode};

mod ast;
mod builtins;
mod codegen;
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
        .map_err(|_| error::Error::msg(format!("cannot read file {input_file:?}")))?
        .leak();

    let items = parse::parse(code)?;
    let resolved = resolve::resolve(items)?;
    let init_order = toposort::static_initialization_order(&resolved)?;

    let llvm_file = codegen::generate(&resolved, &init_order)?;
    let object_file = tempfile::NamedTempFile::with_prefix(".o")
        .map_err(|_| error::Error::msg("cannot create temporary object file"))?;

    let llc_status = Command::new("llc")
        .arg(llvm_file.path())
        .arg("-filetype=obj")
        .arg("-o")
        .arg(object_file.path())
        .status()
        .map_err(|_| error::Error::msg("cannot invoke llc"))?;
    if !llc_status.success() {
        return Err(error::Error::msg("llc failed with a nonzero exit code"));
    }

    let cc_status = Command::new("cc")
        .arg(object_file.path())
        .arg("-o")
        .arg(output_file)
        .status()
        .map_err(|_| error::Error::msg("cannot invoke linker"))?;
    if !cc_status.success() {
        return Err(error::Error::msg("cc failed with a nonzero exit code"));
    }

    Ok(())
}
