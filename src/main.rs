use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::Parser;

mod ast;
mod builtins;
mod codegen;
mod error;
mod lex;
mod parse;
mod resolve;
mod resolved;
mod toposort;

#[derive(Parser)]
struct Arguments {
    /// The input .rasm file
    input: PathBuf,

    /// The output file
    #[arg(default_value = "out")]
    output: PathBuf,

    /// Optimize output
    #[arg(short = 't', long = "release")]
    release: bool,
}

fn main() -> ExitCode {
    let args = Arguments::parse();

    match compile(&args.input, &args.output, args.release) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            error.print();
            ExitCode::FAILURE
        }
    }
}

fn compile(input: &Path, output: &Path, release: bool) -> Result<(), error::Error> {
    let code = std::fs::read_to_string(input)
        .map_err(|_| error::Error::msg(format!("cannot read file {input:?}")))?
        .leak();

    let items = parse::parse(code)?;
    let resolved = resolve::resolve(items)?;
    let init_order = toposort::static_initialization_order(&resolved)?;

    let llvm_file = codegen::generate(&resolved, &init_order)?;
    let object_file = tempfile::NamedTempFile::with_prefix(".o")
        .map_err(|_| error::Error::msg("cannot create temporary object file"))?;

    let opt_level = if release { "-O3" } else { "-O0" };
    let llc_status = Command::new("llc")
        .arg(llvm_file.path())
        .arg("-filetype=obj")
        .arg("-o")
        .arg(object_file.path())
        .arg(opt_level)
        .status()
        .map_err(|_| error::Error::msg("cannot invoke llc"))?;
    if !llc_status.success() {
        return Err(error::Error::msg("llc failed with a nonzero exit code"));
    }

    let cc_status = Command::new("cc")
        .arg(object_file.path())
        .arg("-o")
        .arg(output)
        .status()
        .map_err(|_| error::Error::msg("cannot invoke linker"))?;
    if !cc_status.success() {
        return Err(error::Error::msg("cc failed with a nonzero exit code"));
    }

    Ok(())
}
