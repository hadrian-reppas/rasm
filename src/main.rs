mod ast;
mod error;
mod lex;
mod parse;
mod resolve;
mod resolved;
mod toposort;

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let code = std::fs::read_to_string(file_path).unwrap().leak();

    let items = parse::parse(code).unwrap();

    let resolved = resolve::resolve(items).unwrap();
    println!("{resolved:#?}");

    let order = toposort::global_initialization_order(&resolved).unwrap();
    println!("{order:#?}");
}
