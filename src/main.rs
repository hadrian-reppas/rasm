mod ast;
mod error;
mod lex;
mod parse;
mod resolve;
mod resolved;

fn main() {
    let file_path = std::env::args().nth(1).unwrap();
    let code = std::fs::read_to_string(file_path).unwrap().leak();

    let items = parse::parse(code).unwrap();

    let (globals, functions) = resolve::make_globals_and_functions(&items).unwrap();
    let resolved: Vec<_> = items
        .into_iter()
        .map(|item| resolve::resolve(item, &globals, &functions))
        .collect::<Result<_, _>>()
        .unwrap();
    println!("{resolved:#?}");
}
