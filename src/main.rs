use crate::{interning::InternedStr, parsing::parse_file};
use std::process::ExitCode;

pub mod interning;
pub mod lexer;
pub mod parsing;
pub mod syntax_tree;

fn main() -> ExitCode {
    let filepath: InternedStr = match std::env::args().collect::<Vec<_>>().as_slice() {
        [_, filepath] => filepath.as_str().into(),
        [program_name, ..] => {
            eprintln!("Usage: {program_name} <filepath>");
            return ExitCode::FAILURE;
        }
        [] => unreachable!(),
    };
    let source = match std::fs::read_to_string(filepath.as_str()) {
        Ok(source) => source,
        Err(error) => {
            eprintln!("Unable to read '{filepath}': {error}");
            return ExitCode::FAILURE;
        }
    };
    let syntax_items = match parse_file(filepath, &source) {
        Ok(syntax_items) => syntax_items,
        Err(error) => {
            eprintln!("{error}");
            return ExitCode::FAILURE;
        }
    };

    println!("{:#?}", syntax_items);
    ExitCode::SUCCESS
}
