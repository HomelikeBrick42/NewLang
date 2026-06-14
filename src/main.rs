use crate::{interning::InternedStr, parsing::parse_file, validating::validate_item};
use std::process::ExitCode;

pub mod ast;
pub mod interning;
pub mod lexer;
pub mod parsing;
pub mod syntax_tree;
pub mod validating;

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
    drop(source);

    let ast_items = {
        let mut was_error = false;
        let mut items = Vec::with_capacity(syntax_items.len());
        for item in &syntax_items {
            match validate_item(item) {
                Ok(item) => items.push(item),
                Err(error) => {
                    eprintln!("{error}");
                    was_error = true;
                }
            }
        }
        if was_error {
            return ExitCode::FAILURE;
        }
        items
    };
    drop(syntax_items);

    println!("{ast_items:#?}");
    ExitCode::SUCCESS
}
