use std::{num::NonZeroUsize, process::ExitCode};

use crate::{
    interning::InternedStr, lexing::SourceLocation, parsing::parse_file,
    resolving::resolve_program, validating::validate_items,
};

pub const FILE_EXTENSION: &str = "lang";

pub mod ast;
pub mod idvec;
pub mod interning;
pub mod lexing;
pub mod parsing;
pub mod resolving;
pub mod syntax_tree;
pub mod type_inference_tree;
pub mod validating;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return ExitCode::FAILURE;
    }

    let filepath: InternedStr = args[1].as_str().into();
    let Ok(source) = std::fs::read_to_string(filepath.as_str()) else {
        eprintln!("Unable to open file '{filepath}'");
        return ExitCode::FAILURE;
    };
    drop(args);

    let syntax_tree_items = match parse_file(filepath, &source) {
        Ok(items) => items,
        Err(error) => {
            eprintln!("{error}");
            return ExitCode::FAILURE;
        }
    };
    drop(source);

    let ast_items = match validate_items(&syntax_tree_items) {
        Ok(items) => items,
        Err(error) => {
            eprintln!("{error}");
            return ExitCode::FAILURE;
        }
    };
    drop(syntax_tree_items);

    let result = match resolve_program(
        SourceLocation {
            filepath,
            position: 0,
            line: NonZeroUsize::MIN,
            column: NonZeroUsize::MIN,
        },
        &ast_items,
    ) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("{error:#?}");
            return ExitCode::FAILURE;
        }
    };

    println!("{result:#?}");

    if !result.errors.is_empty() {
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
