use crate::{
    inferring::{InferringErrorKind, infer_program},
    inferring_tree as it,
    interning::InternedStr,
    parsing::parse_file,
    resolving::resolve_program,
    validating::validate_item,
};
use std::process::ExitCode;

pub mod ast;
pub mod inferring;
pub mod inferring_tree;
pub mod interning;
pub mod lexer;
pub mod parsing;
pub mod resolving;
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

    let mut inferring_program = {
        let mut errors = vec![];
        let program = resolve_program(&ast_items, &mut errors);
        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            return ExitCode::FAILURE;
        }
        program
    };
    drop(ast_items);

    {
        let mut errors = vec![];
        infer_program(&mut inferring_program, &mut errors);
        if !errors.is_empty() {
            for error in errors {
                eprint!("{}: ", error.location);
                match error.kind {
                    InferringErrorKind::ExpectedTypeButGotType {
                        expected_id,
                        got_id,
                    } => {
                        eprintln!(
                            "Expected type {} but got type {}",
                            it::PrettyPrintType {
                                id: expected_id,
                                types: &inferring_program.types
                            },
                            it::PrettyPrintType {
                                id: got_id,
                                types: &inferring_program.types
                            },
                        );
                        eprintln!(
                            "NOTE: Expected type declared at {}",
                            inferring_program.types[expected_id].location,
                        );
                        eprintln!(
                            "NOTE: Got type declared at {}",
                            inferring_program.types[got_id].location,
                        );
                    }
                }
            }
            return ExitCode::FAILURE;
        }
    }

    {
        let mut was_error = false;
        for (id, typ) in &inferring_program.types {
            if let it::TypeKind::Infer(_) = typ.kind {
                was_error = true;
                eprintln!(
                    "{}: Unable to infer type, only got as far as {}",
                    typ.location,
                    it::PrettyPrintType {
                        id,
                        types: &inferring_program.types
                    },
                );
            }
        }
        if was_error {
            return ExitCode::FAILURE;
        }
    }

    println!("{inferring_program:#?}");
    ExitCode::SUCCESS
}
