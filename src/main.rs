use slotmap::SlotMap;

use crate::{
    interning::InternedStr,
    parsing::parse_file,
    resolved_tree::ResolvedProgram,
    resolving::{ResolvingErrorKind, resolve_file},
    validating::validate_item,
};
use std::process::ExitCode;

pub mod ast;
pub mod interning;
pub mod lexer;
pub mod parsing;
pub mod resolved_tree;
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

    let mut resolved_program = ResolvedProgram {
        type_aliases: SlotMap::with_key(),
        structs: SlotMap::with_key(),
        generic_types: SlotMap::with_key(),
        generic_dyns: SlotMap::with_key(),
        parameterized_types: SlotMap::with_key(),
        parameterized_dyns: SlotMap::with_key(),
        functions: SlotMap::with_key(),
        variables: SlotMap::with_key(),

        unit_type: None,
    };
    let resolved_items = {
        let mut errors = vec![];
        let items = resolve_file(&ast_items, &mut resolved_program, &mut errors);
        if !errors.is_empty() {
            for error in errors {
                eprint!("{}: ", error.location);
                match error.kind {
                    ResolvingErrorKind::UnknownName { name } => eprintln!("Unknown name '{name}'"),
                    ResolvingErrorKind::ExpectedType => eprintln!("Expected type"),
                    ResolvingErrorKind::ExpectedParameterizedType => {
                        eprintln!("Expected parameterized type")
                    }
                    ResolvingErrorKind::ExpectedDyn => eprintln!("Expected dyn"),
                    ResolvingErrorKind::ExpectedParameterizedDyn => {
                        eprintln!("Expected parameterized dyn")
                    }
                    ResolvingErrorKind::ExpectedPlace => eprintln!("Expected place"),
                }
            }
            return ExitCode::FAILURE;
        }
        items
    };
    drop(ast_items);

    println!("{resolved_program:#?}");
    println!("{resolved_items:#?}");

    drop(resolved_program);
    drop(resolved_items);
    ExitCode::SUCCESS
}
