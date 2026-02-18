use crate::{
    inferring::{infer_program, print_inferring_errors},
    interning::InternedStr,
    interpreting::{Value, interpret_function},
    lexing::SourceLocation,
    parsing::parse_file,
    resolving::{ModuleItemKind, print_resolving_errors, resolve_program},
    type_checker::{print_type_checking_errors, type_check_program},
    typed_tree::PrettyPrintType,
    validating::validate_items,
};
use std::{num::NonZeroUsize, process::ExitCode};

pub const FILE_EXTENSION: &str = "lang";

pub mod ast;
pub mod idvec;
pub mod inferring;
pub mod interning;
pub mod interpreting;
pub mod lexing;
pub mod parsing;
pub mod resolving;
pub mod syntax_tree;
pub mod type_checker;
pub mod type_inference_tree;
pub mod typed_tree;
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

    let mut resolved_program = resolve_program(
        SourceLocation {
            filepath,
            position: 0,
            line: NonZeroUsize::MIN,
            column: NonZeroUsize::MIN,
        },
        &ast_items,
    );
    if !resolved_program.errors.is_empty() {
        print_resolving_errors(&resolved_program);
        return ExitCode::FAILURE;
    }

    let global_module = &resolved_program.modules[resolved_program.global_module.unwrap()];
    let Some(&main_item) = global_module.items.get(&"main".into()) else {
        eprintln!("Cannot find main function");
        return ExitCode::FAILURE;
    };
    let main_item = &resolved_program.module_items[main_item];
    let ModuleItemKind::Function(main_function_id) = main_item.kind else {
        eprintln!(
            "{}: Expected the main function to be a function",
            main_item.location
        );
        return ExitCode::FAILURE;
    };

    let inferring_errors = infer_program(
        &mut resolved_program.types,
        &resolved_program.function_signatures,
        &resolved_program.function_bodies,
    );
    if !inferring_errors.is_empty() {
        print_inferring_errors(&inferring_errors, &resolved_program.types);
        return ExitCode::FAILURE;
    }
    drop(inferring_errors);

    let typed_program = type_check_program(&resolved_program);
    if !typed_program.errors.is_empty() {
        print_type_checking_errors(&typed_program);
        return ExitCode::FAILURE;
    }
    drop(resolved_program);

    let main_function = &typed_program.function_signatures[main_function_id];
    let [runtime_argument] = *main_function.value_parameters else {
        eprintln!(
            "{}: Expected the main function to take one argument",
            main_function.location
        );
        return ExitCode::FAILURE;
    };
    if runtime_argument != typed_program.builtins.runtime_type.unwrap() {
        eprintln!(
            "{}: Expected the main function's parameter to be {} but got {}",
            main_function.location,
            PrettyPrintType {
                typ: typed_program.builtins.runtime_type.unwrap(),
                types: &typed_program.types
            },
            PrettyPrintType {
                typ: runtime_argument,
                types: &typed_program.types
            }
        );
        return ExitCode::FAILURE;
    }
    if main_function.return_type != typed_program.builtins.unit_type.unwrap() {
        eprintln!(
            "{}: Expected the main function's return type to be {} but got {}",
            main_function.location,
            PrettyPrintType {
                typ: typed_program.builtins.unit_type.unwrap(),
                types: &typed_program.types
            },
            PrettyPrintType {
                typ: runtime_argument,
                types: &typed_program.types
            }
        );
        return ExitCode::FAILURE;
    }

    interpret_function(
        main_function_id,
        Box::new([Value::Struct {
            members: Box::new([]),
        }]),
        &typed_program,
    );

    ExitCode::SUCCESS
}
