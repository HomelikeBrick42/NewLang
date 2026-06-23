use enum_map::enum_map;
use slotmap::{SecondaryMap, SlotMap};

use crate::{
    inferring::{InferringErrorKind, infer_program},
    inferring_tree as it,
    interning::InternedStr,
    parsing::parse_file,
    resolving::resolve_program,
    semantic_analysis::{SemanticAnalysisErrorKind, analyze_function},
    to_ir::{ToIrError, ToIrErrorKind, convert_function, convert_type},
    validating::validate_item,
};
use std::process::ExitCode;

pub mod ast;
pub mod inferring;
pub mod inferring_tree;
pub mod interning;
pub mod ir;
pub mod lexer;
pub mod parsing;
pub mod resolving;
pub mod semantic_analysis;
pub mod syntax_tree;
pub mod to_ir;
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
                                types: &inferring_program.types,
                            },
                            it::PrettyPrintType {
                                id: got_id,
                                types: &inferring_program.types,
                            },
                        );
                        eprintln!(
                            "    NOTE: Expected type declared at {}",
                            inferring_program.types[expected_id].location,
                        );
                        eprintln!(
                            "    NOTE: Got type declared at {}",
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
                        types: &inferring_program.types,
                    },
                );
            }
        }
        if was_error {
            return ExitCode::FAILURE;
        }
    }

    let mut ir_program = ir::Program {
        inferring_functions_map: SecondaryMap::new(),
        functions: SlotMap::with_key(),
        inferring_types_map: SecondaryMap::new(),
        types: SlotMap::with_key(),
        builtin_types: enum_map! { _ => None },
    };

    {
        let mut was_error = false;
        let mut print_error = |error: ToIrError, ir_program: &ir::Program| {
            was_error = true;
            eprint!("{}: ", error.location);
            match error.kind {
                ToIrErrorKind::ExpectedTypeButGotType {
                    expected: expected_id,
                    got: got_id,
                } => {
                    eprintln!(
                        "Expected type {} but got type {}",
                        ir::PrettyPrintType {
                            id: expected_id,
                            types: &ir_program.types,
                        },
                        ir::PrettyPrintType {
                            id: got_id,
                            types: &ir_program.types,
                        },
                    );
                    eprintln!(
                        "    NOTE: Expected type declared at {}",
                        ir_program.types[expected_id].location,
                    );
                    eprintln!(
                        "    NOTE: Got type declared at {}",
                        ir_program.types[got_id].location,
                    );
                }
                ToIrErrorKind::IntegerTooBigForI64 => eprintln!("Integer is too big for i64"),
                ToIrErrorKind::PatternNotAssignable => eprintln!("Pattern is not assignable"),
            }
        };

        'converting: {
            for (builtin_type, id) in inferring_program.builtin_types {
                if let Some(id) = id {
                    match convert_type(id, &mut ir_program, &inferring_program) {
                        Ok(id) => ir_program.builtin_types[builtin_type] = Some(id),
                        Err(error) => {
                            print_error(error, &ir_program);
                            break 'converting;
                        }
                    }
                }
            }
            for (id, _) in &inferring_program.types {
                match convert_type(id, &mut ir_program, &inferring_program) {
                    Ok(_) => {}
                    Err(error) => print_error(error, &ir_program),
                }
            }
            for (id, _) in &inferring_program.functions {
                match convert_function(id, &mut ir_program, &inferring_program) {
                    Ok(_) => {}
                    Err(error) => print_error(error, &ir_program),
                }
            }
        }

        if was_error {
            return ExitCode::FAILURE;
        }
    }
    drop(inferring_program);

    {
        let mut errors = vec![];

        for (id, _) in &ir_program.functions {
            analyze_function(id, &ir_program, &mut errors);
        }

        if !errors.is_empty() {
            for error in errors {
                eprint!("{}: ", error.location);
                match error.kind {
                    SemanticAnalysisErrorKind::VariableUninitialized { variable_location } => {
                        eprintln!("Variable is uninitialized");
                        eprintln!("    NOTE: Variable declared at {variable_location}");
                    }
                }
            }
            return ExitCode::FAILURE;
        }
    }

    for (id, function) in &ir_program.functions {
        print!("{id:?} = fn {}(", function.name.unwrap_or("_".into()));
        match function.body {
            ir::FunctionBody::Resolving => println!(") {{ ... }}"),

            ir::FunctionBody::Body {
                ref variables,
                ref parameter_variables,
                ref blocks,
                entry_block,
            } => {
                for (i, variable) in parameter_variables.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{variable:?}");
                }
                println!(") {{");

                for (id, variable) in variables {
                    println!("    // {:?}", variable.location);
                    println!("    {id:?} = {:?}", variable.typ);
                }
                println!();

                println!("    entry = {entry_block:?}");
                println!();

                for (i, (id, block)) in blocks.iter().enumerate() {
                    if i > 0 {
                        println!();
                    }

                    println!("    {id:?} = {{");
                    for instruction in &block.instructions {
                        println!("        // {:?}", instruction.location);
                        println!("        {:?}", instruction.kind);
                    }
                    println!();

                    println!("        // {:?}", block.jump.location);
                    println!("        {:?}", block.jump.kind);
                    println!("    }}");
                }

                println!("}}");
            }
        }
        println!();
    }

    drop(ir_program);
    ExitCode::SUCCESS
}
