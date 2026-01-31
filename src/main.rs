use std::fmt::Display;

use crate::{
    idvec::IdSlice,
    interning::InternedStr,
    parsing::parse_file,
    treewalk_interpreter::execute_program,
    type_checking::{TypeCheckingError, TypeCheckingErrorKind, type_check_functions},
    typed_tree::{Type, TypeId, TypeKind},
    typing::{BindingKind, type_items},
    validating::validate_items,
};

pub const FILE_EXTENSION: &str = "lang";

pub mod ast;
pub mod idvec;
pub mod interning;
pub mod lexing;
pub mod parsing;
pub mod syntax_tree;
pub mod treewalk_interpreter;
pub mod type_checking;
pub mod typed_tree;
pub mod typing;
pub mod validating;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filepath: InternedStr = args[1].as_str().into();
    let Ok(source) = std::fs::read_to_string(filepath.as_str()) else {
        eprintln!("Unable to open file '{filepath}'");
        return;
    };
    drop(args);

    let syntax_tree_items = match parse_file(filepath, &source) {
        Ok(items) => items,
        Err(error) => {
            eprintln!("{error}");
            return;
        }
    };
    drop(source);

    let ast_items = match validate_items(&syntax_tree_items) {
        Ok(items) => items,
        Err(error) => {
            eprintln!("{error}");
            return;
        }
    };
    drop(syntax_tree_items);

    let mut result = match type_items(filepath, &ast_items) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("{error}");
            return;
        }
    };
    drop(ast_items);

    match type_check_functions(
        &result.function_signatures,
        &result.function_bodies,
        &mut result.types,
    ) {
        Ok(()) => {}
        Err(TypeCheckingError { location, kind }) => {
            eprint!("{location}: ");
            match kind {
                TypeCheckingErrorKind::UnableToInferType => {
                    eprintln!("Unable to infer type");
                }
                TypeCheckingErrorKind::IntegerOutOfRangeForType { typ } => {
                    eprintln!(
                        "Integer out of range for type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ
                        },
                    );
                }
                TypeCheckingErrorKind::ExpectedNumberTypeButGot { got } => {
                    eprintln!(
                        "Expected number type, but got type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ: got
                        },
                    );
                    eprintln!("NOTE: got type created at {}", result.types[got].location);
                }
                TypeCheckingErrorKind::ExpectedFunctionTypeButGot { got } => {
                    eprintln!(
                        "Expected function type, but got type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ: got
                        },
                    );
                    eprintln!("NOTE: got type created at {}", result.types[got].location);
                }
                TypeCheckingErrorKind::ExpectedStructTypeButGot { got } => {
                    eprintln!(
                        "Expected struct type, but got type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ: got
                        },
                    );
                    eprintln!("NOTE: got type created at {}", result.types[got].location);
                }
                TypeCheckingErrorKind::ExpectedStructOrEnumTypeButGot { got } => {
                    eprintln!(
                        "Expected struct type, but got type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ: got
                        },
                    );
                    eprintln!("NOTE: got type created at {}", result.types[got].location);
                }
                TypeCheckingErrorKind::ExpectedTypeButGot { expected, got } => {
                    eprintln!(
                        "Expected type {}, but got type {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ: expected
                        },
                        PrettyPrintType {
                            types: &result.types,
                            typ: got
                        },
                    );
                    eprintln!(
                        "NOTE: expected type created at {}",
                        result.types[expected].location
                    );
                    eprintln!("NOTE: got type created at {}", result.types[got].location);
                }
                TypeCheckingErrorKind::WrongNumberOfParameters {
                    expected_count,
                    got_count,
                } => {
                    eprintln!(
                        "Expected {expected_count} parameters, but got {got_count} arguments"
                    );
                }
                TypeCheckingErrorKind::ExpectedTypeArgumentButGotValue => {
                    eprintln!("Expected type argument but got value");
                }
                TypeCheckingErrorKind::ExpectedValueArgumentButGotType => {
                    eprintln!("Expected value argument but got type");
                }
                TypeCheckingErrorKind::UnknownMemberName { typ, name } => {
                    eprintln!("Unknown member name '{name}'");
                    eprintln!(
                        "NOTE: the type was {} and declared at {}",
                        PrettyPrintType {
                            types: &result.types,
                            typ
                        },
                        result.types[typ].location
                    );
                }
                TypeCheckingErrorKind::MustOnlyInitializeOneEnumMember => {
                    eprintln!("Must initialise only one enum member");
                }
                TypeCheckingErrorKind::LeftStructMemberUninitialised { typ } => {
                    eprintln!(
                        "Left struct member of {} uninitialised",
                        PrettyPrintType {
                            types: &result.types,
                            typ
                        },
                    );
                    eprintln!(
                        "NOTE: the type was declared at {}",
                        result.types[typ].location
                    );
                }
                TypeCheckingErrorKind::MustOnlyDeconstructOneEnumMember => {
                    eprintln!("Must deconstruct only one enum member");
                }
                TypeCheckingErrorKind::LeftStructMemberUndeconstructed { typ } => {
                    eprintln!(
                        "Left struct member of {} undeconstructed",
                        PrettyPrintType {
                            types: &result.types,
                            typ
                        },
                    );
                    eprintln!(
                        "NOTE: the type was declared at {}",
                        result.types[typ].location
                    );
                }
            }
            return;
        }
    }

    {
        let mut was_error = false;
        for id in result.types.iter().map(|(id, _)| id).collect::<Vec<_>>() {
            // TODO: fix the tree properly instead of this hack lmao
            while let TypeKind::Inferred(new_id) = result.types[id].kind {
                result.types[id] = result.types[new_id].clone();
            }

            if let TypeKind::Infer = result.types[id].kind {
                was_error = true;
                eprintln!("{}: Unable to infer type", result.types[id].location);
            }
        }
        if was_error {
            return;
        }
    }

    let BindingKind::Module {
        parent: _,
        name: _,
        names: ref global_module,
    } = result.bindings[result.global_module].kind
    else {
        unreachable!();
    };
    let BindingKind::Function(main_function) = result.bindings[global_module[&"main".into()]].kind
    else {
        println!("Unable to find main function");
        return;
    };
    execute_program(
        main_function,
        &result.function_signatures,
        &result.function_bodies,
        &result.types,
    );
}

struct PrettyPrintType<'a> {
    types: &'a IdSlice<TypeId, Type>,
    typ: TypeId,
}

impl Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut typ = &self.types[self.typ];
        loop {
            break match typ.kind {
                TypeKind::Resolving => write!(f, "{{resolving}}"),
                TypeKind::Runtime => write!(f, "Runtime"),
                TypeKind::I64 => write!(f, "I64"),
                TypeKind::FunctionItem(id) => {
                    if let Some(name) = typ.name {
                        write!(f, "{{function item '{name}' {id:?}}}")
                    } else {
                        write!(f, "{{function item {id:?}}}")
                    }
                }
                TypeKind::Struct { ref members } => {
                    if let Some(name) = typ.name {
                        write!(f, "{name}")
                    } else {
                        write!(f, "struct {{")?;
                        for (i, member) in members.iter().enumerate() {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            write!(
                                f,
                                "{}: {}",
                                member.name,
                                PrettyPrintType {
                                    types: self.types,
                                    typ: member.typ
                                }
                            )?;
                            if i + 1 < members.len() {
                                write!(f, ", ")?;
                            } else {
                                write!(f, " ")?;
                            }
                        }
                        write!(f, "}}")
                    }
                }
                TypeKind::Enum { ref members } => {
                    if let Some(name) = typ.name {
                        write!(f, "{name}")
                    } else {
                        write!(f, "enum {{")?;
                        for (i, member) in members.iter().enumerate() {
                            if i > 0 {
                                write!(f, " ")?;
                            }
                            write!(
                                f,
                                "{}: {}",
                                member.name,
                                PrettyPrintType {
                                    types: self.types,
                                    typ: member.typ
                                }
                            )?;
                            if i + 1 < members.len() {
                                write!(f, ", ")?;
                            } else {
                                write!(f, " ")?;
                            }
                        }
                        write!(f, "}}")
                    }
                }
                TypeKind::Generic => write!(f, "{{generic}}"),
                TypeKind::Infer => write!(f, "{{infer}}"),
                TypeKind::Inferred(id) => {
                    typ = &self.types[id];
                    continue;
                }
            };
        }
    }
}
