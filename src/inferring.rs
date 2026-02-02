use std::collections::hash_map::Entry;

use crate::{
    idvec::{IdMap, IdSlice, IdVec},
    lexing::SourceLocation,
    type_inference_tree::{
        ArgumentKind, BuiltinFunctionBody, Expression, ExpressionKind, FunctionBody, FunctionId,
        FunctionSignature, InferTypeKind, Pattern, PatternKind, PrettyPrintError, Statement,
        StatementKind, Type, TypeId, TypeKind,
    },
};

#[derive(Debug)]
pub struct InferringError {
    pub location: SourceLocation,
    pub kind: InferringErrorKind,
}

#[derive(Debug)]
pub enum InferringErrorKind {
    UnificationError { expected: TypeId, got: TypeId },
}

pub fn print_inferring_errors(errors: &[InferringError], types: &IdSlice<TypeId, Type>) {
    for error in errors {
        eprint!("{}: ", error.location);
        match error.kind {
            InferringErrorKind::UnificationError { expected, got } => {
                eprintln!(
                    "Expected type {} but got type {}",
                    PrettyPrintError {
                        typ: expected,
                        types
                    },
                    PrettyPrintError { typ: got, types }
                );
                eprintln!(
                    "NOTE: Expected type was created at {}",
                    types[expected].location
                );
                eprintln!("NOTE: Got type was created at {}", types[got].location);
            }
        }
    }
}

pub fn infer_program(
    types: &mut IdVec<TypeId, Type>,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
) -> Vec<InferringError> {
    let mut errors = vec![];
    for (id, function_body) in function_bodies.iter() {
        let signature = &function_signatures[id];

        match *function_body {
            FunctionBody::Builtin(ref builtin) => match *builtin {
                BuiltinFunctionBody::PrintI64 => {}
            },

            FunctionBody::Expression {
                variables: _,
                parameter_variables: _,
                ref expression,
            } => infer_expression(expression, signature.return_type, types, &mut errors),
        }
    }
    errors
}

pub fn infer_expression(
    expression: &Expression,
    expected_type: TypeId,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    unify_types(
        expression.location,
        expected_type,
        expression.typ,
        types,
        errors,
    );
    match expression.kind {
        ExpressionKind::Variable(_) => {}
        ExpressionKind::Function(_) => {}
        ExpressionKind::Integer(_) => {}

        ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                infer_statement(statement, types, errors);
            }
            infer_expression(last_expression, expected_type, types, errors);
        }

        ExpressionKind::Constructor { ref members } => {
            for member in members {
                infer_expression(&member.value, member.value.typ, types, errors);
            }
            let struct_like_type = types.push(Type {
                location: expression.location,
                name: None,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: members
                        .iter()
                        .map(|member| (member.name, member.value.typ))
                        .collect(),
                }),
            });
            unify_types(
                expression.location,
                struct_like_type,
                expression.typ,
                types,
                errors,
            );
        }

        ExpressionKind::Unary {
            operator: _,
            ref operand,
        } => {
            infer_expression(operand, expression.typ, types, errors);
        }

        ExpressionKind::Binary {
            ref left,
            operator: _,
            ref right,
        } => {
            infer_expression(left, expression.typ, types, errors);
            infer_expression(right, expression.typ, types, errors);
        }

        ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            infer_expression(operand, operand.typ, types, errors);
            for argument in arguments {
                match argument.kind {
                    ArgumentKind::Value(ref expression) => {
                        infer_expression(expression, expression.typ, types, errors);
                    }
                }
            }
        }

        ExpressionKind::MemberAccess { ref operand, name } => {
            let struct_like_type = types.push(Type {
                location: expression.location,
                name: None,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: [(name, expression.typ)].into_iter().collect(),
                }),
            });
            infer_expression(operand, struct_like_type, types, errors);
        }
    }
}

pub fn infer_statement(
    statement: &Statement,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    match statement.kind {
        StatementKind::Expression(ref expression) => {
            infer_expression(expression, expression.typ, types, errors);
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            infer_pattern(pattern, value.typ, types, errors);
            infer_expression(value, pattern.typ, types, errors);
        }
    }
}

pub fn infer_pattern(
    pattern: &Pattern,
    expected_type: TypeId,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    unify_types(pattern.location, expected_type, pattern.typ, types, errors);
    match pattern.kind {
        PatternKind::Variable(_) => {}
        PatternKind::Function(_) => {}
        PatternKind::Integer(_) => {}

        PatternKind::Deconstructor { ref members } => {
            for member in members {
                infer_pattern(&member.pattern, member.pattern.typ, types, errors);
            }
            let struct_like_type = types.push(Type {
                location: pattern.location,
                name: None,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: members
                        .iter()
                        .map(|member| (member.name, member.pattern.typ))
                        .collect(),
                }),
            });
            unify_types(
                pattern.location,
                struct_like_type,
                pattern.typ,
                types,
                errors,
            );
        }

        PatternKind::MemberAccess { ref operand, name } => {
            let struct_like_type = types.push(Type {
                location: pattern.location,
                name: None,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: [(name, pattern.typ)].into_iter().collect(),
                }),
            });
            infer_expression(operand, struct_like_type, types, errors);
        }

        PatternKind::Let(_) => {}
    }
}

fn unify_types(
    location: SourceLocation,
    mut expected_id: TypeId,
    mut got_id: TypeId,
    types: &mut IdSlice<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    while let TypeKind::Inferred(id) = types[expected_id].kind {
        expected_id = id;
    }
    while let TypeKind::Inferred(id) = types[got_id].kind {
        got_id = id;
    }

    if expected_id == got_id {
        return;
    }

    let [expected, got] = types.get_disjoint_mut([expected_id, got_id]).unwrap();
    let equal = match (&mut expected.kind, &mut got.kind) {
        (TypeKind::Infer(expected_infer), TypeKind::Infer(got_infer)) => {
            match (&mut *expected_infer, &mut *got_infer) {
                (_, InferTypeKind::Anything) => {
                    got.kind = TypeKind::Inferred(expected_id);
                    true
                }

                (InferTypeKind::Anything, _) => {
                    expected.kind = TypeKind::Inferred(got_id);
                    true
                }

                (InferTypeKind::Number, InferTypeKind::Number) => true,

                (
                    InferTypeKind::StructLike {
                        members: expected_members,
                    },
                    InferTypeKind::StructLike {
                        members: got_members,
                    },
                ) => {
                    let mut expected_members = expected_members.clone();
                    for (got_name, got_typ) in got_members.clone() {
                        match expected_members.entry(got_name) {
                            Entry::Occupied(e) => {
                                let expected = *e.get();
                                let expected_location = types[expected].location;
                                unify_types(expected_location, expected, got_typ, types, errors);
                            }
                            Entry::Vacant(e) => {
                                e.insert(got_typ);
                            }
                        }
                    }

                    types[expected_id].kind = TypeKind::Infer(InferTypeKind::StructLike {
                        members: expected_members,
                    });
                    types[got_id].kind = TypeKind::Inferred(expected_id);
                    true
                }

                _ => false,
            }
        }

        (TypeKind::Infer(expected_infer), got) => match expected_infer {
            InferTypeKind::Anything => {
                expected.kind = TypeKind::Inferred(got_id);
                true
            }

            InferTypeKind::Number => {
                if let TypeKind::Integer(_) = got {
                    expected.kind = TypeKind::Inferred(got_id);
                    true
                } else {
                    false
                }
            }

            InferTypeKind::StructLike { members } => match got {
                TypeKind::Struct {
                    members: struct_members,
                } => {
                    let mut members = members.clone();
                    let struct_members = struct_members.clone();

                    for struct_member in struct_members {
                        let Some(member) = members.remove(&struct_member.name) else {
                            continue;
                        };
                        let location = types[member].location;
                        unify_types(location, struct_member.typ, member, types, errors);
                    }

                    if members.is_empty() {
                        types[expected_id].kind = TypeKind::Inferred(got_id);
                        true
                    } else {
                        false
                    }
                }

                TypeKind::Enum {
                    members: enum_members,
                } => {
                    let mut members = members.clone();
                    let enum_members = enum_members.clone();

                    for enum_member in enum_members {
                        let Some(member) = members.remove(&enum_member.name) else {
                            continue;
                        };
                        let location = types[member].location;
                        unify_types(location, enum_member.typ, member, types, errors);
                    }

                    if members.is_empty() {
                        types[expected_id].kind = TypeKind::Inferred(got_id);
                        true
                    } else {
                        false
                    }
                }

                _ => false,
            },
        },

        (expected, TypeKind::Infer(got_infer)) => match got_infer {
            InferTypeKind::Anything => {
                got.kind = TypeKind::Inferred(expected_id);
                true
            }

            InferTypeKind::Number => {
                if let TypeKind::Integer(_) = expected {
                    got.kind = TypeKind::Inferred(expected_id);
                    true
                } else {
                    false
                }
            }

            InferTypeKind::StructLike { members } => match expected {
                TypeKind::Struct {
                    members: struct_members,
                } => {
                    let mut members = members.clone();
                    let struct_members = struct_members.clone();

                    for struct_member in struct_members {
                        let Some(member) = members.remove(&struct_member.name) else {
                            continue;
                        };
                        let location = types[member].location;
                        unify_types(location, struct_member.typ, member, types, errors);
                    }

                    if members.is_empty() {
                        types[got_id].kind = TypeKind::Inferred(expected_id);
                        true
                    } else {
                        false
                    }
                }

                TypeKind::Enum {
                    members: enum_members,
                } => {
                    let mut members = members.clone();
                    let enum_members = enum_members.clone();

                    for enum_member in enum_members {
                        let Some(member) = members.remove(&enum_member.name) else {
                            continue;
                        };
                        let location = types[member].location;
                        unify_types(location, enum_member.typ, member, types, errors);
                    }

                    if members.is_empty() {
                        types[got_id].kind = TypeKind::Inferred(expected_id);
                        true
                    } else {
                        false
                    }
                }

                _ => false,
            },
        },

        (_, _) => false,
    };

    if !equal {
        errors.push(InferringError {
            location,
            kind: InferringErrorKind::UnificationError {
                expected: expected_id,
                got: got_id,
            },
        });
    }
}
