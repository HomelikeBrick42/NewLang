use crate::{
    inferring_tree::{
        Argument, Expression, ExpressionKind, FunctionBody, InferTypeKind, Pattern, PatternKind,
        Place, PlaceKind, Program, Statement, StatementKind, Type, TypeId, TypeKind, TypeParameter,
    },
    lexer::SourceLocation,
};
use slotmap::SlotMap;

pub fn infer_program(program: &mut Program, errors: &mut Vec<InferringError>) {
    for (id, body) in &program.function_bodies {
        let function = &program.functions[id];
        match *body {
            FunctionBody::Expression {
                variables: _,
                parameter_variables: _,
                ref expression,
            } => infer_expression(
                expression,
                Some(function.return_type),
                &mut program.types,
                errors,
            ),

            FunctionBody::Builtin(_) => {}
        }
    }
}

pub fn infer_statement(
    statement: &Statement,
    types: &mut SlotMap<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    match statement.kind {
        StatementKind::Expression(ref expression) => {
            infer_expression(expression, None, types, errors);
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            infer_pattern(pattern, None, types, errors);
            infer_expression(value, Some(pattern.typ), types, errors);
        }
    }
}

pub fn infer_expression(
    expression: &Expression,
    expected_type: Option<TypeId>,
    types: &mut SlotMap<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    if let Some(expected_type) = expected_type {
        infer_equal(
            expression.location,
            expression.typ,
            expected_type,
            types,
            errors,
        );
    }

    match expression.kind {
        ExpressionKind::Place(ref place) => infer_place(place, Some(expression.typ), types, errors),

        ExpressionKind::Integer(_) => {}

        ExpressionKind::Block {
            end_location: _,
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                infer_statement(statement, types, errors);
            }
            infer_expression(last_expression, expected_type, types, errors);
        }

        ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            // TODO: make this better so that the expected and got types are the right way around
            let function_type = types.insert(Type {
                location: expression.location,
                kind: TypeKind::Function {
                    parameters: arguments
                        .iter()
                        .map(|argument| match *argument {
                            Argument::Value { ref expression } => TypeParameter::Value {
                                typ: expression.typ,
                            },
                        })
                        .collect(),
                    return_type: expression.typ,
                },
            });

            infer_expression(operand, Some(function_type), types, errors);
            for argument in arguments {
                match *argument {
                    Argument::Value { ref expression } => {
                        infer_expression(expression, None, types, errors);
                    }
                }
            }
        }

        ExpressionKind::Constructor { ref members } => {
            let struct_like = types.insert(Type {
                location: expression.location,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: members
                        .iter()
                        .map(|member| (member.name, member.value.typ))
                        .collect(),
                }),
            });
            infer_equal(
                expression.location,
                expression.typ,
                struct_like,
                types,
                errors,
            );
        }
    }
}

pub fn infer_pattern(
    pattern: &Pattern,
    expected_type: Option<TypeId>,
    types: &mut SlotMap<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    if let Some(expected_type) = expected_type {
        infer_equal(pattern.location, pattern.typ, expected_type, types, errors);
    }

    match pattern.kind {
        PatternKind::Place(ref place) => infer_place(place, Some(pattern.typ), types, errors),

        PatternKind::Integer(_) => {}
        PatternKind::Discard => {}

        PatternKind::Deconstructor { ref members } => {
            let struct_like = types.insert(Type {
                location: pattern.location,
                kind: TypeKind::Infer(InferTypeKind::StructLike {
                    members: members
                        .iter()
                        .map(|member| (member.name, member.pattern.typ))
                        .collect(),
                }),
            });
            infer_equal(pattern.location, pattern.typ, struct_like, types, errors);
        }
    }
}

pub fn infer_place(
    place: &Place,
    expected_type: Option<TypeId>,
    types: &mut SlotMap<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    if let Some(expected_type) = expected_type {
        infer_equal(place.location, place.typ, expected_type, types, errors);
    }

    match place.kind {
        PlaceKind::Function(_) => {}
        PlaceKind::Variable(_) => {}
        PlaceKind::Let(_) => {}
    }
}

pub fn infer_equal(
    location: SourceLocation,
    got_id: TypeId,
    expected_id: TypeId,
    types: &mut SlotMap<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) -> bool {
    if got_id == expected_id {
        return true;
    }
    if let TypeKind::Inferred(got_type) = types[got_id].kind {
        return infer_equal(location, got_type, expected_id, types, errors);
    }
    if let TypeKind::Inferred(expected_type) = types[expected_id].kind {
        return infer_equal(location, got_id, expected_type, types, errors);
    }

    if type_contains(got_id, expected_id, types) || type_contains(expected_id, got_id, types) {
        return false;
    }

    match (&types[got_id].kind, &types[expected_id].kind) {
        (&TypeKind::Infer(InferTypeKind::Anything), &_) => {
            types[got_id].kind = TypeKind::Inferred(expected_id);
            true
        }
        (&_, &TypeKind::Infer(InferTypeKind::Anything)) => {
            types[expected_id].kind = TypeKind::Inferred(got_id);
            true
        }

        (
            TypeKind::Infer(InferTypeKind::StructLike {
                members: got_members,
            }),
            TypeKind::Struct {
                name: _,
                members: expected_members,
            },
        ) => {
            let mut equal = true;

            let got_members = got_members.clone();
            for member in expected_members.clone() {
                if let Some(&got_id) = got_members.get(&member.name) {
                    equal &= infer_equal(location, got_id, member.typ, types, errors);
                }
            }

            if equal {
                types[got_id].kind = TypeKind::Inferred(expected_id);
            } else {
                errors.push(InferringError {
                    location,
                    kind: InferringErrorKind::ExpectedTypeButGotType {
                        expected_id,
                        got_id,
                    },
                });
            }

            equal
        }
        (
            TypeKind::Struct {
                name: _,
                members: got_members,
            },
            TypeKind::Infer(InferTypeKind::StructLike {
                members: expected_members,
            }),
        ) => {
            let mut equal = true;

            let expected_members = expected_members.clone();
            for member in got_members.clone() {
                if let Some(&expected_id) = expected_members.get(&member.name) {
                    equal &= infer_equal(location, member.typ, expected_id, types, errors);
                }
            }

            if equal {
                types[expected_id].kind = TypeKind::Inferred(got_id);
            } else {
                errors.push(InferringError {
                    location,
                    kind: InferringErrorKind::ExpectedTypeButGotType {
                        expected_id,
                        got_id,
                    },
                });
            }

            equal
        }

        (
            &TypeKind::Infer(InferTypeKind::StructLike {
                members: ref got_members,
            }),
            &TypeKind::Infer(InferTypeKind::StructLike {
                members: ref expected_members,
            }),
        ) => {
            let mut equal = true;

            let mut members = got_members.clone();
            for (name, expected_id) in expected_members.clone() {
                match members.get(&name) {
                    Some(&got_id) => {
                        equal &= infer_equal(location, got_id, expected_id, types, errors)
                    }
                    None => _ = members.insert(name, expected_id),
                }
            }

            if equal {
                let TypeKind::Infer(InferTypeKind::StructLike {
                    members: ref mut expected_members,
                }) = types[expected_id].kind
                else {
                    unreachable!()
                };
                *expected_members = members;
                types[got_id].kind = TypeKind::Inferred(expected_id);
            } else {
                errors.push(InferringError {
                    location,
                    kind: InferringErrorKind::ExpectedTypeButGotType {
                        expected_id,
                        got_id,
                    },
                });
            }

            equal
        }

        (
            &TypeKind::Function {
                parameters: ref got_parameters,
                return_type: got_return_type,
            },
            &TypeKind::Function {
                parameters: ref expected_parameters,
                return_type: expected_return_type,
            },
        ) if got_parameters.len() == expected_parameters.len() => {
            let mut equal = true;
            for (got_parameter, expected_parameter) in got_parameters
                .clone()
                .into_iter()
                .zip(expected_parameters.clone())
            {
                match (got_parameter, expected_parameter) {
                    (
                        TypeParameter::Value { typ: got_id },
                        TypeParameter::Value { typ: expected_id },
                    ) => equal &= infer_equal(location, got_id, expected_id, types, errors),
                }
            }
            equal &= infer_equal(
                location,
                got_return_type,
                expected_return_type,
                types,
                errors,
            );
            if equal {
                types[got_id].kind = TypeKind::Inferred(expected_id);
            } else {
                errors.push(InferringError {
                    location,
                    kind: InferringErrorKind::ExpectedTypeButGotType {
                        expected_id,
                        got_id,
                    },
                });
            }
            equal
        }

        _ => {
            errors.push(InferringError {
                location,
                kind: InferringErrorKind::ExpectedTypeButGotType {
                    expected_id,
                    got_id,
                },
            });
            false
        }
    }
}

// returns true if a contains b
fn type_contains(a: TypeId, b: TypeId, types: &SlotMap<TypeId, Type>) -> bool {
    if a == b {
        return true;
    }

    match types[a].kind {
        TypeKind::Resolving => false,
        TypeKind::Infer(ref infer_type_kind) => match *infer_type_kind {
            InferTypeKind::Anything => false,
            InferTypeKind::StructLike { ref members } => {
                members.iter().any(|(_, &typ)| type_contains(typ, b, types))
            }
        },
        TypeKind::Inferred(id) => type_contains(id, b, types),
        TypeKind::Unit => false,
        TypeKind::Runtime => false,
        TypeKind::I64 => false,
        TypeKind::Struct {
            name: _,
            members: _,
        } => false,
        TypeKind::Function {
            ref parameters,
            return_type,
        } => {
            type_contains(return_type, b, types)
                || parameters.iter().any(|parameter| match *parameter {
                    TypeParameter::Value { typ } => type_contains(typ, b, types),
                })
        }
    }
}

#[derive(Debug)]
pub struct InferringError {
    pub location: SourceLocation,
    pub kind: InferringErrorKind,
}

#[derive(Debug)]
pub enum InferringErrorKind {
    ExpectedTypeButGotType { expected_id: TypeId, got_id: TypeId },
}
