use std::collections::hash_map::Entry;

use crate::{
    idvec::{IdMap, IdSlice, IdVec},
    lexing::SourceLocation,
    type_inference_tree::{
        ArgumentKind, BuiltinFunctionBody, Expression, ExpressionKind, FunctionBody, FunctionId,
        FunctionParameterKind, FunctionSignature, InferFunctionParameter, InferTypeKind, Pattern,
        PatternKind, PrettyPrintError, Statement, StatementKind, Type, TypeId, TypeKind,
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
            } => infer_expression(
                expression,
                signature.return_type,
                function_signatures,
                types,
                &mut errors,
            ),
        }
    }
    errors
}

pub fn infer_expression(
    expression: &Expression,
    expected_type: TypeId,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    unify_types(
        expression.location,
        expected_type,
        expression.typ,
        function_signatures,
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
                infer_statement(statement, function_signatures, types, errors);
            }
            infer_expression(
                last_expression,
                expected_type,
                function_signatures,
                types,
                errors,
            );
        }

        ExpressionKind::Constructor { ref members } => {
            for member in members {
                infer_expression(
                    &member.value,
                    member.value.typ,
                    function_signatures,
                    types,
                    errors,
                );
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
                function_signatures,
                types,
                errors,
            );
        }

        ExpressionKind::Unary {
            operator: _,
            ref operand,
        } => {
            infer_expression(operand, expression.typ, function_signatures, types, errors);
        }

        ExpressionKind::Binary {
            ref left,
            operator: _,
            ref right,
        } => {
            infer_expression(left, expression.typ, function_signatures, types, errors);
            infer_expression(right, expression.typ, function_signatures, types, errors);
        }

        ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            let function_like_type = types.push(Type {
                location: expression.location,
                name: None,
                kind: TypeKind::Infer(InferTypeKind::FunctionLike {
                    parameters: arguments
                        .iter()
                        .map(|argument| match argument.kind {
                            ArgumentKind::Value(ref expression) => InferFunctionParameter::Value {
                                typ: expression.typ,
                            },
                        })
                        .collect(),
                    return_type: expression.typ,
                }),
            });
            infer_expression(
                operand,
                function_like_type,
                function_signatures,
                types,
                errors,
            );
            for argument in arguments {
                match argument.kind {
                    ArgumentKind::Value(ref expression) => {
                        infer_expression(
                            expression,
                            expression.typ,
                            function_signatures,
                            types,
                            errors,
                        );
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
            infer_expression(
                operand,
                struct_like_type,
                function_signatures,
                types,
                errors,
            );
        }
    }
}

pub fn infer_statement(
    statement: &Statement,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    match statement.kind {
        StatementKind::Expression(ref expression) => {
            infer_expression(
                expression,
                expression.typ,
                function_signatures,
                types,
                errors,
            );
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            infer_pattern(pattern, value.typ, function_signatures, types, errors);
            infer_expression(value, pattern.typ, function_signatures, types, errors);
        }
    }
}

pub fn infer_pattern(
    pattern: &Pattern,
    expected_type: TypeId,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    types: &mut IdVec<TypeId, Type>,
    errors: &mut Vec<InferringError>,
) {
    unify_types(
        pattern.location,
        expected_type,
        pattern.typ,
        function_signatures,
        types,
        errors,
    );
    match pattern.kind {
        PatternKind::Variable(_) => {}
        PatternKind::Function(_) => {}
        PatternKind::Integer(_) => {}

        PatternKind::Deconstructor { ref members } => {
            for member in members {
                infer_pattern(
                    &member.pattern,
                    member.pattern.typ,
                    function_signatures,
                    types,
                    errors,
                );
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
                function_signatures,
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
            infer_expression(
                operand,
                struct_like_type,
                function_signatures,
                types,
                errors,
            );
        }

        PatternKind::Let(_) => {}
    }
}

fn unify_types(
    location: SourceLocation,
    mut expected_id: TypeId,
    mut got_id: TypeId,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
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

                (InferTypeKind::Number, InferTypeKind::Number) => {
                    got.kind = TypeKind::Inferred(expected_id);
                    true
                }

                (
                    &mut InferTypeKind::FunctionLike {
                        parameters: ref expected_parameters,
                        return_type: expected_return_type,
                    },
                    &mut InferTypeKind::FunctionLike {
                        parameters: ref got_parameters,
                        return_type: got_return_type,
                    },
                ) => {
                    let expected_parameters = expected_parameters.clone();
                    let got_parameters = got_parameters.clone();

                    unify_types(
                        types[expected_return_type].location,
                        expected_return_type,
                        got_return_type,
                        function_signatures,
                        types,
                        errors,
                    );

                    if expected_parameters.len() == got_parameters.len() {
                        let same_parameter_types = true;

                        for (expected_parameter, got_parameter) in
                            expected_parameters.into_iter().zip(got_parameters)
                        {
                            match (expected_parameter, got_parameter) {
                                (
                                    InferFunctionParameter::Value { typ: expected_typ },
                                    InferFunctionParameter::Value { typ: got_typ },
                                ) => {
                                    unify_types(
                                        types[expected_typ].location,
                                        expected_typ,
                                        got_typ,
                                        function_signatures,
                                        types,
                                        errors,
                                    );
                                }
                            }
                        }

                        if same_parameter_types {
                            types[expected_id].kind = TypeKind::Inferred(got_id);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }

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
                                unify_types(
                                    expected_location,
                                    expected,
                                    got_typ,
                                    function_signatures,
                                    types,
                                    errors,
                                );
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

            InferTypeKind::FunctionLike {
                parameters,
                return_type,
            } => match *got {
                TypeKind::FunctionItem(function_id) => {
                    let parameters = parameters.clone();
                    let return_type = *return_type;

                    let signature = &function_signatures[function_id];

                    unify_types(
                        types[return_type].location,
                        signature.return_type,
                        return_type,
                        function_signatures,
                        types,
                        errors,
                    );

                    if parameters.len() == signature.parameters.len() {
                        let same_parameter_types = true;

                        for (function_parameter, parameter) in
                            signature.parameters.iter().zip(parameters)
                        {
                            match (&function_parameter.kind, parameter) {
                                (
                                    &FunctionParameterKind::Value {
                                        name: _,
                                        typ: expected_typ,
                                    },
                                    InferFunctionParameter::Value { typ: got_typ },
                                ) => {
                                    unify_types(
                                        types[got_typ].location,
                                        expected_typ,
                                        got_typ,
                                        function_signatures,
                                        types,
                                        errors,
                                    );
                                }
                            }
                        }

                        if same_parameter_types {
                            types[expected_id].kind = TypeKind::Inferred(got_id);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }

                _ => false,
            },

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
                        unify_types(
                            location,
                            struct_member.typ,
                            member,
                            function_signatures,
                            types,
                            errors,
                        );
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
                        unify_types(
                            location,
                            enum_member.typ,
                            member,
                            function_signatures,
                            types,
                            errors,
                        );
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

            InferTypeKind::FunctionLike {
                parameters,
                return_type,
            } => match *expected {
                TypeKind::FunctionItem(function_id) => {
                    let parameters = parameters.clone();
                    let return_type = *return_type;

                    let signature = &function_signatures[function_id];

                    unify_types(
                        types[return_type].location,
                        signature.return_type,
                        return_type,
                        function_signatures,
                        types,
                        errors,
                    );

                    if parameters.len() == signature.parameters.len() {
                        let same_parameter_types = true;

                        for (function_parameter, parameter) in
                            signature.parameters.iter().zip(parameters)
                        {
                            match (&function_parameter.kind, parameter) {
                                (
                                    &FunctionParameterKind::Value {
                                        name: _,
                                        typ: expected_typ,
                                    },
                                    InferFunctionParameter::Value { typ: got_typ },
                                ) => {
                                    unify_types(
                                        types[got_typ].location,
                                        expected_typ,
                                        got_typ,
                                        function_signatures,
                                        types,
                                        errors,
                                    );
                                }
                            }
                        }

                        if same_parameter_types {
                            types[got_id].kind = TypeKind::Inferred(expected_id);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }

                _ => false,
            },

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
                        unify_types(
                            location,
                            struct_member.typ,
                            member,
                            function_signatures,
                            types,
                            errors,
                        );
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
                        unify_types(
                            location,
                            enum_member.typ,
                            member,
                            function_signatures,
                            types,
                            errors,
                        );
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
