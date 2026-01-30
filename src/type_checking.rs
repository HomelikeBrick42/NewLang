use rustc_hash::FxHashMap;

use crate::{
    idvec::{IdMap, IdSlice},
    interning::InternedStr,
    lexing::SourceLocation,
    typed_tree::{
        ArgumentKind, BuiltinFunctionBody, Expression, ExpressionKind, FunctionBody, FunctionId,
        FunctionParameterKind, FunctionSignature, Pattern, PatternKind, Statement, StatementKind,
        Type, TypeId, TypeKind,
    },
};

#[derive(Debug)]
pub struct TypeCheckingError {
    pub location: SourceLocation,
    pub kind: TypeCheckingErrorKind,
}

#[derive(Debug)]
pub enum TypeCheckingErrorKind {
    UnableToInferType,
    IntegerOutOfRangeForType {
        typ: TypeId,
    },
    ExpectedNumberTypeButGot {
        got: TypeId,
    },
    ExpectedFunctionTypeButGot {
        got: TypeId,
    },
    ExpectedStructTypeButGot {
        got: TypeId,
    },
    ExpectedStructOrEnumTypeButGot {
        got: TypeId,
    },
    ExpectedTypeButGot {
        expected: TypeId,
        got: TypeId,
    },
    WrongNumberOfParameters {
        expected_count: usize,
        got_count: usize,
    },
    ExpectedTypeArgumentButGotValue,
    ExpectedValueArgumentButGotType,
    UnknownMemberName {
        typ: TypeId,
        name: InternedStr,
    },
    MustOnlyInitializeOneEnumMember,
    LeftStructMemberUninitialised {
        typ: TypeId,
    },
    MustOnlyDeconstructOneEnumMember,
    LeftStructMemberUndeconstructed {
        typ: TypeId,
    },
}

fn expect_equal(
    location: SourceLocation,
    expected: Option<TypeId>,
    mut got: TypeId,
    types: &mut IdSlice<TypeId, Type>,
) -> Result<(), TypeCheckingError> {
    let Some(mut expected) = expected else {
        return Ok(());
    };

    while let TypeKind::Inferred(id) = types[expected].kind {
        expected = id;
    }
    while let TypeKind::Inferred(id) = types[got].kind {
        got = id;
    }

    match types[got].kind {
        TypeKind::Resolving => panic!(
            "compiler bug, resolving types should not exist at this point {}",
            types[got].location
        ),

        TypeKind::Runtime
        | TypeKind::I64
        | TypeKind::FunctionItem(_)
        | TypeKind::Struct { .. }
        | TypeKind::Enum { .. }
        | TypeKind::Generic => {
            if expected != got {
                if let TypeKind::Infer = types[expected].kind {
                    types[expected].kind = TypeKind::Inferred(got);
                    return Ok(());
                }

                return Err(TypeCheckingError {
                    location,
                    kind: TypeCheckingErrorKind::ExpectedTypeButGot { expected, got },
                });
            }
        }

        TypeKind::Infer => types[got].kind = TypeKind::Inferred(expected),

        TypeKind::Inferred(_) => unreachable!(),
    }

    Ok(())
}

pub fn type_check_functions(
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
    types: &mut IdSlice<TypeId, Type>,
) -> Result<(), TypeCheckingError> {
    for (id, signature) in function_signatures.iter() {
        let body = &function_bodies[id];

        match *body {
            FunctionBody::Builtin(ref builtin) => match *builtin {
                BuiltinFunctionBody::PrintI64 => {}
            },

            FunctionBody::Expression {
                variables: _,
                ref expression,
            } => {
                type_check_expression(
                    expression,
                    Some(signature.return_type),
                    types,
                    function_signatures,
                )?;
            }
        }
    }
    Ok(())
}

fn type_check_expression(
    &Expression {
        location,
        mut typ,
        ref kind,
    }: &Expression,
    expected_type: Option<TypeId>,
    types: &mut IdSlice<TypeId, Type>,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
) -> Result<(), TypeCheckingError> {
    expect_equal(location, expected_type, typ, types)?;

    match *kind {
        ExpressionKind::Variable(_) | ExpressionKind::Function(_) => Ok(()),

        ExpressionKind::Integer(value) => loop {
            break match types[typ].kind {
                TypeKind::Resolving => panic!(
                    "compiler bug, resolving types should not exist at this point {}",
                    types[typ].location
                ),

                TypeKind::I64 => {
                    if value > i64::MAX as _ {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::IntegerOutOfRangeForType { typ },
                        });
                    }
                    Ok(())
                }

                TypeKind::Runtime
                | TypeKind::FunctionItem(_)
                | TypeKind::Struct { .. }
                | TypeKind::Enum { .. }
                | TypeKind::Generic => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::ExpectedNumberTypeButGot { got: typ },
                    });
                }

                TypeKind::Infer => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::UnableToInferType,
                    });
                }

                TypeKind::Inferred(id) => {
                    typ = id;
                    continue;
                }
            };
        },

        ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                type_check_statement(statement, types, function_signatures)?;
            }
            type_check_expression(last_expression, Some(typ), types, function_signatures)?;
            Ok(())
        }

        ExpressionKind::Constructor { ref members } => loop {
            break match types[typ].kind {
                TypeKind::Resolving => panic!(
                    "compiler bug, resolving types should not exist at this point {}",
                    types[typ].location
                ),

                TypeKind::I64
                | TypeKind::Runtime
                | TypeKind::FunctionItem(_)
                | TypeKind::Generic => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::ExpectedStructOrEnumTypeButGot { got: typ },
                    });
                }

                TypeKind::Struct {
                    members: ref struct_members,
                } => {
                    let mut members_to_initialise = struct_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for member in members {
                        let Some(struct_member_type) = members_to_initialise.remove(&member.name)
                        else {
                            return Err(TypeCheckingError {
                                location: member.location,
                                kind: TypeCheckingErrorKind::UnknownMemberName {
                                    typ,
                                    name: member.name,
                                },
                            });
                        };
                        type_check_expression(
                            &member.value,
                            Some(struct_member_type),
                            types,
                            function_signatures,
                        )?;
                    }
                    if !members_to_initialise.is_empty() {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::LeftStructMemberUninitialised { typ },
                        });
                    }
                    Ok(())
                }

                TypeKind::Enum {
                    members: ref enum_members,
                } => {
                    let [member] = &**members else {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::MustOnlyInitializeOneEnumMember,
                        });
                    };
                    let enum_member = enum_members
                        .iter()
                        .find(|enum_member| enum_member.name == member.name)
                        .ok_or(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnknownMemberName {
                                typ,
                                name: member.name,
                            },
                        })?;
                    type_check_expression(
                        &member.value,
                        Some(enum_member.typ),
                        types,
                        function_signatures,
                    )?;
                    Ok(())
                }

                TypeKind::Infer => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::UnableToInferType,
                    });
                }

                TypeKind::Inferred(id) => {
                    typ = id;
                    continue;
                }
            };
        },

        ExpressionKind::Unary {
            operator: _,
            ref operand,
        } => {
            type_check_expression(operand, Some(typ), types, function_signatures)?;
            loop {
                break match types[typ].kind {
                    TypeKind::Resolving => panic!(
                        "compiler bug, resolving types should not exist at this point {}",
                        types[typ].location
                    ),

                    TypeKind::I64 => Ok(()),

                    TypeKind::Runtime
                    | TypeKind::FunctionItem(_)
                    | TypeKind::Struct { .. }
                    | TypeKind::Enum { .. }
                    | TypeKind::Generic => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::ExpectedNumberTypeButGot { got: typ },
                        });
                    }

                    TypeKind::Infer => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnableToInferType,
                        });
                    }

                    TypeKind::Inferred(id) => {
                        typ = id;
                        continue;
                    }
                };
            }
        }

        ExpressionKind::Binary {
            ref left,
            operator: _,
            ref right,
        } => {
            type_check_expression(left, Some(typ), types, function_signatures)?;
            type_check_expression(right, Some(typ), types, function_signatures)?;
            loop {
                break match types[typ].kind {
                    TypeKind::Resolving => panic!(
                        "compiler bug, resolving types should not exist at this point {}",
                        types[typ].location
                    ),

                    TypeKind::I64 => Ok(()),

                    TypeKind::Runtime
                    | TypeKind::FunctionItem(_)
                    | TypeKind::Struct { .. }
                    | TypeKind::Enum { .. }
                    | TypeKind::Generic => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::ExpectedNumberTypeButGot { got: typ },
                        });
                    }

                    TypeKind::Infer => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnableToInferType,
                        });
                    }

                    TypeKind::Inferred(id) => {
                        typ = id;
                        continue;
                    }
                };
            }
        }

        ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            type_check_expression(operand, None, types, function_signatures)?;

            let mut function_type = operand.typ;
            let (parameters, return_type) = loop {
                break match types[function_type].kind {
                    TypeKind::Resolving => panic!(
                        "compiler bug, resolving types should not exist at this point {}",
                        types[function_type].location
                    ),

                    TypeKind::Runtime
                    | TypeKind::I64
                    | TypeKind::Struct { .. }
                    | TypeKind::Enum { .. }
                    | TypeKind::Generic => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::ExpectedFunctionTypeButGot {
                                got: function_type,
                            },
                        });
                    }

                    TypeKind::FunctionItem(id) => {
                        let signature = &function_signatures[id];
                        (&signature.parameters, signature.return_type)
                    }

                    TypeKind::Infer => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnableToInferType,
                        });
                    }

                    TypeKind::Inferred(id) => {
                        function_type = id;
                        continue;
                    }
                };
            };

            if parameters.len() != arguments.len() {
                return Err(TypeCheckingError {
                    location,
                    kind: TypeCheckingErrorKind::WrongNumberOfParameters {
                        expected_count: parameters.len(),
                        got_count: arguments.len(),
                    },
                });
            }

            for (parameter, argument) in parameters.iter().zip(arguments.iter()) {
                match (&parameter.kind, &argument.kind) {
                    (
                        &FunctionParameterKind::Value { name: _, typ },
                        ArgumentKind::Value(expression),
                    ) => {
                        type_check_expression(expression, Some(typ), types, function_signatures)?;
                    }

                    (&FunctionParameterKind::Type { .. }, &ArgumentKind::Type(_)) => {
                        todo!(
                            "implement type arguments and generic type replacement with the argument type"
                        )
                    }

                    (&FunctionParameterKind::Value { .. }, &ArgumentKind::Type(_)) => {
                        return Err(TypeCheckingError {
                            location: argument.location,
                            kind: TypeCheckingErrorKind::ExpectedValueArgumentButGotType,
                        });
                    }
                    (&FunctionParameterKind::Type { .. }, &ArgumentKind::Value(_)) => {
                        return Err(TypeCheckingError {
                            location: argument.location,
                            kind: TypeCheckingErrorKind::ExpectedTypeArgumentButGotValue,
                        });
                    }
                }
            }

            expect_equal(location, Some(return_type), typ, types)?;

            Ok(())
        }

        ExpressionKind::MemberAccess { ref operand, name } => {
            type_check_expression(operand, None, types, function_signatures)?;
            let mut operand_typ = operand.typ;
            loop {
                break match types[operand_typ].kind {
                    TypeKind::Resolving => panic!(
                        "compiler bug, resolving types should not exist at this point {}",
                        types[operand_typ].location
                    ),

                    TypeKind::I64
                    | TypeKind::Runtime
                    | TypeKind::FunctionItem(_)
                    | TypeKind::Enum { .. }
                    | TypeKind::Generic => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::ExpectedStructTypeButGot {
                                got: operand_typ,
                            },
                        });
                    }

                    TypeKind::Struct { ref members } => {
                        let member = members.iter().find(|member| member.name == name).ok_or(
                            TypeCheckingError {
                                location,
                                kind: TypeCheckingErrorKind::UnknownMemberName {
                                    typ: operand_typ,
                                    name,
                                },
                            },
                        )?;
                        expect_equal(location, Some(member.typ), typ, types)?;
                        Ok(())
                    }

                    TypeKind::Infer => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnableToInferType,
                        });
                    }

                    TypeKind::Inferred(id) => {
                        operand_typ = id;
                        continue;
                    }
                };
            }
        }
    }
}

fn type_check_statement(
    Statement { location: _, kind }: &Statement,
    types: &mut IdSlice<TypeId, Type>,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
) -> Result<(), TypeCheckingError> {
    match *kind {
        StatementKind::Expression(ref expression) => {
            type_check_expression(expression, None, types, function_signatures)
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            type_check_pattern(pattern, None, types)?;
            type_check_expression(value, Some(pattern.typ), types, function_signatures)?;
            Ok(())
        }
    }
}

fn type_check_pattern(
    &Pattern {
        location,
        mut typ,
        ref kind,
    }: &Pattern,
    expected_type: Option<TypeId>,
    types: &mut IdSlice<TypeId, Type>,
) -> Result<(), TypeCheckingError> {
    expect_equal(location, expected_type, typ, types)?;

    match *kind {
        PatternKind::Variable(_) | PatternKind::Function(_) | PatternKind::Let(_) => Ok(()),

        PatternKind::Integer(value) => loop {
            break match types[typ].kind {
                TypeKind::Resolving => panic!(
                    "compiler bug, resolving types should not exist at this point {}",
                    types[typ].location
                ),

                TypeKind::I64 => {
                    if value > i64::MAX as _ {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::IntegerOutOfRangeForType { typ },
                        });
                    }
                    Ok(())
                }

                TypeKind::Runtime
                | TypeKind::FunctionItem(_)
                | TypeKind::Struct { .. }
                | TypeKind::Enum { .. }
                | TypeKind::Generic => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::ExpectedNumberTypeButGot { got: typ },
                    });
                }

                TypeKind::Infer => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::UnableToInferType,
                    });
                }

                TypeKind::Inferred(id) => {
                    typ = id;
                    continue;
                }
            };
        },

        PatternKind::Destructor { ref members } => loop {
            break match types[typ].kind {
                TypeKind::Resolving => panic!(
                    "compiler bug, resolving types should not exist at this point {}",
                    types[typ].location
                ),

                TypeKind::I64
                | TypeKind::Runtime
                | TypeKind::FunctionItem(_)
                | TypeKind::Generic => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::ExpectedStructOrEnumTypeButGot { got: typ },
                    });
                }

                TypeKind::Struct {
                    members: ref struct_members,
                } => {
                    let mut members_to_initialise = struct_members
                        .iter()
                        .map(|member| (member.name, member.typ))
                        .collect::<FxHashMap<_, _>>();
                    for member in members {
                        let Some(struct_member_type) = members_to_initialise.remove(&member.name)
                        else {
                            return Err(TypeCheckingError {
                                location: member.location,
                                kind: TypeCheckingErrorKind::UnknownMemberName {
                                    typ,
                                    name: member.name,
                                },
                            });
                        };
                        type_check_pattern(&member.pattern, Some(struct_member_type), types)?;
                    }
                    if !members_to_initialise.is_empty() {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::LeftStructMemberUndeconstructed { typ },
                        });
                    }
                    Ok(())
                }

                TypeKind::Enum {
                    members: ref enum_members,
                } => {
                    let [member] = &**members else {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::MustOnlyDeconstructOneEnumMember,
                        });
                    };
                    let enum_member = enum_members
                        .iter()
                        .find(|enum_member| enum_member.name == member.name)
                        .ok_or(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::UnknownMemberName {
                                typ,
                                name: member.name,
                            },
                        })?;
                    type_check_pattern(&member.pattern, Some(enum_member.typ), types)?;
                    Ok(())
                }

                TypeKind::Infer => {
                    return Err(TypeCheckingError {
                        location,
                        kind: TypeCheckingErrorKind::UnableToInferType,
                    });
                }

                TypeKind::Inferred(id) => {
                    typ = id;
                    continue;
                }
            };
        },
    }
}
