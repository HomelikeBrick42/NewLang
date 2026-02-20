#![expect(clippy::too_many_arguments)]

use crate::{
    ast,
    idvec::{IdMap, IdVec},
    interning::InternedStr,
    interpreting::{IntegerValue, Value, interpret_expression},
    lexing::SourceLocation,
    resolving::ResolvedProgram,
    type_inference_tree as ti, typed_tree as tt,
};
use derive_more::Debug;

#[derive(Debug)]
pub struct TypeCheckingError {
    pub location: SourceLocation,
    pub kind: TypeCheckingErrorKind,
}

#[derive(Debug)]
pub enum TypeCheckingErrorKind {
    CylicDependency,
    ValueTooBigForI64(u128),
    UnknownMember {
        name: InternedStr,
    },
    MemberLeftUninitialised {
        name: InternedStr,
    },
    OnlyOneEnumMemberCanBeInitialised,
    UnknownUnaryOperatorForType {
        operand_typ: tt::TypeId,
        result_typ: tt::TypeId,
    },
    UnknownBinaryOperatorForType {
        left_typ: tt::TypeId,
        right_typ: tt::TypeId,
        result_typ: tt::TypeId,
    },
    PlaceIsNotAssignable,
    PatternIsNotAssignable,
    CannotAccessMemberOfEnum,
    CannotInferType(ti::TypeId),
}

pub fn print_type_checking_errors(
    type_checked_program: &TypedProgram,
    resolved_program: &ResolvedProgram,
) {
    for error in &type_checked_program.errors {
        eprint!("{}: ", error.location);
        match error.kind {
            TypeCheckingErrorKind::CylicDependency => {
                eprintln!("Cyclic dependency");
            }

            TypeCheckingErrorKind::ValueTooBigForI64(value) => {
                eprintln!("Value too big for I64: '{value}'");
            }

            TypeCheckingErrorKind::UnknownMember { name } => {
                eprintln!("Unknown member '{name}'");
            }

            TypeCheckingErrorKind::MemberLeftUninitialised { name } => {
                eprintln!("The '{name}' member was left uninitialised");
            }

            TypeCheckingErrorKind::OnlyOneEnumMemberCanBeInitialised => {
                eprintln!("Only one enum member should be initialised");
            }

            TypeCheckingErrorKind::UnknownUnaryOperatorForType {
                operand_typ,
                result_typ,
            } => {
                eprintln!("{:?} {:?}", operand_typ, result_typ);
            }

            TypeCheckingErrorKind::UnknownBinaryOperatorForType {
                left_typ,
                right_typ,
                result_typ,
            } => {
                eprintln!("{:?} {:?} {:?}", left_typ, right_typ, result_typ);
            }

            TypeCheckingErrorKind::PlaceIsNotAssignable => {
                eprintln!("Expression is not assignable");
            }

            TypeCheckingErrorKind::PatternIsNotAssignable => {
                eprintln!("Pattern is not assignable");
            }

            TypeCheckingErrorKind::CannotAccessMemberOfEnum => {
                eprintln!("Cannot access member of enum");
            }

            TypeCheckingErrorKind::CannotInferType(typ) => {
                eprintln!(
                    "Could not infer type, only got as far as {}",
                    ti::PrettyPrintType {
                        typ,
                        types: &resolved_program.types
                    }
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct Builtins {
    pub runtime_type: Option<tt::TypeId>,
    pub i64_type: Option<tt::TypeId>,
    pub unit_type: Option<tt::TypeId>,
    pub bool_type: Option<tt::TypeId>,
}

#[derive(Debug)]
pub struct TypedProgram {
    pub types: IdVec<tt::TypeId, tt::Type>,

    pub function_signatures: IdMap<ti::FunctionId, tt::FunctionSignature>,
    pub function_bodies: IdMap<ti::FunctionId, tt::FunctionBody>,

    pub const_values: IdVec<tt::ConstId, Value>,

    pub builtins: Builtins,

    #[debug(ignore)]
    pub errors: Vec<TypeCheckingError>,
}

pub fn type_check_program(resolved_program: &ResolvedProgram) -> TypedProgram {
    let mut resolved_types = IdMap::new();
    let mut types = IdVec::new();
    let mut type_checking_functions = IdMap::new();
    let mut function_signatures = IdMap::new();
    let mut function_bodies = IdMap::new();
    let mut evaluating_consts = IdMap::new();
    let mut resolved_const_values = IdMap::new();
    let mut const_values = IdVec::new();
    let mut errors = vec![];

    for (id, _) in resolved_program.types.iter() {
        match type_check_type(id, resolved_program, &mut resolved_types, &mut types) {
            Ok(_) => {}
            Err(error) => {
                errors.push(error);
            }
        }
    }
    for (id, _) in resolved_program.function_signatures.iter() {
        match type_check_function_signature(
            id,
            resolved_program,
            &mut resolved_types,
            &mut types,
            &mut type_checking_functions,
            &mut function_signatures,
            &mut function_bodies,
        ) {
            Ok(()) => {}
            Err(error) => {
                errors.push(error);
            }
        }
    }
    for (id, _) in resolved_program.function_signatures.iter() {
        match type_check_function_body(
            id,
            resolved_program,
            &mut resolved_types,
            &mut types,
            &mut type_checking_functions,
            &mut function_signatures,
            &mut function_bodies,
            &mut evaluating_consts,
            &mut resolved_const_values,
            &mut const_values,
            false,
        ) {
            Ok(()) => {}
            Err(error) => {
                errors.push(error);
            }
        }
    }
    for (id, _) in resolved_program.consts.iter() {
        match type_check_const(
            id,
            resolved_program,
            &mut resolved_types,
            &mut types,
            &mut type_checking_functions,
            &mut function_signatures,
            &mut function_bodies,
            &mut evaluating_consts,
            &mut resolved_const_values,
            &mut const_values,
        ) {
            Ok(_) => {}
            Err(error) => {
                errors.push(error);
            }
        }
    }

    TypedProgram {
        types,
        function_signatures,
        function_bodies,
        const_values,
        builtins: Builtins {
            runtime_type: resolved_program
                .builtins
                .runtime_type
                .map(|typ| resolved_types[typ]),
            i64_type: resolved_program
                .builtins
                .i64_type
                .map(|typ| resolved_types[typ]),
            unit_type: resolved_program
                .builtins
                .unit_type
                .map(|typ| resolved_types[typ]),
            bool_type: resolved_program
                .builtins
                .bool_type
                .map(|typ| resolved_types[typ]),
        },
        errors,
    }
}

fn type_check_type(
    typ: ti::TypeId,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
) -> Result<tt::TypeId, TypeCheckingError> {
    if let Some(&typ) = resolved_types.get(typ) {
        return Ok(typ);
    }
    if let ti::TypeKind::Inferred(id) = resolved_program.types[typ].kind {
        let id = type_check_type(id, resolved_program, resolved_types, types)?;
        resolved_types.insert(typ, id);
        return Ok(id);
    }

    let id = types.push(tt::Type {
        location: resolved_program.types[typ].location,
        name: resolved_program.types[typ].name,
        kind: tt::TypeKind::Resolving,
    });
    resolved_types.insert(typ, id);
    types[id].kind = match resolved_program.types[typ].kind {
        ti::TypeKind::Resolving | ti::TypeKind::Inferred(_) => {
            unreachable!(
                "{}: {:?}",
                resolved_program.types[typ].location, resolved_program.types[typ].kind
            )
        }

        ti::TypeKind::Infer(_) => {
            return Err(TypeCheckingError {
                location: resolved_program.types[typ].location,
                kind: TypeCheckingErrorKind::CannotInferType(typ),
            });
        }

        ti::TypeKind::Runtime => tt::TypeKind::Runtime,

        ti::TypeKind::Integer(integer_type_kind) => tt::TypeKind::Integer(integer_type_kind),

        ti::TypeKind::FunctionItem(id) => tt::TypeKind::FunctionItem(id),

        ti::TypeKind::Struct { ref members } => tt::TypeKind::Struct {
            members: members
                .iter()
                .map(
                    |&ti::TypeMember {
                         location,
                         name,
                         typ,
                     }| {
                        Ok(tt::TypeMember {
                            location,
                            name,
                            typ: type_check_type(typ, resolved_program, resolved_types, types)?,
                        })
                    },
                )
                .collect::<Result<_, TypeCheckingError>>()?,
        },

        ti::TypeKind::Enum { ref members } => tt::TypeKind::Enum {
            members: members
                .iter()
                .map(
                    |&ti::TypeMember {
                         location,
                         name,
                         typ,
                     }| {
                        Ok(tt::TypeMember {
                            location,
                            name,
                            typ: type_check_type(typ, resolved_program, resolved_types, types)?,
                        })
                    },
                )
                .collect::<Result<_, TypeCheckingError>>()?,
        },

        ti::TypeKind::Generic => tt::TypeKind::Generic,
    };
    Ok(id)
}

fn type_check_function_signature(
    function: ti::FunctionId,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    #[expect(unused)] function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
) -> Result<(), TypeCheckingError> {
    if function_signatures.contains(function) {
        return Ok(());
    }
    if type_checking_functions.insert(function, ()).is_some() {
        return Err(TypeCheckingError {
            location: resolved_program.function_signatures[function].location,
            kind: TypeCheckingErrorKind::CylicDependency,
        });
    }

    let &ti::FunctionSignature {
        location,
        name,
        ref parameters,
        return_type,
        typ,
    } = &resolved_program.function_signatures[function];

    let mut value_parameters = vec![];
    for parameter in parameters {
        match parameter.kind {
            ti::FunctionParameterKind::Value { name: _, typ } => {
                value_parameters.push(type_check_type(
                    typ,
                    resolved_program,
                    resolved_types,
                    types,
                )?);
            }
        }
    }

    let return_type = type_check_type(return_type, resolved_program, resolved_types, types)?;
    let typ = type_check_type(typ, resolved_program, resolved_types, types)?;
    function_signatures.insert(
        function,
        tt::FunctionSignature {
            location,
            name,
            value_parameters: value_parameters.into_boxed_slice(),
            return_type,
            typ,
        },
    );

    type_checking_functions.remove(function);
    Ok(())
}

fn type_check_function_body(
    function: ti::FunctionId,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
    fully_type_check_body: bool,
) -> Result<(), TypeCheckingError> {
    type_check_function_signature(
        function,
        resolved_program,
        resolved_types,
        types,
        type_checking_functions,
        function_signatures,
        function_bodies,
    )?;
    if function_bodies.contains(function) {
        return Ok(());
    }
    if type_checking_functions.insert(function, ()).is_some() {
        return Err(TypeCheckingError {
            location: resolved_program.function_signatures[function].location,
            kind: TypeCheckingErrorKind::CylicDependency,
        });
    }

    let body = match resolved_program.function_bodies[function] {
        ti::FunctionBody::Builtin(builtin_function_body) => {
            tt::FunctionBody::Builtin(builtin_function_body)
        }

        ti::FunctionBody::Expression {
            ref variables,
            ref parameter_variables,
            ref expression,
        } => {
            let mut resolved_variables = IdMap::new();
            let mut typed_variables = IdVec::new();
            for (
                id,
                &ti::Variable {
                    location,
                    name,
                    typ,
                },
            ) in variables.iter()
            {
                resolved_variables.insert(
                    id,
                    typed_variables.push(tt::Variable {
                        location,
                        name,
                        typ: type_check_type(typ, resolved_program, resolved_types, types)?,
                    }),
                );
            }
            tt::FunctionBody::Expression {
                variables: typed_variables,
                value_parameter_variables: parameter_variables
                    .iter()
                    .flat_map(|&variable| variable.map(|id| resolved_variables[id]))
                    .collect(),
                expression: {
                    let expression = Box::new(type_check_expression(
                        expression,
                        resolved_program,
                        resolved_types,
                        types,
                        type_checking_functions,
                        function_signatures,
                        function_bodies,
                        evaluating_consts,
                        resolved_const_values,
                        const_values,
                        &resolved_variables,
                    )?);
                    if fully_type_check_body {
                        fully_type_check_expression(
                            &expression,
                            resolved_program,
                            resolved_types,
                            types,
                            type_checking_functions,
                            function_signatures,
                            function_bodies,
                            evaluating_consts,
                            resolved_const_values,
                            const_values,
                        )?;
                    }
                    expression
                },
            }
        }
    };
    function_bodies.insert(function, body);

    type_checking_functions.remove(function);
    Ok(())
}

fn type_check_const(
    const_: ti::ConstId,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
) -> Result<tt::ConstId, TypeCheckingError> {
    if let Some(&id) = resolved_const_values.get(const_) {
        return Ok(id);
    }
    if evaluating_consts.insert(const_, ()).is_some() {
        return Err(TypeCheckingError {
            location: resolved_program.consts[const_].location,
            kind: TypeCheckingErrorKind::CylicDependency,
        });
    }

    let value = match resolved_program.const_values[const_] {
        ti::ConstValue::Value {
            ref variables,
            ref value,
        } => {
            let mut resolved_variables = IdMap::new();
            let mut typed_variables = IdVec::new();
            for (
                id,
                &ti::Variable {
                    location,
                    name,
                    typ,
                },
            ) in variables.iter()
            {
                resolved_variables.insert(
                    id,
                    typed_variables.push(tt::Variable {
                        location,
                        name,
                        typ: type_check_type(typ, resolved_program, resolved_types, types)?,
                    }),
                );
            }
            let value = type_check_expression(
                value,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
                &resolved_variables,
            )?;

            fully_type_check_expression(
                &value,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
            interpret_expression(
                &value,
                types,
                function_signatures,
                function_bodies,
                const_values,
                &mut IdMap::new(),
            )
        }
    };

    let id = const_values.push(value);
    resolved_const_values.insert(const_, id);

    evaluating_consts.remove(const_);
    Ok(id)
}

fn type_check_expression(
    &ti::Expression {
        location,
        typ,
        ref kind,
    }: &ti::Expression,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
    resolved_variables: &IdMap<ti::VariableId, tt::VariableId>,
) -> Result<tt::Expression, TypeCheckingError> {
    let typ = type_check_type(typ, resolved_program, resolved_types, types)?;
    Ok(tt::Expression {
        location,
        typ,
        kind: match *kind {
            ti::ExpressionKind::Place(ref place) => {
                tt::ExpressionKind::Place(Box::new(type_check_place(
                    place,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?))
            }

            ti::ExpressionKind::Integer(value) => tt::ExpressionKind::Place(Box::new(tt::Place {
                location,
                typ,
                kind: tt::PlaceKind::Const(const_values.push(Value::Integer(
                    match types[typ].kind {
                        tt::TypeKind::Integer(integer_type_kind) => match integer_type_kind {
                            ti::IntegerTypeKind::I64 => {
                                if value > i64::MAX as _ {
                                    return Err(TypeCheckingError {
                                        location,
                                        kind: TypeCheckingErrorKind::ValueTooBigForI64(value),
                                    });
                                }
                                IntegerValue::I64(value as i64)
                            }
                        },

                        _ => unreachable!(),
                    },
                ))),
            })),

            ti::ExpressionKind::Block {
                ref statements,
                ref last_expression,
            } => tt::ExpressionKind::Block {
                statements: statements
                    .iter()
                    .map(|statement| {
                        type_check_statement(
                            statement,
                            resolved_program,
                            resolved_types,
                            types,
                            type_checking_functions,
                            function_signatures,
                            function_bodies,
                            evaluating_consts,
                            resolved_const_values,
                            const_values,
                            resolved_variables,
                        )
                    })
                    .collect::<Result<_, TypeCheckingError>>()?,
                last_expression: Box::new(type_check_expression(
                    last_expression,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?),
            },

            ti::ExpressionKind::Constructor { ref members } => match types[typ].kind {
                tt::TypeKind::Struct {
                    members: ref struct_members,
                } => {
                    let mut struct_member_indices = struct_members
                        .iter()
                        .enumerate()
                        .map(|(index, struct_member)| (index, struct_member.name))
                        .collect::<Vec<_>>();
                    let members = members
                        .iter()
                        .map(
                            |&ti::ConstructorMember {
                                 location,
                                 name,
                                 ref value,
                             }| {
                                Ok(tt::ConstructorMember {
                                    location,
                                    member_index: struct_member_indices
                                        .remove(
                                            struct_member_indices
                                                .iter()
                                                .position(|&(_, member_name)| member_name == name)
                                                .ok_or(TypeCheckingError {
                                                    location,
                                                    kind: TypeCheckingErrorKind::UnknownMember {
                                                        name,
                                                    },
                                                })?,
                                        )
                                        .0,
                                    value: type_check_expression(
                                        value,
                                        resolved_program,
                                        resolved_types,
                                        types,
                                        type_checking_functions,
                                        function_signatures,
                                        function_bodies,
                                        evaluating_consts,
                                        resolved_const_values,
                                        const_values,
                                        resolved_variables,
                                    )?,
                                })
                            },
                        )
                        .collect::<Result<_, TypeCheckingError>>()?;
                    if let Some((_, name)) = struct_member_indices.into_iter().next() {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::MemberLeftUninitialised { name },
                        });
                    }
                    tt::ExpressionKind::StructConstructor { members }
                }

                tt::TypeKind::Enum {
                    members: ref enum_members,
                } => {
                    let [
                        ti::ConstructorMember {
                            location,
                            name,
                            ref value,
                        },
                    ] = **members
                    else {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::OnlyOneEnumMemberCanBeInitialised,
                        });
                    };
                    tt::ExpressionKind::EnumConstructor {
                        member: Box::new(tt::ConstructorMember {
                            location,
                            member_index: enum_members
                                .iter()
                                .position(|enum_member| enum_member.name == name)
                                .ok_or(TypeCheckingError {
                                    location,
                                    kind: TypeCheckingErrorKind::UnknownMember { name },
                                })?,
                            value: type_check_expression(
                                value,
                                resolved_program,
                                resolved_types,
                                types,
                                type_checking_functions,
                                function_signatures,
                                function_bodies,
                                evaluating_consts,
                                resolved_const_values,
                                const_values,
                                resolved_variables,
                            )?,
                        }),
                    }
                }

                _ => unreachable!(),
            },

            ti::ExpressionKind::Unary {
                operator,
                ref operand,
            } => {
                let operand = Box::new(type_check_expression(
                    operand,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                tt::ExpressionKind::Unary {
                    operator: match (&types[operand.typ].kind, &types[typ].kind) {
                        (
                            &tt::TypeKind::Integer(ti::IntegerTypeKind::I64),
                            &tt::TypeKind::Integer(ti::IntegerTypeKind::I64),
                        ) => match operator {
                            ast::UnaryOperator::Plus => tt::UnaryOperator::Identity,
                            ast::UnaryOperator::Negate => tt::UnaryOperator::NegateI64,
                        },

                        _ => {
                            return Err(TypeCheckingError {
                                location,
                                kind: TypeCheckingErrorKind::UnknownUnaryOperatorForType {
                                    operand_typ: operand.typ,
                                    result_typ: typ,
                                },
                            });
                        }
                    },
                    operand,
                }
            }

            ti::ExpressionKind::Binary {
                ref left,
                operator,
                ref right,
            } => {
                let left = Box::new(type_check_expression(
                    left,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                let right = Box::new(type_check_expression(
                    right,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                tt::ExpressionKind::Binary {
                    operator: match (
                        &types[left.typ].kind,
                        &types[right.typ].kind,
                        &types[typ].kind,
                    ) {
                        (
                            &tt::TypeKind::Integer(ti::IntegerTypeKind::I64),
                            &tt::TypeKind::Integer(ti::IntegerTypeKind::I64),
                            &tt::TypeKind::Integer(ti::IntegerTypeKind::I64),
                        ) => match operator {
                            ast::BinaryOperator::Add => tt::BinaryOperator::AddI64,
                            ast::BinaryOperator::Subtract => tt::BinaryOperator::SubtractI64,
                            ast::BinaryOperator::Multiply => tt::BinaryOperator::MultiplyI64,
                            ast::BinaryOperator::Divide => tt::BinaryOperator::DivideI64,
                        },

                        _ => {
                            return Err(TypeCheckingError {
                                location,
                                kind: TypeCheckingErrorKind::UnknownBinaryOperatorForType {
                                    left_typ: left.typ,
                                    right_typ: right.typ,
                                    result_typ: typ,
                                },
                            });
                        }
                    },
                    left,
                    right,
                }
            }

            ti::ExpressionKind::Call {
                ref operand,
                ref arguments,
            } => {
                let operand = Box::new(type_check_expression(
                    operand,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);

                let mut value_arguments = vec![];
                for argument in arguments {
                    match argument.kind {
                        ti::ArgumentKind::Value(ref expression) => {
                            value_arguments.push(type_check_expression(
                                expression,
                                resolved_program,
                                resolved_types,
                                types,
                                type_checking_functions,
                                function_signatures,
                                function_bodies,
                                evaluating_consts,
                                resolved_const_values,
                                const_values,
                                resolved_variables,
                            )?);
                        }
                    }
                }

                tt::ExpressionKind::Call {
                    operand,
                    value_arguments: value_arguments.into_boxed_slice(),
                }
            }
        },
    })
}

fn type_check_statement(
    &ti::Statement { location, ref kind }: &ti::Statement,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
    resolved_variables: &IdMap<ti::VariableId, tt::VariableId>,
) -> Result<tt::Statement, TypeCheckingError> {
    Ok(tt::Statement {
        location,
        kind: match *kind {
            ti::StatementKind::Expression(ref expression) => {
                tt::StatementKind::Expression(Box::new(type_check_expression(
                    expression,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?))
            }

            ti::StatementKind::Assignment {
                ref pattern,
                ref value,
            } => {
                let pattern = Box::new(type_check_pattern(
                    pattern,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                let value = Box::new(type_check_expression(
                    value,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                check_pattern_assignable(&pattern)?;
                tt::StatementKind::Assignment { pattern, value }
            }
        },
    })
}

fn type_check_place(
    &ti::Place {
        location,
        typ,
        ref kind,
    }: &ti::Place,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
    resolved_variables: &IdMap<ti::VariableId, tt::VariableId>,
) -> Result<tt::Place, TypeCheckingError> {
    let typ = type_check_type(typ, resolved_program, resolved_types, types)?;
    Ok(tt::Place {
        location,
        typ,
        kind: match *kind {
            ti::PlaceKind::Variable(id) => tt::PlaceKind::Variable(resolved_variables[id]),
            ti::PlaceKind::Function(id) => tt::PlaceKind::Function(id),
            ti::PlaceKind::Const(id) => tt::PlaceKind::Const(type_check_const(
                id,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?),

            ti::PlaceKind::Expression(ref expression) => {
                tt::PlaceKind::Expression(Box::new(type_check_expression(
                    expression,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?))
            }

            ti::PlaceKind::MemberAccess { ref operand, name } => {
                let operand = Box::new(type_check_place(
                    operand,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?);
                match types[operand.typ].kind {
                    tt::TypeKind::Struct { ref members } => tt::PlaceKind::StructMemberAccess {
                        operand,
                        member_index: members
                            .iter()
                            .position(|member| member.name == name)
                            .unwrap(),
                    },

                    tt::TypeKind::Enum { .. } => {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::CannotAccessMemberOfEnum,
                        });
                    }

                    _ => unreachable!(),
                }
            }
        },
    })
}

fn type_check_pattern(
    &ti::Pattern {
        location,
        typ,
        ref kind,
    }: &ti::Pattern,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
    resolved_variables: &IdMap<ti::VariableId, tt::VariableId>,
) -> Result<tt::Pattern, TypeCheckingError> {
    let typ = type_check_type(typ, resolved_program, resolved_types, types)?;
    Ok(tt::Pattern {
        location,
        typ,
        kind: match *kind {
            ti::PatternKind::Place(ref place) => {
                tt::PatternKind::Place(Box::new(type_check_place(
                    place,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                    resolved_variables,
                )?))
            }

            ti::PatternKind::Integer(value) => tt::PatternKind::Place(Box::new(tt::Place {
                location,
                typ,
                kind: tt::PlaceKind::Const(const_values.push(Value::Integer(
                    match types[typ].kind {
                        tt::TypeKind::Integer(integer_type_kind) => match integer_type_kind {
                            ti::IntegerTypeKind::I64 => {
                                if value > i64::MAX as _ {
                                    return Err(TypeCheckingError {
                                        location,
                                        kind: TypeCheckingErrorKind::ValueTooBigForI64(value),
                                    });
                                }
                                IntegerValue::I64(value as i64)
                            }
                        },

                        _ => unreachable!(),
                    },
                ))),
            })),

            ti::PatternKind::Deconstructor { ref members } => match types[typ].kind {
                tt::TypeKind::Struct {
                    members: ref struct_members,
                } => {
                    let mut struct_member_indices = struct_members
                        .iter()
                        .enumerate()
                        .map(|(index, struct_member)| (index, struct_member.name))
                        .collect::<Vec<_>>();
                    let members = members
                        .iter()
                        .map(
                            |&ti::DeconstructorMember {
                                 location,
                                 name,
                                 ref pattern,
                             }| {
                                Ok(tt::DeconstructorMember {
                                    location,
                                    member_index: struct_member_indices
                                        .remove(
                                            struct_member_indices
                                                .iter()
                                                .position(|&(_, member_name)| member_name == name)
                                                .ok_or(TypeCheckingError {
                                                    location,
                                                    kind: TypeCheckingErrorKind::UnknownMember {
                                                        name,
                                                    },
                                                })?,
                                        )
                                        .0,
                                    pattern: type_check_pattern(
                                        pattern,
                                        resolved_program,
                                        resolved_types,
                                        types,
                                        type_checking_functions,
                                        function_signatures,
                                        function_bodies,
                                        evaluating_consts,
                                        resolved_const_values,
                                        const_values,
                                        resolved_variables,
                                    )?,
                                })
                            },
                        )
                        .collect::<Result<_, TypeCheckingError>>()?;
                    if let Some((_, name)) = struct_member_indices.into_iter().next() {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::MemberLeftUninitialised { name },
                        });
                    }
                    tt::PatternKind::StructDeconstructor { members }
                }

                tt::TypeKind::Enum {
                    members: ref enum_members,
                } => {
                    let [
                        ti::DeconstructorMember {
                            location,
                            name,
                            ref pattern,
                        },
                    ] = **members
                    else {
                        return Err(TypeCheckingError {
                            location,
                            kind: TypeCheckingErrorKind::OnlyOneEnumMemberCanBeInitialised,
                        });
                    };
                    tt::PatternKind::EnumDeconstructor {
                        member: Box::new(tt::DeconstructorMember {
                            location,
                            member_index: enum_members
                                .iter()
                                .position(|enum_member| enum_member.name == name)
                                .ok_or(TypeCheckingError {
                                    location,
                                    kind: TypeCheckingErrorKind::UnknownMember { name },
                                })?,
                            pattern: type_check_pattern(
                                pattern,
                                resolved_program,
                                resolved_types,
                                types,
                                type_checking_functions,
                                function_signatures,
                                function_bodies,
                                evaluating_consts,
                                resolved_const_values,
                                const_values,
                                resolved_variables,
                            )?,
                        }),
                    }
                }

                _ => unreachable!(),
            },

            ti::PatternKind::Let(id) => tt::PatternKind::Let(resolved_variables[id]),
        },
    })
}

fn check_place_assignable(
    &tt::Place {
        location,
        typ: _,
        ref kind,
    }: &tt::Place,
) -> Result<(), TypeCheckingError> {
    let assignable = match *kind {
        tt::PlaceKind::Variable(_) => true,
        tt::PlaceKind::Function(_) => false,
        tt::PlaceKind::Const(_) => false,
        tt::PlaceKind::Expression(_) => false,
        tt::PlaceKind::StructMemberAccess {
            ref operand,
            member_index: _,
        } => {
            check_place_assignable(operand)?;
            true
        }
    };

    if assignable {
        Ok(())
    } else {
        Err(TypeCheckingError {
            location,
            kind: TypeCheckingErrorKind::PlaceIsNotAssignable,
        })
    }
}

fn check_pattern_assignable(
    &tt::Pattern {
        location,
        typ: _,
        ref kind,
    }: &tt::Pattern,
) -> Result<(), TypeCheckingError> {
    let assignable = match *kind {
        tt::PatternKind::Place(ref place) => {
            check_place_assignable(place)?;
            true
        }
        tt::PatternKind::StructDeconstructor { ref members } => {
            members
                .iter()
                .try_for_each(|member| check_pattern_assignable(&member.pattern))?;
            true
        }
        tt::PatternKind::EnumDeconstructor { ref member } => {
            check_pattern_assignable(&member.pattern)?;
            true
        }
        tt::PatternKind::Let(_) => true,
    };

    if assignable {
        Ok(())
    } else {
        Err(TypeCheckingError {
            location,
            kind: TypeCheckingErrorKind::PatternIsNotAssignable,
        })
    }
}

fn fully_type_check_expression(
    expression: &tt::Expression,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
) -> Result<(), TypeCheckingError> {
    match expression.kind {
        tt::ExpressionKind::Place(ref place) => {
            fully_type_check_place(
                place,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::ExpressionKind::StructConstructor { ref members } => {
            for member in members {
                fully_type_check_expression(
                    &member.value,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                )?;
            }
        }

        tt::ExpressionKind::EnumConstructor { ref member } => {
            fully_type_check_expression(
                &member.value,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                fully_type_check_statement(
                    statement,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                )?;
            }
            fully_type_check_expression(
                last_expression,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::ExpressionKind::Unary {
            operator: _,
            ref operand,
        } => {
            fully_type_check_expression(
                operand,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::ExpressionKind::Binary {
            ref left,
            operator: _,
            ref right,
        } => {
            fully_type_check_expression(
                left,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
            fully_type_check_expression(
                right,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::ExpressionKind::Call {
            ref operand,
            ref value_arguments,
        } => {
            fully_type_check_expression(
                operand,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
            for argument in value_arguments {
                fully_type_check_expression(
                    argument,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                )?;
            }
        }
    }
    Ok(())
}

fn fully_type_check_place(
    place: &tt::Place,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
) -> Result<(), TypeCheckingError> {
    match place.kind {
        tt::PlaceKind::Variable(_) => {}

        tt::PlaceKind::Function(id) => {
            type_check_function_body(
                id,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
                true,
            )?;
        }

        tt::PlaceKind::Const(_) => {}

        tt::PlaceKind::Expression(ref expression) => {
            fully_type_check_expression(
                expression,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::PlaceKind::StructMemberAccess {
            ref operand,
            member_index: _,
        } => {
            fully_type_check_place(
                operand,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }
    }
    Ok(())
}

fn fully_type_check_statement(
    statement: &tt::Statement,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
) -> Result<(), TypeCheckingError> {
    match statement.kind {
        tt::StatementKind::Expression(ref expression) => {
            fully_type_check_expression(
                expression,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            fully_type_check_pattern(
                pattern,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
            fully_type_check_expression(
                value,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }
    }
    Ok(())
}

fn fully_type_check_pattern(
    pattern: &tt::Pattern,
    resolved_program: &ResolvedProgram,
    resolved_types: &mut IdMap<ti::TypeId, tt::TypeId>,
    types: &mut IdVec<tt::TypeId, tt::Type>,
    type_checking_functions: &mut IdMap<ti::FunctionId, ()>,
    function_signatures: &mut IdMap<ti::FunctionId, tt::FunctionSignature>,
    function_bodies: &mut IdMap<ti::FunctionId, tt::FunctionBody>,
    evaluating_consts: &mut IdMap<ti::ConstId, ()>,
    resolved_const_values: &mut IdMap<ti::ConstId, tt::ConstId>,
    const_values: &mut IdVec<tt::ConstId, Value>,
) -> Result<(), TypeCheckingError> {
    match pattern.kind {
        tt::PatternKind::Place(ref place) => {
            fully_type_check_place(
                place,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::PatternKind::StructDeconstructor { ref members } => {
            for member in members {
                fully_type_check_pattern(
                    &member.pattern,
                    resolved_program,
                    resolved_types,
                    types,
                    type_checking_functions,
                    function_signatures,
                    function_bodies,
                    evaluating_consts,
                    resolved_const_values,
                    const_values,
                )?;
            }
        }

        tt::PatternKind::EnumDeconstructor { ref member } => {
            fully_type_check_pattern(
                &member.pattern,
                resolved_program,
                resolved_types,
                types,
                type_checking_functions,
                function_signatures,
                function_bodies,
                evaluating_consts,
                resolved_const_values,
                const_values,
            )?;
        }

        tt::PatternKind::Let(_) => {}
    }
    Ok(())
}
