use crate::{
    idvec::{IdMap, IdSlice},
    typed_tree::{
        ArgumentKind, BinaryOperator, BuiltinFunctionBody, Expression, ExpressionKind,
        FunctionBody, FunctionId, FunctionParameterKind, FunctionSignature, Pattern, PatternKind,
        Statement, StatementKind, Type, TypeId, TypeKind, UnaryOperator, VariableId,
    },
};

#[derive(Debug, Clone)]
pub enum Value {
    I64(i64),
    Struct {
        members: Box<[Value]>,
    },
    Enum {
        member_index: usize,
        value: Box<Value>,
    },
}

pub fn execute_program(
    main_function: FunctionId,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
    types: &IdSlice<TypeId, Type>,
) {
    execute_function(
        main_function,
        &[Value::Struct {
            members: Box::new([]),
        }],
        function_signatures,
        function_bodies,
        types,
    );
}

pub fn execute_function(
    function: FunctionId,
    arguments: &[Value],
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
    types: &IdSlice<TypeId, Type>,
) -> Value {
    let signature = &function_signatures[function];
    let body = &function_bodies[function];

    assert_eq!(
        signature
            .parameters
            .iter()
            .filter(|parameter| matches!(parameter.kind, FunctionParameterKind::Value { .. }))
            .count(),
        arguments.len()
    );

    match body {
        FunctionBody::Builtin(builtin) => match *builtin {
            BuiltinFunctionBody::PrintI64 => match arguments[0] {
                Value::I64(value) => {
                    println!("{value}");
                    Value::Struct {
                        members: Box::new([]),
                    }
                }
                _ => unreachable!(),
            },
        },

        FunctionBody::Expression {
            variables: _,
            parameter_variables,
            expression,
        } => {
            let mut variables = IdMap::new();
            {
                let mut arguments_iter = arguments.iter();
                for &parameter_variable in parameter_variables {
                    if let Some(parameter_variable) = parameter_variable {
                        variables
                            .insert(parameter_variable, arguments_iter.next().unwrap().clone());
                    }
                }
                assert!(arguments_iter.next().is_none());
            }
            execute_expression(
                expression,
                function_signatures,
                function_bodies,
                types,
                &mut variables,
            )
        }
    }
}

pub fn execute_expression(
    expression: &Expression,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
    types: &IdSlice<TypeId, Type>,
    variables: &mut IdMap<VariableId, Value>,
) -> Value {
    match expression.kind {
        ExpressionKind::Variable(id) => variables[id].clone(),

        ExpressionKind::Function(_) => Value::Struct {
            members: Box::new([]),
        },

        ExpressionKind::Integer(value) => match types[expression.typ].kind {
            TypeKind::I64 => Value::I64(value as i64),
            _ => unreachable!(),
        },

        ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                execute_statement(
                    statement,
                    function_signatures,
                    function_bodies,
                    types,
                    variables,
                );
            }
            execute_expression(
                last_expression,
                function_signatures,
                function_bodies,
                types,
                variables,
            )
        }

        ExpressionKind::Constructor { ref members } => match types[expression.typ].kind {
            TypeKind::Struct {
                members: ref struct_members,
            } => {
                assert_eq!(members.len(), struct_members.len());
                let mut members = members.iter().collect::<Box<_>>();
                members.sort_unstable_by_key(|member| {
                    struct_members
                        .iter()
                        .position(|struct_member| struct_member.name == member.name)
                        .unwrap()
                });
                Value::Struct {
                    members: members
                        .into_iter()
                        .map(|member| {
                            execute_expression(
                                &member.value,
                                function_signatures,
                                function_bodies,
                                types,
                                variables,
                            )
                        })
                        .collect(),
                }
            }

            TypeKind::Enum {
                members: ref enum_members,
            } => {
                assert_eq!(members.len(), 1);
                let member = &members[0];
                Value::Enum {
                    member_index: enum_members
                        .iter()
                        .position(|enum_member| enum_member.name == member.name)
                        .unwrap(),
                    value: Box::new(execute_expression(
                        &member.value,
                        function_signatures,
                        function_bodies,
                        types,
                        variables,
                    )),
                }
            }

            _ => unreachable!(),
        },

        ExpressionKind::Unary {
            ref operator,
            ref operand,
        } => {
            let operand = execute_expression(
                operand,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
            match *operator {
                UnaryOperator::Plus => match operand {
                    Value::I64(value) => Value::I64(value),
                    _ => unreachable!(),
                },
                UnaryOperator::Negate => match operand {
                    Value::I64(value) => Value::I64(value.wrapping_neg()),
                    _ => unreachable!(),
                },
            }
        }

        ExpressionKind::Binary {
            ref left,
            ref operator,
            ref right,
        } => {
            let left =
                execute_expression(left, function_signatures, function_bodies, types, variables);
            let right = execute_expression(
                right,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
            match *operator {
                BinaryOperator::Add => match (left, right) {
                    (Value::I64(a), Value::I64(b)) => Value::I64(a.wrapping_add(b)),
                    _ => unreachable!(),
                },
                BinaryOperator::Subtract => match (left, right) {
                    (Value::I64(a), Value::I64(b)) => Value::I64(a.wrapping_sub(b)),
                    _ => unreachable!(),
                },
                BinaryOperator::Multiply => match (left, right) {
                    (Value::I64(a), Value::I64(b)) => Value::I64(a.wrapping_mul(b)),
                    _ => unreachable!(),
                },
                BinaryOperator::Divide => match (left, right) {
                    (Value::I64(a), Value::I64(b)) => Value::I64(a.checked_div(b).unwrap_or(0)),
                    _ => unreachable!(),
                },
            }
        }

        ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            let operand_typ = operand.typ;
            #[expect(unused)]
            let operand = execute_expression(
                operand,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
            let arguments = arguments
                .iter()
                .filter_map(|argument| match argument.kind {
                    ArgumentKind::Value(ref expression) => Some(execute_expression(
                        expression,
                        function_signatures,
                        function_bodies,
                        types,
                        variables,
                    )),
                    ArgumentKind::Type(_) => None,
                })
                .collect::<Vec<_>>();
            match types[operand_typ].kind {
                TypeKind::FunctionItem(id) => {
                    execute_function(id, &arguments, function_signatures, function_bodies, types)
                }
                _ => unreachable!(),
            }
        }

        ExpressionKind::MemberAccess { ref operand, name } => {
            let index = match types[operand.typ].kind {
                TypeKind::Struct { ref members } => members
                    .iter()
                    .position(|member| member.name == name)
                    .unwrap(),
                _ => unreachable!(),
            };
            let operand = execute_expression(
                operand,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
            match operand {
                Value::Struct { members } => members[index].clone(),
                _ => unreachable!(),
            }
        }
    }
}

pub fn execute_statement(
    statement: &Statement,
    function_signatures: &IdSlice<FunctionId, FunctionSignature>,
    function_bodies: &IdMap<FunctionId, FunctionBody>,
    types: &IdSlice<TypeId, Type>,
    variables: &mut IdMap<VariableId, Value>,
) {
    match statement.kind {
        StatementKind::Expression(ref expression) => {
            execute_expression(
                expression,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            let value = execute_expression(
                value,
                function_signatures,
                function_bodies,
                types,
                variables,
            );
            assign_pattern(pattern, value, types, variables);
        }
    }
}

pub fn assign_pattern(
    pattern: &Pattern,
    value: Value,
    types: &IdSlice<TypeId, Type>,
    variables: &mut IdMap<VariableId, Value>,
) {
    match pattern.kind {
        PatternKind::Variable(id) | PatternKind::Let(id) => {
            variables.insert(id, value);
        }

        PatternKind::Function(_) => {}
        PatternKind::Integer(_) => {}

        PatternKind::Destructor { ref members } => match types[pattern.typ].kind {
            TypeKind::Struct {
                members: ref struct_members,
            } => {
                assert_eq!(members.len(), struct_members.len());
                let mut members = members.iter().collect::<Box<_>>();
                members.sort_unstable_by_key(|member| {
                    struct_members
                        .iter()
                        .position(|struct_member| struct_member.name == member.name)
                        .unwrap()
                });
                let Value::Struct { members: values } = value else {
                    unreachable!()
                };
                for (member, value) in members.into_iter().zip(values) {
                    assign_pattern(&member.pattern, value, types, variables);
                }
            }

            TypeKind::Enum {
                members: ref enum_members,
            } => {
                assert_eq!(members.len(), 1);
                let member = &members[0];
                let Value::Enum {
                    member_index,
                    value,
                } = value
                else {
                    unreachable!()
                };
                assert_eq!(enum_members[member_index].name, member.name);
                assign_pattern(&member.pattern, *value, types, variables);
            }

            _ => unreachable!(),
        },
    }
}
