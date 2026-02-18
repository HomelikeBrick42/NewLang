use crate::{
    idvec::IdMap,
    type_checker::TypedProgram,
    type_inference_tree::{BuiltinFunctionBody, FunctionId},
    typed_tree::{
        BinaryOperator, Expression, ExpressionKind, FunctionBody, IntegerValue, Pattern,
        PatternKind, Place, PlaceKind, Statement, StatementKind, TypeKind, UnaryOperator,
        VariableId,
    },
};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(IntegerValue),
    Struct {
        members: Box<[Value]>,
    },
    Enum {
        member_index: usize,
        member: Box<Value>,
    },
}

pub fn interpret_function(
    function: FunctionId,
    value_arguments: Box<[Value]>,
    typed_program: &TypedProgram,
) -> Value {
    let mut variables = IdMap::new();
    match typed_program.function_bodies[function] {
        FunctionBody::Builtin(builtin_function_body) => match builtin_function_body {
            BuiltinFunctionBody::PrintI64 => {
                let [Value::Integer(IntegerValue::I64(value)), _] = *value_arguments else {
                    unreachable!()
                };
                println!("{value}");

                Value::Struct {
                    members: Box::new([]),
                }
            }
        },

        FunctionBody::Expression {
            variables: _,
            ref value_parameter_variables,
            ref expression,
        } => {
            assert_eq!(value_arguments.len(), value_parameter_variables.len());
            for (&variable, value) in value_parameter_variables.iter().zip(value_arguments) {
                variables.insert(variable, value);
            }
            interpret_expression(expression, typed_program, &mut variables)
        }
    }
}

fn interpret_expression(
    expression: &Expression,
    typed_program: &TypedProgram,
    variables: &mut IdMap<VariableId, Value>,
) -> Value {
    match expression.kind {
        ExpressionKind::Place(ref place) => copy_place(place, typed_program, variables),

        ExpressionKind::Integer(value) => Value::Integer(value),

        ExpressionKind::StructConstructor {
            members: ref constructor_members,
        } => {
            let mut members = vec![
                Value::Struct {
                    members: Box::new([])
                };
                constructor_members.len()
            ]
            .into_boxed_slice();
            for member in constructor_members {
                members[member.member_index] =
                    interpret_expression(&member.value, typed_program, variables);
            }
            Value::Struct { members }
        }

        ExpressionKind::EnumConstructor { ref member } => Value::Enum {
            member_index: member.member_index,
            member: Box::new(interpret_expression(
                &member.value,
                typed_program,
                variables,
            )),
        },

        ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                interpret_statement(statement, typed_program, variables);
            }
            interpret_expression(last_expression, typed_program, variables)
        }

        ExpressionKind::Unary {
            operator,
            ref operand,
        } => {
            let operand = interpret_expression(operand, typed_program, variables);
            match operator {
                UnaryOperator::Identity => operand,

                UnaryOperator::NegateI64 => {
                    let Value::Integer(IntegerValue::I64(value)) = operand else {
                        unreachable!()
                    };
                    Value::Integer(IntegerValue::I64(value.wrapping_neg()))
                }
            }
        }

        ExpressionKind::Binary {
            ref left,
            operator,
            ref right,
        } => {
            let left = interpret_expression(left, typed_program, variables);
            let right = interpret_expression(right, typed_program, variables);
            match operator {
                BinaryOperator::AddI64 => {
                    let Value::Integer(IntegerValue::I64(a)) = left else {
                        unreachable!()
                    };
                    let Value::Integer(IntegerValue::I64(b)) = right else {
                        unreachable!()
                    };
                    Value::Integer(IntegerValue::I64(a.wrapping_add(b)))
                }

                BinaryOperator::SubtractI64 => {
                    let Value::Integer(IntegerValue::I64(a)) = left else {
                        unreachable!()
                    };
                    let Value::Integer(IntegerValue::I64(b)) = right else {
                        unreachable!()
                    };
                    Value::Integer(IntegerValue::I64(a.wrapping_sub(b)))
                }

                BinaryOperator::MultiplyI64 => {
                    let Value::Integer(IntegerValue::I64(a)) = left else {
                        unreachable!()
                    };
                    let Value::Integer(IntegerValue::I64(b)) = right else {
                        unreachable!()
                    };
                    Value::Integer(IntegerValue::I64(a.wrapping_mul(b)))
                }

                BinaryOperator::DivideI64 => {
                    let Value::Integer(IntegerValue::I64(a)) = left else {
                        unreachable!()
                    };
                    let Value::Integer(IntegerValue::I64(b)) = right else {
                        unreachable!()
                    };
                    if b == 0 {
                        Value::Integer(IntegerValue::I64(0))
                    } else {
                        Value::Integer(IntegerValue::I64(a.wrapping_div(b)))
                    }
                }
            }
        }

        ExpressionKind::Call {
            ref operand,
            ref value_arguments,
        } => {
            let function_type = operand.typ;
            let _operand = interpret_expression(operand, typed_program, variables);
            let value_arguments = value_arguments
                .iter()
                .map(|argument| interpret_expression(argument, typed_program, variables))
                .collect();
            match typed_program.types[function_type].kind {
                TypeKind::FunctionItem(id) => {
                    interpret_function(id, value_arguments, typed_program)
                }

                _ => unreachable!(),
            }
        }
    }
}

fn interpret_statement(
    statement: &Statement,
    typed_program: &TypedProgram,
    variables: &mut IdMap<VariableId, Value>,
) {
    match statement.kind {
        StatementKind::Expression(ref expression) => {
            interpret_expression(expression, typed_program, variables);
        }

        StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            let value = interpret_expression(value, typed_program, variables);
            assign_pattern(pattern, value, variables);
        }
    }
}

fn copy_place(
    place: &Place,
    typed_program: &TypedProgram,
    variables: &mut IdMap<VariableId, Value>,
) -> Value {
    match place.kind {
        PlaceKind::Variable(id) => variables[id].clone(),

        PlaceKind::Function(_) => match typed_program.types[place.typ].kind {
            TypeKind::FunctionItem(_) => Value::Struct {
                members: Box::new([]),
            },

            _ => unreachable!(),
        },

        PlaceKind::Expression(ref expression) => {
            interpret_expression(expression, typed_program, variables)
        }

        PlaceKind::StructMemberAccess {
            ref operand,
            member_index,
        } => {
            let Value::Struct { members } = copy_place(operand, typed_program, variables) else {
                unreachable!()
            };
            members[member_index].clone()
        }
    }
}

fn assignable_place<'a>(
    place: &Place,
    variables: &'a mut IdMap<VariableId, Value>,
) -> &'a mut Value {
    match place.kind {
        PlaceKind::Variable(id) => &mut variables[id],

        PlaceKind::Function(_) => unreachable!(),

        PlaceKind::Expression(_) => unreachable!("cannot assign to expression"),

        PlaceKind::StructMemberAccess {
            ref operand,
            member_index,
        } => {
            let Value::Struct { members } = assignable_place(operand, variables) else {
                unreachable!()
            };
            &mut members[member_index]
        }
    }
}

fn assign_pattern(pattern: &Pattern, value: Value, variables: &mut IdMap<VariableId, Value>) {
    match pattern.kind {
        PatternKind::Place(ref place) => *assignable_place(place, variables) = value,

        PatternKind::Integer(_) => unreachable!(),

        PatternKind::StructDeconstructor {
            members: ref deconstructor_members,
        } => {
            let Value::Struct { members } = value else {
                unreachable!()
            };
            for deconstructor_member in deconstructor_members {
                assign_pattern(
                    &deconstructor_member.pattern,
                    members[deconstructor_member.member_index].clone(),
                    variables,
                );
            }
        }

        PatternKind::EnumDeconstructor {
            member: ref deconstructor_member,
        } => {
            let Value::Enum {
                member_index,
                member,
            } = value
            else {
                unreachable!()
            };
            assert_eq!(deconstructor_member.member_index, member_index);
            assign_pattern(pattern, *member, variables);
        }

        PatternKind::Let(id) => {
            variables.insert(id, value);
        }
    }
}
