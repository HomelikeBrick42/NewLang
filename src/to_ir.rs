use crate::{
    ast,
    inferring_tree::{self as it, BuiltinType},
    ir,
    lexer::SourceLocation,
};
use slotmap::{SecondaryMap, SlotMap};

pub fn convert_function(
    function_id: it::FunctionId,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
) -> Result<ir::FunctionId, ToIrError> {
    if let Some(&id) = ir_program.inferring_functions_map.get(function_id) {
        return Ok(id);
    }

    let function = &inferring_program.functions[function_id];
    let function_body = &inferring_program.function_bodies[function_id];

    let id = ir_program.functions.insert(ir::Function {
        location: function.location,
        name: function.name,
        body: ir::FunctionBody::Resolving,
    });
    ir_program.inferring_functions_map.insert(function_id, id);
    ir_program.functions[id].body = match *function_body {
        it::FunctionBody::Expression {
            variables: ref inferring_variables,
            ref parameter_variables,
            ref expression,
        } => {
            let return_type = convert_type(function.return_type, ir_program, inferring_program)?;
            let expression_type = convert_type(expression.typ, ir_program, inferring_program)?;
            expect_types_same(expression.location, expression_type, return_type)?;

            let mut variables = SlotMap::with_key();
            let return_variable = variables.insert(ir::Variable {
                location: function.location,
                name: None,
                typ: return_type,
            });

            let mut inferring_variables_map = SecondaryMap::new();
            for (id, inferred_variable) in inferring_variables {
                inferring_variables_map.insert(
                    id,
                    variables.insert(ir::Variable {
                        location: inferred_variable.location,
                        name: inferred_variable.name,
                        typ: convert_type(inferred_variable.typ, ir_program, inferring_program)?,
                    }),
                );
            }

            let mut blocks = SlotMap::with_key();
            let entry_block = blocks.insert(ir::Block {
                instructions: vec![],
                jump: ir::Jump {
                    location: function.location,
                    kind: ir::JumpKind::Return {
                        variable: return_variable,
                    },
                },
            });

            let mut current_block = entry_block;
            let expression_result = emit_expression(
                expression,
                ir_program,
                inferring_program,
                &mut variables,
                &inferring_variables_map,
                &mut blocks,
                &mut current_block,
            )?;

            blocks[current_block].instructions.push(ir::Instruction {
                location: function.location,
                kind: ir::InstructionKind::Copy {
                    source: expression_result,
                    destination: return_variable,
                },
            });

            ir::FunctionBody::Body {
                variables,
                parameter_variables: parameter_variables
                    .iter()
                    .copied()
                    .flatten()
                    .map(|id| inferring_variables_map[id])
                    .collect(),
                blocks,
                entry_block,
            }
        }

        it::FunctionBody::Builtin(builtin_function_body) => match builtin_function_body {
            ast::BuiltinFunctionBody::PrintI64 => {
                let unit_type = ir_program.builtin_types[BuiltinType::Unit].unwrap();
                let i64_type = ir_program.builtin_types[BuiltinType::I64].unwrap();
                let runtime_type = ir_program.builtin_types[BuiltinType::Runtime].unwrap();

                {
                    assert_eq!(function.parameters.len(), 2);
                    let it::Parameter {
                        location: first_parameter_location,
                        name: _,
                        kind:
                            it::ParameterKind::Value {
                                typ: first_parameter_type,
                            },
                    } = function.parameters[0];
                    expect_types_same(
                        first_parameter_location,
                        convert_type(first_parameter_type, ir_program, inferring_program)?,
                        i64_type,
                    )?;

                    let it::Parameter {
                        location: second_parameter_location,
                        name: _,
                        kind:
                            it::ParameterKind::Value {
                                typ: second_parameter_type,
                            },
                    } = function.parameters[1];
                    expect_types_same(
                        second_parameter_location,
                        convert_type(second_parameter_type, ir_program, inferring_program)?,
                        runtime_type,
                    )?;

                    expect_types_same(
                        function.location,
                        convert_type(function.return_type, ir_program, inferring_program)?,
                        unit_type,
                    )?;
                }

                let mut variables = SlotMap::with_key();
                let i64_value = variables.insert(ir::Variable {
                    location: function.location,
                    name: None,
                    typ: i64_type,
                });
                let runtime_value = variables.insert(ir::Variable {
                    location: function.location,
                    name: None,
                    typ: runtime_type,
                });
                let unit_value = variables.insert(ir::Variable {
                    location: function.location,
                    name: None,
                    typ: unit_type,
                });

                let parameter_variables = Box::new([i64_value, runtime_value]);

                let mut blocks = SlotMap::with_key();
                let entry_block = blocks.insert(ir::Block {
                    instructions: vec![ir::Instruction {
                        location: function.location,
                        kind: ir::InstructionKind::PrintI64(i64_value),
                    }],
                    jump: ir::Jump {
                        location: function.location,
                        kind: ir::JumpKind::Return {
                            variable: unit_value,
                        },
                    },
                });

                ir::FunctionBody::Body {
                    variables,
                    parameter_variables,
                    blocks,
                    entry_block,
                }
            }
        },
    };
    Ok(id)
}

pub fn emit_expression(
    &it::Expression {
        location,
        typ,
        ref kind,
    }: &it::Expression,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
    variables: &mut SlotMap<ir::VariableId, ir::Variable>,
    inferring_variables_map: &SecondaryMap<it::VariableId, ir::VariableId>,
    blocks: &mut SlotMap<ir::BlockId, ir::Block>,
    current_block: &mut ir::BlockId,
) -> Result<ir::VariableId, ToIrError> {
    let typ = convert_type(typ, ir_program, inferring_program)?;
    Ok(match *kind {
        it::ExpressionKind::Place(ref place) => {
            return emit_place_copy(
                place,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            );
        }

        it::ExpressionKind::Integer(value) => {
            let i64_type = ir_program.builtin_types[BuiltinType::I64].unwrap();
            expect_types_same(location, typ, i64_type)?;
            if value > i64::MAX as _ {
                return Err(ToIrError {
                    location,
                    kind: ToIrErrorKind::IntegerTooBigForI64,
                });
            }

            let variable = variables.insert(ir::Variable {
                location,
                name: None,
                typ,
            });
            blocks[*current_block].instructions.push(ir::Instruction {
                location,
                kind: ir::InstructionKind::ConstantI64 {
                    destination: variable,
                    value: value as _,
                },
            });
            variable
        }

        it::ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            for statement in statements {
                emit_statement(
                    statement,
                    ir_program,
                    inferring_program,
                    variables,
                    inferring_variables_map,
                    blocks,
                    current_block,
                )?;
            }

            let last_expression_type =
                convert_type(last_expression.typ, ir_program, inferring_program)?;
            expect_types_same(location, last_expression_type, typ)?;
            emit_expression(
                last_expression,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            )?
        }

        it::ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => {
            // TODO: actually type check this

            let operand = emit_expression(
                operand,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            )?;

            let arguments = arguments
                .iter()
                .map(|argument| {
                    Ok(match *argument {
                        it::Argument::Value { ref expression } => ir::Argument::Value {
                            variable: emit_expression(
                                expression,
                                ir_program,
                                inferring_program,
                                variables,
                                inferring_variables_map,
                                blocks,
                                current_block,
                            )?,
                        },
                    })
                })
                .collect::<Result<_, ToIrError>>()?;

            let return_variable = variables.insert(ir::Variable {
                location,
                name: None,
                typ,
            });

            let final_jump = std::mem::replace(
                &mut blocks[*current_block].jump,
                ir::Jump {
                    location,
                    kind: ir::JumpKind::Unreachable,
                },
            );
            let return_to = blocks.insert(ir::Block {
                instructions: vec![],
                jump: final_jump,
            });
            blocks[*current_block].jump.kind = ir::JumpKind::Call {
                operand,
                arguments,
                return_variable,
                return_to,
            };
            *current_block = return_to;

            return_variable
        }
    })
}

pub fn emit_statement(
    &it::Statement {
        #[expect(unused)]
        location,
        ref kind,
    }: &it::Statement,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
    variables: &mut SlotMap<ir::VariableId, ir::Variable>,
    inferring_variables_map: &SecondaryMap<it::VariableId, ir::VariableId>,
    blocks: &mut SlotMap<ir::BlockId, ir::Block>,
    current_block: &mut ir::BlockId,
) -> Result<(), ToIrError> {
    match *kind {
        it::StatementKind::Expression(ref expression) => {
            emit_expression(
                expression,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            )?;
        }

        it::StatementKind::Assignment {
            ref pattern,
            ref value,
        } => {
            let value = emit_expression(
                value,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            )?;
            assign_pattern(
                pattern,
                value,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            )?;
        }
    }
    Ok(())
}

pub fn emit_place_copy(
    &it::Place {
        location,
        typ,
        ref kind,
    }: &it::Place,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
    variables: &mut SlotMap<ir::VariableId, ir::Variable>,
    inferring_variables_map: &SecondaryMap<it::VariableId, ir::VariableId>,
    blocks: &mut SlotMap<ir::BlockId, ir::Block>,
    current_block: &mut ir::BlockId,
) -> Result<ir::VariableId, ToIrError> {
    let typ = convert_type(typ, ir_program, inferring_program)?;
    Ok(match *kind {
        it::PlaceKind::Function(_) => variables.insert(ir::Variable {
            location,
            name: None,
            typ,
        }),

        it::PlaceKind::Variable(id) => {
            let variable = inferring_variables_map[id];
            expect_types_same(location, typ, variables[variable].typ)?;

            let copy = variables.insert(ir::Variable {
                location,
                name: None,
                typ,
            });
            blocks[*current_block].instructions.push(ir::Instruction {
                location,
                kind: ir::InstructionKind::Copy {
                    source: variable,
                    destination: copy,
                },
            });
            copy
        }
    })
}

pub fn assign_pattern(
    &it::Pattern {
        location,
        typ,
        ref kind,
    }: &it::Pattern,
    value: ir::VariableId,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
    variables: &mut SlotMap<ir::VariableId, ir::Variable>,
    inferring_variables_map: &SecondaryMap<it::VariableId, ir::VariableId>,
    blocks: &mut SlotMap<ir::BlockId, ir::Block>,
    current_block: &mut ir::BlockId,
) -> Result<(), ToIrError> {
    let typ = convert_type(typ, ir_program, inferring_program)?;
    match *kind {
        it::PatternKind::Place(ref place) => {
            let place_type = convert_type(place.typ, ir_program, inferring_program)?;
            expect_types_same(location, typ, place_type)?;
            return assign_place(
                place,
                value,
                ir_program,
                inferring_program,
                variables,
                inferring_variables_map,
                blocks,
                current_block,
            );
        }

        it::PatternKind::Integer(_) => {
            return Err(ToIrError {
                location,
                kind: ToIrErrorKind::PatternNotAssignable,
            });
        }

        it::PatternKind::Let(variable) => {
            let variable = inferring_variables_map[variable];
            expect_types_same(location, variables[variable].typ, typ)?;
            blocks[*current_block].instructions.push(ir::Instruction {
                location,
                kind: ir::InstructionKind::Copy {
                    source: value,
                    destination: variable,
                },
            });
        }
    }
    Ok(())
}

pub fn assign_place(
    &it::Place {
        location,
        typ,
        ref kind,
    }: &it::Place,
    value: ir::VariableId,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
    variables: &mut SlotMap<ir::VariableId, ir::Variable>,
    inferring_variables_map: &SecondaryMap<it::VariableId, ir::VariableId>,
    blocks: &mut SlotMap<ir::BlockId, ir::Block>,
    current_block: &mut ir::BlockId,
) -> Result<(), ToIrError> {
    let typ = convert_type(typ, ir_program, inferring_program)?;
    match *kind {
        it::PlaceKind::Function(_) => {
            return Err(ToIrError {
                location,
                kind: ToIrErrorKind::PatternNotAssignable,
            });
        }

        it::PlaceKind::Variable(variable) => {
            let variable = inferring_variables_map[variable];
            expect_types_same(location, variables[variable].typ, typ)?;
            blocks[*current_block].instructions.push(ir::Instruction {
                location,
                kind: ir::InstructionKind::Copy {
                    source: value,
                    destination: variable,
                },
            });
        }
    }
    Ok(())
}

pub fn convert_type(
    type_id: it::TypeId,
    ir_program: &mut ir::Program,
    inferring_program: &it::Program,
) -> Result<ir::TypeId, ToIrError> {
    if let Some(&id) = ir_program.inferring_types_map.get(type_id) {
        return Ok(id);
    }

    let id = ir_program.types.insert(ir::Type {
        location: inferring_program.types[type_id].location,
        kind: ir::TypeKind::Resolving,
    });
    ir_program.inferring_types_map.insert(type_id, id);
    ir_program.types[id].kind = match inferring_program.types[type_id].kind {
        it::TypeKind::Resolving => unreachable!(),
        it::TypeKind::Infer(_) => panic!("uninferred type got to type checking"),

        it::TypeKind::Inferred(inferred_type_id) => {
            let inferred_id = convert_type(inferred_type_id, ir_program, inferring_program)?;
            ir_program.types.remove(id);
            ir_program.inferring_types_map[type_id] = inferred_id;
            return Ok(inferred_id);
        }

        it::TypeKind::Unit => ir::TypeKind::Unit,
        it::TypeKind::Runtime => ir::TypeKind::Runtime,
        it::TypeKind::I64 => ir::TypeKind::I64,
        it::TypeKind::FunctionItem {
            function,
            parameters: _,
            return_type: _,
        } => ir::TypeKind::FunctionItem(convert_function(function, ir_program, inferring_program)?),
    };
    Ok(id)
}

fn expect_types_same(
    location: SourceLocation,
    got: ir::TypeId,
    expected: ir::TypeId,
) -> Result<(), ToIrError> {
    if expected == got {
        Ok(())
    } else {
        Err(ToIrError {
            location,
            kind: ToIrErrorKind::ExpectedTypeButGotType { expected, got },
        })
    }
}

#[derive(Debug)]
pub struct ToIrError {
    pub location: SourceLocation,
    pub kind: ToIrErrorKind,
}

#[derive(Debug)]
pub enum ToIrErrorKind {
    CyclicDependency,
    ExpectedTypeButGotType {
        expected: ir::TypeId,
        got: ir::TypeId,
    },
    IntegerTooBigForI64,
    PatternNotAssignable,
}
