use slotmap::{SecondaryMap, SlotMap};

use crate::{
    ir::{self, BlockId, FunctionId, Program, Variable, VariableId},
    lexer::SourceLocation,
};

pub fn analyze_function(
    function_id: FunctionId,
    program: &Program,
    errors: &mut Vec<SemanticAnalysisError>,
) {
    let function = &program.functions[function_id];
    match function.body {
        ir::FunctionBody::Resolving => {}

        ir::FunctionBody::Body {
            ref variables,
            ref parameter_variables,
            ref blocks,
            entry_block,
        } => {
            struct CheckState {
                variable_initialization: SecondaryMap<VariableId, bool>,
                block: BlockId,
            }

            let mut check_states = vec![CheckState {
                variable_initialization: {
                    let mut variable_initialization = SecondaryMap::new();
                    for (id, _) in variables {
                        variable_initialization.insert(id, false);
                    }
                    for &id in parameter_variables {
                        variable_initialization[id] = true;
                    }
                    variable_initialization
                },
                block: entry_block,
            }];
            while let Some(CheckState {
                mut variable_initialization,
                block,
            }) = check_states.pop()
            {
                let block = &blocks[block];
                for instruction in &block.instructions {
                    match instruction.kind {
                        ir::InstructionKind::Copy {
                            source,
                            destination,
                        } => {
                            require_init(
                                instruction.location,
                                source,
                                program,
                                variables,
                                &variable_initialization,
                                errors,
                            );
                            variable_initialization[destination] = true;
                        }

                        ir::InstructionKind::ConstantI64 {
                            destination,
                            value: _,
                        } => {
                            variable_initialization[destination] = true;
                        }

                        ir::InstructionKind::ConstantFunctionItem { destination } => {
                            variable_initialization[destination] = true;
                        }

                        ir::InstructionKind::AssumeInit { variable } => {
                            variable_initialization[variable] = true;
                        }

                        ir::InstructionKind::PrintI64 { variable } => {
                            require_init(
                                instruction.location,
                                variable,
                                program,
                                variables,
                                &variable_initialization,
                                errors,
                            );
                        }
                    }
                }
                match block.jump.kind {
                    ir::JumpKind::Unreachable => {}

                    ir::JumpKind::Next(block) => {
                        check_states.push(CheckState {
                            variable_initialization,
                            block,
                        });
                    }

                    ir::JumpKind::Return { variable } => {
                        require_init(
                            block.jump.location,
                            variable,
                            program,
                            variables,
                            &variable_initialization,
                            errors,
                        );
                    }

                    ir::JumpKind::Call {
                        operand,
                        ref arguments,
                        return_variable,
                        return_to,
                    } => {
                        require_init(
                            block.jump.location,
                            operand,
                            program,
                            variables,
                            &variable_initialization,
                            errors,
                        );
                        for argument in arguments {
                            match *argument {
                                ir::Argument::Value { variable } => {
                                    require_init(
                                        block.jump.location,
                                        variable,
                                        program,
                                        variables,
                                        &variable_initialization,
                                        errors,
                                    );
                                }
                            }
                        }
                        variable_initialization[return_variable] = true;

                        check_states.push(CheckState {
                            variable_initialization,
                            block: return_to,
                        });
                    }
                }
            }
        }
    }
}

fn require_init(
    location: SourceLocation,
    variable: VariableId,
    #[expect(unused)] program: &Program,
    variables: &SlotMap<VariableId, Variable>,
    variable_initialization: &SecondaryMap<VariableId, bool>,
    errors: &mut Vec<SemanticAnalysisError>,
) {
    if !variable_initialization[variable] {
        errors.push(SemanticAnalysisError {
            location,
            kind: SemanticAnalysisErrorKind::VariableUninitialized {
                variable_location: variables[variable].location,
            },
        });
    }
}

#[derive(Debug)]
pub struct SemanticAnalysisError {
    pub location: SourceLocation,
    pub kind: SemanticAnalysisErrorKind,
}

#[derive(Debug)]
pub enum SemanticAnalysisErrorKind {
    VariableUninitialized { variable_location: SourceLocation },
}
