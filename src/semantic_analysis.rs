use std::collections::hash_map::Entry;

use crate::{
    ir::{self, BlockId, FunctionId, Program, Variable, VariableId},
    lexer::SourceLocation,
};
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap};

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
                came_from: Option<BlockId>,
                block: BlockId,
                traversed_edges: FxHashMap<(BlockId, BlockId), bool>,
                variable_initialization: SecondaryMap<VariableId, bool>,
            }

            let mut check_states = vec![CheckState {
                came_from: None,
                block: entry_block,
                traversed_edges: FxHashMap::default(),
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
            }];
            while let Some(CheckState {
                came_from,
                block: current_block,
                mut traversed_edges,
                mut variable_initialization,
            }) = check_states.pop()
            {
                if let Some(came_from) = came_from {
                    match traversed_edges.entry((came_from, current_block)) {
                        Entry::Vacant(e) => _ = e.insert(false),
                        Entry::Occupied(e) => {
                            if std::mem::replace(e.into_mut(), true) {
                                continue;
                            }
                        }
                    }
                }

                // prevent any further code from accidentally using `came_from`
                #[expect(unused)]
                let came_from = ();

                let block = &blocks[current_block];
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
                            came_from: Some(current_block),
                            block,
                            traversed_edges,
                            variable_initialization,
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
                            came_from: Some(current_block),
                            block: return_to,
                            traversed_edges,
                            variable_initialization,
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
