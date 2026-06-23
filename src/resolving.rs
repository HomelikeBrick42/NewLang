use std::collections::VecDeque;

use crate::{ast, inferring_tree as it, interning::InternedStr, lexer::SourceLocation};
use derive_more::Display;
use enum_map::enum_map;
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap, new_key_type};

new_key_type! {
    pub struct ItemId;
}

#[derive(Debug, Clone)]
enum Item<'ast> {
    Unresolved {
        names: FxHashMap<InternedStr, Name>,
        item: &'ast ast::Item,
    },
    Resolving,
    Type(it::TypeId),
    Function(it::FunctionId),
}

#[derive(Debug, Clone)]
enum Name {
    Item(ItemId),
    Variable(it::VariableId),
}

enum DelayedResolution<'ast> {
    FunctionBody {
        function_id: it::FunctionId,
        names: FxHashMap<InternedStr, Name>,
        variables: SlotMap<it::VariableId, it::Variable>,
        parameter_variables: Box<[Option<it::VariableId>]>,
        expression: &'ast ast::Expression,
    },
}

pub fn resolve_program(items: &[ast::Item], errors: &mut Vec<ResolvingError>) -> it::Program {
    let mut program = it::Program {
        functions: SlotMap::with_key(),
        function_bodies: SecondaryMap::new(),
        types: SlotMap::with_key(),
        builtin_types: enum_map! {
            _ => None,
        },
    };

    let mut resolving_items = SlotMap::with_key();
    insert_item_names(
        items.iter(),
        &mut resolving_items,
        &mut FxHashMap::default(),
    );

    let mut delayed_resolutions = VecDeque::new();
    while {
        let mut resolved_something = false;

        // TODO: make it so that it doesnt have to search from the start every time
        while let Some((id, item)) = resolving_items
            .iter_mut()
            .find(|(_, item)| matches!(item, Item::Unresolved { .. }))
        {
            resolved_something = true;

            let Item::Unresolved { names, item } = std::mem::replace(item, Item::Resolving) else {
                unreachable!()
            };

            match resolve_item(
                item,
                &mut program,
                &mut resolving_items,
                &mut delayed_resolutions,
                names,
            ) {
                Ok(item) => resolving_items[id] = item,
                Err(error) => errors.push(error),
            }
        }

        while let Some(delayed_resolution) = delayed_resolutions.pop_front() {
            resolved_something = true;

            match delayed_resolution {
                DelayedResolution::FunctionBody {
                    function_id,
                    mut names,
                    mut variables,
                    parameter_variables,
                    expression,
                } => {
                    let expression = match resolve_expression(
                        expression,
                        &mut program,
                        &mut resolving_items,
                        &mut delayed_resolutions,
                        &mut names,
                        &mut variables,
                    ) {
                        Ok(expression) => Box::new(expression),
                        Err(error) => {
                            errors.push(error);
                            continue;
                        }
                    };
                    program.function_bodies.insert(
                        function_id,
                        it::FunctionBody::Expression {
                            variables,
                            parameter_variables,
                            expression,
                        },
                    );
                }
            }
        }

        resolved_something
    } {}

    program
}

fn insert_item_names<'ast>(
    items: impl Iterator<Item = &'ast ast::Item>,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    names: &mut FxHashMap<InternedStr, Name>,
) {
    let ids = items
        .map(|item| {
            let name = match item.kind {
                ast::ItemKind::Type { name, .. } | ast::ItemKind::Function { name, .. } => name,
            };
            let id = resolving_items.insert(Item::Unresolved {
                names: FxHashMap::default(),
                item,
            });
            names.insert(name, Name::Item(id));
            id
        })
        .collect::<Vec<_>>();
    for id in ids {
        let Item::Unresolved {
            names: ref mut item_names,
            ..
        } = resolving_items[id]
        else {
            unreachable!()
        };
        *item_names = names.clone();
    }
}

fn resolve_item<'ast>(
    item: &'ast ast::Item,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    mut names: FxHashMap<InternedStr, Name>,
) -> Result<Item<'ast>, ResolvingError> {
    Ok(match item.kind {
        ast::ItemKind::Type { name: _, ref typ } => Item::Type(resolve_type(
            typ,
            program,
            resolving_items,
            delayed_resolutions,
            &names,
        )?),

        ast::ItemKind::Function {
            name,
            ref parameters,
            ref return_type,
            ref body,
        } => {
            names.retain(|_, name| match *name {
                Name::Item { .. } => true,
                Name::Variable { .. } => false,
            });

            let mut variables = SlotMap::with_key();
            let mut parameter_variables =
                std::iter::repeat_n(None, parameters.len()).collect::<Box<[_]>>();
            let parameters = parameters
                .iter()
                .enumerate()
                .map(|(i, &ast::Parameter { location, ref kind })| {
                    Ok(match *kind {
                        ast::ParameterKind::Value { name, ref typ } => {
                            let typ = resolve_type(
                                typ,
                                program,
                                resolving_items,
                                delayed_resolutions,
                                &names,
                            )?;

                            let variable = variables.insert(it::Variable {
                                location,
                                name: Some(name),
                                typ,
                            });
                            parameter_variables[i] = Some(variable);
                            names.insert(name, Name::Variable(variable));

                            it::Parameter {
                                location,
                                name: Some(name),
                                kind: it::ParameterKind::Value { typ },
                            }
                        }
                    })
                })
                .collect::<Result<Box<[_]>, ResolvingError>>()?;
            let return_type = resolve_type(
                return_type,
                program,
                resolving_items,
                delayed_resolutions,
                &names,
            )?;

            let function_id = program.functions.insert_with_key(|id| {
                let function_type = program.types.insert(it::Type {
                    location: item.location,
                    kind: it::TypeKind::FunctionItem {
                        function: id,
                        parameters: parameters
                            .iter()
                            .map(|parameter| match parameter.kind {
                                it::ParameterKind::Value { typ } => {
                                    it::ParameterType::Value { typ }
                                }
                            })
                            .collect(),
                        return_type,
                    },
                });
                it::Function {
                    location: item.location,
                    name: Some(name),
                    parameters,
                    return_type,
                    function_type,
                }
            });

            match *body {
                ast::FunctionBody::Expression(ref expression) => {
                    delayed_resolutions.push_back(DelayedResolution::FunctionBody {
                        function_id,
                        names,
                        variables,
                        parameter_variables,
                        expression,
                    });
                }

                ast::FunctionBody::Builtin(builtin_function_body) => {
                    program.function_bodies.insert(
                        function_id,
                        it::FunctionBody::Builtin(builtin_function_body),
                    );
                }
            }

            Item::Function(function_id)
        }
    })
}

fn resolve_statement<'ast>(
    statement: &'ast ast::Statement,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &mut FxHashMap<InternedStr, Name>,
    variables: &mut SlotMap<it::VariableId, it::Variable>,
) -> Result<Option<it::Statement>, ResolvingError> {
    Ok(match statement.kind {
        ast::StatementKind::Item(_) => None,

        ast::StatementKind::Expression(ref expression) => Some(it::Statement {
            location: statement.location,
            kind: it::StatementKind::Expression(Box::new(resolve_expression(
                expression,
                program,
                resolving_items,
                delayed_resolutions,
                names,
                variables,
            )?)),
        }),

        ast::StatementKind::Assignment {
            ref pattern,
            ref value,
        } => Some(it::Statement {
            location: statement.location,
            kind: it::StatementKind::Assignment {
                pattern: Box::new(resolve_pattern(
                    pattern,
                    program,
                    resolving_items,
                    delayed_resolutions,
                    names,
                    variables,
                )?),
                value: Box::new(resolve_expression(
                    value,
                    program,
                    resolving_items,
                    delayed_resolutions,
                    names,
                    variables,
                )?),
            },
        }),
    })
}

fn resolve_expression<'ast>(
    expression: &'ast ast::Expression,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &mut FxHashMap<InternedStr, Name>,
    variables: &mut SlotMap<it::VariableId, it::Variable>,
) -> Result<it::Expression, ResolvingError> {
    Ok(match expression.kind {
        ast::ExpressionKind::Place(ref place) => {
            let place = Box::new(resolve_place(
                place,
                program,
                resolving_items,
                delayed_resolutions,
                names,
                variables,
            )?);
            it::Expression {
                location: expression.location,
                typ: place.typ,
                kind: it::ExpressionKind::Place(place),
            }
        }

        ast::ExpressionKind::Integer(value) => it::Expression {
            location: expression.location,
            typ: program.types.insert(it::Type {
                location: expression.location,
                kind: it::TypeKind::Infer(it::InferTypeKind::Anything),
            }),
            kind: it::ExpressionKind::Integer(value),
        },

        ast::ExpressionKind::Block {
            end_location,
            ref statements,
            ref last_expression,
        } => {
            let mut names = names.clone();
            insert_item_names(
                statements
                    .iter()
                    .filter_map(|statement| match statement.kind {
                        ast::StatementKind::Item(ref item) => Some(&**item),
                        _ => None,
                    }),
                resolving_items,
                &mut names,
            );

            let statements = statements
                .iter()
                .filter_map(|statement| {
                    resolve_statement(
                        statement,
                        program,
                        resolving_items,
                        delayed_resolutions,
                        &mut names,
                        variables,
                    )
                    .transpose()
                })
                .collect::<Result<_, ResolvingError>>()?;
            let last_expression = Box::new(resolve_expression(
                last_expression,
                program,
                resolving_items,
                delayed_resolutions,
                &mut names,
                variables,
            )?);

            it::Expression {
                location: expression.location,
                typ: last_expression.typ,
                kind: it::ExpressionKind::Block {
                    end_location,
                    statements,
                    last_expression,
                },
            }
        }

        ast::ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => it::Expression {
            location: expression.location,
            typ: program.types.insert(it::Type {
                location: expression.location,
                kind: it::TypeKind::Infer(it::InferTypeKind::Anything),
            }),
            kind: it::ExpressionKind::Call {
                operand: Box::new(resolve_expression(
                    operand,
                    program,
                    resolving_items,
                    delayed_resolutions,
                    names,
                    variables,
                )?),
                arguments: arguments
                    .iter()
                    .map(|argument| {
                        Ok(match argument {
                            ast::Argument::Value { expression } => it::Argument::Value {
                                expression: resolve_expression(
                                    expression,
                                    program,
                                    resolving_items,
                                    delayed_resolutions,
                                    names,
                                    variables,
                                )?,
                            },
                        })
                    })
                    .collect::<Result<_, ResolvingError>>()?,
            },
        },
    })
}

fn resolve_pattern<'ast>(
    pattern: &'ast ast::Pattern,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &mut FxHashMap<InternedStr, Name>,
    variables: &mut SlotMap<it::VariableId, it::Variable>,
) -> Result<it::Pattern, ResolvingError> {
    Ok(match pattern.kind {
        ast::PatternKind::Place(ref place) => {
            let place = Box::new(resolve_place(
                place,
                program,
                resolving_items,
                delayed_resolutions,
                names,
                variables,
            )?);
            it::Pattern {
                location: pattern.location,
                typ: place.typ,
                kind: it::PatternKind::Place(place),
            }
        }

        ast::PatternKind::Integer(value) => it::Pattern {
            location: pattern.location,
            typ: program.types.insert(it::Type {
                location: pattern.location,
                kind: it::TypeKind::Infer(it::InferTypeKind::Anything),
            }),
            kind: it::PatternKind::Integer(value),
        },
    })
}

fn resolve_place<'ast>(
    place: &'ast ast::Place,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &mut FxHashMap<InternedStr, Name>,
    variables: &mut SlotMap<it::VariableId, it::Variable>,
) -> Result<it::Place, ResolvingError> {
    Ok(match place.kind {
        ast::PlaceKind::Name(name) => match resolve_name(
            place.location,
            name,
            program,
            resolving_items,
            delayed_resolutions,
            names,
        )? {
            Name::Item(id) => match resolving_items[id] {
                Item::Unresolved { .. } | Item::Resolving => unreachable!(),

                Item::Type(id) => {
                    return Err(ResolvingError {
                        location: place.location,
                        kind: ResolvingErrorKind::ExpectedValueButGotType(
                            program.types[id].location,
                        ),
                    });
                }

                Item::Function(id) => it::Place {
                    location: place.location,
                    typ: program.functions[id].function_type,
                    kind: it::PlaceKind::Function(id),
                },
            },

            Name::Variable(id) => it::Place {
                location: place.location,
                typ: variables[id].typ,
                kind: it::PlaceKind::Variable(id),
            },
        },

        ast::PlaceKind::Let { name, ref typ } => {
            let typ = resolve_type(typ, program, resolving_items, delayed_resolutions, names)?;
            let id = variables.insert(it::Variable {
                location: place.location,
                name: Some(name),
                typ,
            });
            names.insert(name, Name::Variable(id));
            it::Place {
                location: place.location,
                typ,
                kind: it::PlaceKind::Let(id),
            }
        }
    })
}

fn resolve_type<'ast>(
    typ: &'ast ast::Type,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &FxHashMap<InternedStr, Name>,
) -> Result<it::TypeId, ResolvingError> {
    Ok(match typ.kind {
        ast::TypeKind::Infer => program.types.insert(it::Type {
            location: typ.location,
            kind: it::TypeKind::Infer(it::InferTypeKind::Anything),
        }),

        ast::TypeKind::Name(name) => match resolve_name(
            typ.location,
            name,
            program,
            resolving_items,
            delayed_resolutions,
            names,
        )? {
            Name::Item(id) => match resolving_items[id] {
                Item::Unresolved { .. } | Item::Resolving => unreachable!(),

                Item::Type(id) => id,

                Item::Function(_) => {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::ExpectedTypeButGotValue,
                    });
                }
            },

            Name::Variable(_) => {
                return Err(ResolvingError {
                    location: typ.location,
                    kind: ResolvingErrorKind::ExpectedTypeButGotValue,
                });
            }
        },

        ast::TypeKind::DeclareBuiltin(ref builtin_type) => {
            let (builtin_type, kind) = match *builtin_type {
                ast::BuiltinType::Unit => (it::BuiltinType::Unit, it::TypeKind::Unit),
                ast::BuiltinType::Runtime => (it::BuiltinType::Runtime, it::TypeKind::Runtime),
                ast::BuiltinType::I64 => (it::BuiltinType::I64, it::TypeKind::I64),
            };
            match program.builtin_types[builtin_type] {
                Some(_) => panic!("{}: Cannot redeclare builtin type {kind:?}", typ.location),
                ref mut builtin_type => {
                    let id = program.types.insert(it::Type {
                        location: typ.location,
                        kind,
                    });
                    *builtin_type = Some(id);
                    id
                }
            }
        }

        ast::TypeKind::Builtin(ref builtin_type) => {
            match *builtin_type {
                ast::BuiltinType::Unit => program.builtin_types[it::BuiltinType::Unit]
                    .expect("Unit builtin type not defined"),
                ast::BuiltinType::Runtime => program.builtin_types[it::BuiltinType::Runtime]
                    .expect("Runtime builtin type not defined"),
                ast::BuiltinType::I64 => program.builtin_types[it::BuiltinType::I64]
                    .expect("I64 builtin type not defined"),
            }
        }
    })
}

fn resolve_name<'ast>(
    location: SourceLocation,
    name: InternedStr,
    program: &mut it::Program,
    resolving_items: &mut SlotMap<ItemId, Item<'ast>>,
    delayed_resolutions: &mut VecDeque<DelayedResolution<'ast>>,
    names: &FxHashMap<InternedStr, Name>,
) -> Result<Name, ResolvingError> {
    Ok(match names.get(&name) {
        Some(name) => match *name {
            Name::Item(id) => {
                loop {
                    break match resolving_items[id] {
                        Item::Unresolved { .. } => {
                            let Item::Unresolved { names, item } =
                                std::mem::replace(&mut resolving_items[id], Item::Resolving)
                            else {
                                unreachable!()
                            };
                            resolving_items[id] = resolve_item(
                                item,
                                program,
                                resolving_items,
                                delayed_resolutions,
                                names,
                            )?;
                            continue;
                        }

                        Item::Resolving => {
                            return Err(ResolvingError {
                                location,
                                kind: ResolvingErrorKind::CyclicDependency,
                            });
                        }

                        _ => {}
                    };
                }
                Name::Item(id)
            }

            Name::Variable(id) => Name::Variable(id),
        },

        None => {
            return Err(ResolvingError {
                location,
                kind: ResolvingErrorKind::UnknownName(name),
            });
        }
    })
}

#[derive(Debug, Display)]
#[display("{location}: {kind}")]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug, Display)]
pub enum ResolvingErrorKind {
    #[display("Cyclic dependency detected")]
    CyclicDependency,
    #[display("Unknown name '{_0}'")]
    UnknownName(InternedStr),
    #[display("Expected value but got type declared at {_0}")]
    ExpectedValueButGotType(SourceLocation),
    #[display("Expected type but got value")]
    ExpectedTypeButGotValue,
}
