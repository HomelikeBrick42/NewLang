use std::{
    collections::{VecDeque, hash_map::Entry},
    num::NonZeroUsize,
};

use crate::{
    ast,
    idvec::{Id, IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
    typed_tree as tt,
};
use derive_more::Display;
use rustc_hash::FxHashMap;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{location}: {kind}")]
pub struct TypingError {
    pub location: SourceLocation,
    pub kind: TypingErrorKind,
}

#[derive(Debug, Display)]
pub enum TypingErrorKind {
    #[display("Cyclic dependency detected")]
    CyclicDependency,
    #[display("'{name}' was already declared at {old_location}")]
    NameRedeclaration {
        name: InternedStr,
        old_location: SourceLocation,
    },
    #[display("'{name}' member was already declared at {old_location}")]
    MemberRedeclaration {
        name: InternedStr,
        old_location: SourceLocation,
    },
    #[display("Type aliases must have an assigned type")]
    TypeAliasNoAssignedType,
    #[display("Functions must have a body")]
    FunctionMissingBody,
    #[display("Unknown name '{name}'")]
    UnknownName { name: InternedStr },
    #[display("Unknown path '{name}'")]
    UnknownPath { name: InternedStr },
    #[display("Expected type")]
    ExpectedType,
    #[display("Expected value")]
    ExpectedValue,
}

new_id_type!(pub struct BindingId);

#[derive(Debug)]
pub struct Binding<'ast> {
    pub location: SourceLocation,
    pub kind: BindingKind<'ast>,
}

#[derive(Debug)]
pub enum BindingKind<'ast> {
    UnresolvedItem {
        item: &'ast ast::Item,
        module: BindingId,
    },
    Resolving,
    Module {
        parent: Option<BindingId>,
        name: Option<InternedStr>,
        names: FxHashMap<InternedStr, BindingId>,
    },
    Function(tt::FunctionId),
    Type(tt::TypeId),
}

#[derive(Debug)]
struct FunctionBodyToCheck<'ast> {
    id: tt::FunctionId,
    module: BindingId,
    body: &'ast ast::Expression,
}

#[derive(Debug)]
pub struct TypingResult<'ast> {
    pub bindings: IdVec<BindingId, Binding<'ast>>,
    pub global_module: BindingId,

    pub types: IdVec<tt::TypeId, tt::Type>,
    pub runtime_type: Option<tt::TypeId>,
    pub i64_type: Option<tt::TypeId>,
    pub unit_type: Option<tt::TypeId>,
    pub bool_type: Option<tt::TypeId>,

    pub function_signatures: IdVec<tt::FunctionId, tt::FunctionSignature>,
    pub function_bodies: FxHashMap<tt::FunctionId, tt::FunctionBody>,
    function_bodies_to_check: VecDeque<FunctionBodyToCheck<'ast>>,
}

pub fn type_items(
    filepath: InternedStr,
    items: &[ast::Item],
) -> Result<TypingResult<'static>, TypingError> {
    let location = SourceLocation {
        filepath,
        position: 0,
        line: NonZeroUsize::MIN,
        column: NonZeroUsize::MIN,
    };

    let mut result = {
        let mut bindings = IdVec::new();
        let global_module = bindings.push(Binding {
            location,
            kind: BindingKind::Resolving,
        });
        TypingResult {
            global_module,
            bindings,

            types: IdVec::new(),
            runtime_type: None,
            i64_type: None,
            unit_type: None,
            bool_type: None,

            function_signatures: IdVec::new(),
            function_bodies: FxHashMap::default(),
            function_bodies_to_check: VecDeque::new(),
        }
    };
    result.bindings[result.global_module].kind = BindingKind::Module {
        parent: None,
        name: None,
        names: forward_declare_items(items, &mut result.bindings, result.global_module)?,
    };

    {
        let forced_core_names = ["I64".into(), "Unit".into()];

        let BindingKind::Module {
            parent: _,
            name: _,
            ref names,
        } = result.bindings[result.global_module].kind
        else {
            unreachable!()
        };
        let names = names
            .iter()
            .map(|(&name, &binding)| (name, Name::Binding(binding)))
            .collect();
        for name in forced_core_names {
            match type_path(
                &ast::Path {
                    location,
                    kind: ast::PathKind::PathAccess {
                        operand: Box::new(ast::Path {
                            location,
                            kind: ast::PathKind::Name {
                                name: "core".into(),
                            },
                        }),
                        name,
                    },
                },
                &mut result,
                &names,
            )? {
                Name::Binding(id) => resolve_item(&mut result, id)?,
                Name::Variable(_) => unreachable!(),
            }
        }
    }

    let mut last_binding_index = 0;

    let mut still_more = true;
    while still_more {
        still_more = false;

        while last_binding_index < result.bindings.len() {
            still_more = true;

            let id = BindingId::from_index(last_binding_index.try_into().unwrap());
            resolve_item(&mut result, id)?;
            last_binding_index += 1;
        }

        while let Some(FunctionBodyToCheck { id, module, body }) =
            result.function_bodies_to_check.pop_front()
        {
            still_more = true;

            let function = &result.function_signatures[id];
            let BindingKind::Module {
                parent: _,
                name: _,
                ref names,
            } = result.bindings[module].kind
            else {
                unreachable!()
            };
            let mut names = names
                .iter()
                .map(|(&name, &binding)| (name, Name::Binding(binding)))
                .collect::<FxHashMap<InternedStr, Name>>();

            let mut variables = IdVec::new();
            for &tt::FunctionParameter { location, ref kind } in &function.parameters {
                match *kind {
                    tt::FunctionParameterKind::Value { name, typ } => {
                        let variable = variables.push(tt::Variable {
                            name: Some(name),
                            typ,
                        });
                        names.insert(name, Name::Variable(variable));
                    }

                    tt::FunctionParameterKind::Type { name, typ } => {
                        let binding = result.bindings.push(Binding {
                            location,
                            kind: BindingKind::Type(typ),
                        });
                        names.insert(name, Name::Binding(binding));
                    }
                }
            }

            let expression = Box::new(type_expression(
                body,
                &mut result,
                &mut variables,
                &names,
                module,
            )?);
            result.function_bodies.insert(
                id,
                tt::FunctionBody::Expression {
                    variables,
                    expression,
                },
            );
        }
    }

    Ok(TypingResult {
        bindings: IdVec::from_vec(
            result
                .bindings
                .into_vec()
                .into_iter()
                .map(|Binding { location, kind }|
                    Binding {
                        location,
                        kind: match kind {
                            BindingKind::UnresolvedItem { item, module: _ } => panic!("compiler bug, there was an unresolved item at {} at the end of type checking", item.location),
                            BindingKind::Resolving => panic!("compiler bug, there should be no resolving bindings at the end of type checking"),
                            BindingKind::Module { parent, name, names } => BindingKind::Module { parent, name, names },
                            BindingKind::Function(id) => BindingKind::Function(id),
                            BindingKind::Type(id) => BindingKind::Type(id),
                        },
                    }
                )
                .collect()
        ),
        function_bodies_to_check: result.function_bodies_to_check
            .into_iter()
            .map(|body| panic!("compiler bug, unchecked function body at {}", body.body.location))
            .collect(),
        ..result
    })
}

fn forward_declare_items<'ast>(
    items: impl IntoIterator<Item = &'ast ast::Item>,
    bindings: &mut IdVec<BindingId, Binding<'ast>>,
    module: BindingId,
) -> Result<FxHashMap<InternedStr, BindingId>, TypingError> {
    let mut names = FxHashMap::default();
    for item in items {
        let (ast::ItemKind::Module { name, .. }
        | ast::ItemKind::Fn { name, .. }
        | ast::ItemKind::Struct { name, .. }
        | ast::ItemKind::Enum { name, .. }
        | ast::ItemKind::Type { name, .. }) = item.kind;

        let id = bindings.push(Binding {
            location: item.location,
            kind: BindingKind::UnresolvedItem { item, module },
        });

        match names.entry(name) {
            Entry::Vacant(e) => {
                e.insert(id);
            }

            Entry::Occupied(e) => {
                return Err(TypingError {
                    location: item.location,
                    kind: TypingErrorKind::NameRedeclaration {
                        name,
                        old_location: bindings[*e.get()].location,
                    },
                });
            }
        }
    }
    Ok(names)
}

fn resolve_item<'ast>(result: &mut TypingResult<'ast>, id: BindingId) -> Result<(), TypingError> {
    if let ref mut binding_kind @ BindingKind::UnresolvedItem { item, module } =
        result.bindings[id].kind
    {
        *binding_kind = BindingKind::Resolving;
        let binding_kind = type_item(item, id, module, result)?;
        result.bindings[id].kind = binding_kind;
    }
    Ok(())
}

fn type_item<'ast>(
    &ast::Item {
        location,
        builtin,
        ref kind,
    }: &'ast ast::Item,
    binding: BindingId,
    module: BindingId,
    result: &mut TypingResult<'ast>,
) -> Result<BindingKind<'ast>, TypingError> {
    let BindingKind::Module {
        parent: _,
        name: _,
        ref names,
    } = result.bindings[module].kind
    else {
        unreachable!()
    };
    let names = names
        .iter()
        .map(|(&name, &binding)| (name, Name::Binding(binding)))
        .collect::<FxHashMap<InternedStr, Name>>();

    Ok(match *kind {
        ast::ItemKind::Module { name, ref items } => {
            assert!(!builtin, "modules cannot be builtin");
            BindingKind::Module {
                parent: Some(module),
                name: Some(name),
                names: forward_declare_items(items, &mut result.bindings, binding)?,
            }
        }

        ast::ItemKind::Fn {
            name,
            ref parameters,
            ref return_type,
            ref body,
        } => {
            let parameters = parameters
                .iter()
                .map(|&ast::Parameter { location, ref kind }| {
                    Ok(tt::FunctionParameter {
                        location,
                        kind: match *kind {
                            ast::ParameterKind::Value { name, ref typ } => {
                                tt::FunctionParameterKind::Value {
                                    name,
                                    typ: type_type(typ, result, &names)?,
                                }
                            }

                            ast::ParameterKind::Type { name } => tt::FunctionParameterKind::Type {
                                name,
                                typ: result.types.push(tt::Type {
                                    location,
                                    name: Some(name),
                                    kind: tt::TypeKind::Generic,
                                }),
                            },

                            ast::ParameterKind::Lifetime { name: _ } => {
                                todo!("Unimplemented lifetimes at {location}")
                            }
                        },
                    })
                })
                .collect::<Result<_, TypingError>>()?;
            let return_type = type_type(return_type, result, &names)?;

            let id = result
                .function_signatures
                .push_with(|id| tt::FunctionSignature {
                    name: Some(name),
                    parameters,
                    return_type,
                    typ: result.types.push(tt::Type {
                        location,
                        name: Some(name),
                        kind: tt::TypeKind::FunctionItem(id),
                    }),
                });

            if builtin {
                assert!(body.is_none(), "builtin function must have no body");

                match name.as_str() {
                    "print_i64" => {
                        result.function_bodies.insert(
                            id,
                            tt::FunctionBody::Builtin(tt::BuiltinFunctionBody::PrintI64),
                        );
                    }

                    _ => panic!("unknown builtin function {name} at {location}"),
                }
            } else {
                let Some(body) = body else {
                    return Err(TypingError {
                        location,
                        kind: TypingErrorKind::FunctionMissingBody,
                    });
                };
                result
                    .function_bodies_to_check
                    .push_back(FunctionBodyToCheck { id, module, body });
            }

            BindingKind::Function(id)
        }

        ast::ItemKind::Struct { name, ref members } => {
            let id = result.types.push(tt::Type {
                location,
                name: Some(name),
                kind: tt::TypeKind::Resolving,
            });
            result.bindings[binding].kind = BindingKind::Type(id);

            if builtin {
                match name.as_str() {
                    "Unit" => {
                        assert!(
                            result.unit_type.is_none(),
                            "duplicate Unit lang item at {location}"
                        );
                        result.unit_type = Some(id);
                    }

                    _ => panic!("unknown builtin struct {name} at {location}"),
                }
            }

            let members = members
                .iter()
                .map(|&ast::Member { name, ref typ }| {
                    Ok(tt::Member {
                        name,
                        typ: type_type(typ, result, &names)?,
                    })
                })
                .collect::<Result<_, TypingError>>()?;
            result.types[id].kind = tt::TypeKind::Struct { members };
            BindingKind::Type(id)
        }

        ast::ItemKind::Enum { name, ref members } => {
            let id = result.types.push(tt::Type {
                location,
                name: Some(name),
                kind: tt::TypeKind::Resolving,
            });
            result.bindings[binding].kind = BindingKind::Type(id);

            if builtin {
                match name.as_str() {
                    "Bool" => {
                        assert!(
                            result.bool_type.is_none(),
                            "duplicate Bool lang item at {location}"
                        );
                        result.bool_type = Some(id);
                    }

                    _ => panic!("unknown builtin enum {name} at {location}"),
                }
            }

            let members = members
                .iter()
                .map(|&ast::Member { name, ref typ }| {
                    Ok(tt::Member {
                        name,
                        typ: type_type(typ, result, &names)?,
                    })
                })
                .collect::<Result<_, TypingError>>()?;
            result.types[id].kind = tt::TypeKind::Enum { members };
            BindingKind::Type(id)
        }

        ast::ItemKind::Type { name, ref typ } => BindingKind::Type(if builtin {
            assert!(
                typ.is_none(),
                "the builtin type alias at {location} must not have an assigned type"
            );

            match name.as_str() {
                "Runtime" => {
                    assert!(
                        result.runtime_type.is_none(),
                        "duplicate Runtime lang item at {location}"
                    );
                    let id = result.types.push(tt::Type {
                        location,
                        name: Some(name),
                        kind: tt::TypeKind::Runtime,
                    });
                    result.runtime_type = Some(id);
                    id
                }

                "I64" => {
                    assert!(
                        result.i64_type.is_none(),
                        "duplicate I64 lang item at {location}"
                    );
                    let id = result.types.push(tt::Type {
                        location,
                        name: Some(name),
                        kind: tt::TypeKind::I64,
                    });
                    result.i64_type = Some(id);
                    id
                }

                _ => panic!("unknown builtin type alias {name} at {location}"),
            }
        } else {
            let Some(typ) = typ else {
                return Err(TypingError {
                    location,
                    kind: TypingErrorKind::TypeAliasNoAssignedType,
                });
            };
            type_type(typ, result, &names)?
        }),
    })
}

fn type_type<'ast>(
    &ast::Type { location, ref kind }: &'ast ast::Type,
    result: &mut TypingResult<'ast>,
    names: &FxHashMap<InternedStr, Name>,
) -> Result<tt::TypeId, TypingError> {
    match *kind {
        ast::TypeKind::Path(ref path) => match type_path(path, result, names)? {
            Name::Binding(id) => loop {
                break match result.bindings[id].kind {
                    BindingKind::UnresolvedItem { .. } => {
                        resolve_item(result, id)?;
                        continue;
                    }

                    BindingKind::Type(id) => Ok(id),

                    BindingKind::Resolving => Err(TypingError {
                        location,
                        kind: TypingErrorKind::CyclicDependency,
                    }),

                    BindingKind::Module { .. } | BindingKind::Function { .. } => Err(TypingError {
                        location,
                        kind: TypingErrorKind::ExpectedType,
                    }),
                };
            },

            Name::Variable { .. } => Err(TypingError {
                location,
                kind: TypingErrorKind::ExpectedType,
            }),
        },

        ast::TypeKind::Unit => Ok(result
            .unit_type
            .expect("the builtin unit type should have been declared")),
    }
}

fn type_path<'ast>(
    &ast::Path { location, ref kind }: &ast::Path,
    result: &mut TypingResult<'ast>,
    names: &FxHashMap<InternedStr, Name>,
) -> Result<Name, TypingError> {
    match *kind {
        ast::PathKind::Name { name } => match names.get(&name) {
            Some(&name) => Ok(name),
            None => Err(TypingError {
                location,
                kind: TypingErrorKind::UnknownName { name },
            }),
        },

        ast::PathKind::PathAccess { ref operand, name } => {
            let operand = type_path(operand, result, names)?;
            match operand {
                Name::Binding(id) => loop {
                    break match result.bindings[id].kind {
                        BindingKind::UnresolvedItem { .. } => {
                            resolve_item(result, id)?;
                            continue;
                        }

                        BindingKind::Resolving => Err(TypingError {
                            location,
                            kind: TypingErrorKind::CyclicDependency,
                        }),

                        BindingKind::Module {
                            parent: _,
                            name: _,
                            ref names,
                        } => match names.get(&name) {
                            Some(&binding) => Ok(Name::Binding(binding)),
                            None => Err(TypingError {
                                location,
                                kind: TypingErrorKind::UnknownPath { name },
                            }),
                        },

                        BindingKind::Function { .. } | BindingKind::Type { .. } => {
                            Err(TypingError {
                                location,
                                kind: TypingErrorKind::UnknownPath { name },
                            })
                        }
                    };
                },

                Name::Variable { .. } => Err(TypingError {
                    location,
                    kind: TypingErrorKind::UnknownPath { name },
                }),
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Name {
    Binding(BindingId),
    Variable(tt::VariableId),
}

fn type_expression<'ast>(
    &ast::Expression { location, ref kind }: &'ast ast::Expression,
    result: &mut TypingResult<'ast>,
    variables: &mut IdVec<tt::VariableId, tt::Variable>,
    names: &FxHashMap<InternedStr, Name>,
    module: BindingId,
) -> Result<tt::Expression, TypingError> {
    Ok(match *kind {
        ast::ExpressionKind::Path(ref path) => match type_path(path, result, names)? {
            Name::Binding(id) => loop {
                break match result.bindings[id].kind {
                    BindingKind::UnresolvedItem { .. } => {
                        resolve_item(result, id)?;
                        continue;
                    }

                    BindingKind::Resolving => {
                        return Err(TypingError {
                            location,
                            kind: TypingErrorKind::CyclicDependency,
                        });
                    }

                    BindingKind::Function(id) => tt::Expression {
                        location,
                        typ: result.function_signatures[id].typ,
                        kind: tt::ExpressionKind::Function(id),
                    },

                    BindingKind::Type { .. } | BindingKind::Module { .. } => {
                        return Err(TypingError {
                            location,
                            kind: TypingErrorKind::ExpectedValue,
                        });
                    }
                };
            },

            Name::Variable(id) => tt::Expression {
                location,
                typ: variables[id].typ,
                kind: tt::ExpressionKind::Variable(id),
            },
        },

        ast::ExpressionKind::Integer(value) => tt::Expression {
            location,
            typ: result
                .i64_type
                .expect("the builtin i64 type should have already been declared"),
            kind: tt::ExpressionKind::Integer(value),
        },

        ast::ExpressionKind::Block {
            ref statements,
            ref last_expression,
        } => {
            let mut names = names.clone();
            let item_names = forward_declare_items(
                statements
                    .iter()
                    .filter_map(|statement| match statement.kind {
                        ast::StatementKind::Item(ref item) => Some(&**item),
                        _ => None,
                    }),
                &mut result.bindings,
                module,
            )?;
            names.extend(
                item_names
                    .into_iter()
                    .map(|(name, binding)| (name, Name::Binding(binding))),
            );
            let statements = statements
                .iter()
                .filter_map(|statement| {
                    type_statement(statement, result, variables, &mut names, module).transpose()
                })
                .collect::<Result<_, TypingError>>()?;
            let last_expression = Box::new(type_expression(
                last_expression,
                result,
                variables,
                &names,
                module,
            )?);
            tt::Expression {
                location,
                typ: last_expression.typ,
                kind: tt::ExpressionKind::Block {
                    statements,
                    last_expression,
                },
            }
        }

        ast::ExpressionKind::Constructor {
            ref typ,
            ref members,
        } => tt::Expression {
            location,
            typ: type_type(typ, result, names)?,
            kind: tt::ExpressionKind::Constructor {
                members: members
                    .iter()
                    .map(|&ast::ConstructorMember { name, ref value }| {
                        Ok(tt::ConstructorMember {
                            name,
                            value: type_expression(value, result, variables, names, module)?,
                        })
                    })
                    .collect::<Result<_, TypingError>>()?,
            },
        },

        ast::ExpressionKind::Unary {
            ref operator,
            ref operand,
        } => tt::Expression {
            location,
            typ: result.types.push(tt::Type {
                location,
                name: None,
                kind: tt::TypeKind::Infer,
            }),
            kind: tt::ExpressionKind::Unary {
                operator: match *operator {
                    ast::UnaryOperator::Plus => tt::UnaryOperator::Plus,
                    ast::UnaryOperator::Negate => tt::UnaryOperator::Negate,
                },
                operand: Box::new(type_expression(operand, result, variables, names, module)?),
            },
        },

        ast::ExpressionKind::Binary {
            ref left,
            ref operator,
            ref right,
        } => tt::Expression {
            location,
            typ: result.types.push(tt::Type {
                location,
                name: None,
                kind: tt::TypeKind::Infer,
            }),
            kind: tt::ExpressionKind::Binary {
                left: Box::new(type_expression(left, result, variables, names, module)?),
                operator: match *operator {
                    ast::BinaryOperator::Add => tt::BinaryOperator::Add,
                    ast::BinaryOperator::Subtract => tt::BinaryOperator::Subtract,
                    ast::BinaryOperator::Multiply => tt::BinaryOperator::Multiply,
                    ast::BinaryOperator::Divide => tt::BinaryOperator::Divide,
                },
                right: Box::new(type_expression(right, result, variables, names, module)?),
            },
        },

        ast::ExpressionKind::Call {
            ref operand,
            ref arguments,
        } => tt::Expression {
            location,
            typ: result.types.push(tt::Type {
                location,
                name: None,
                kind: tt::TypeKind::Infer,
            }),
            kind: tt::ExpressionKind::Call {
                operand: Box::new(type_expression(operand, result, variables, names, module)?),
                arguments: arguments
                    .iter()
                    .map(|argument| type_argument(argument, result, variables, names, module))
                    .collect::<Result<_, TypingError>>()?,
            },
        },

        ast::ExpressionKind::MemberAccess { ref operand, name } => tt::Expression {
            location,
            typ: result.types.push(tt::Type {
                location,
                name: None,
                kind: tt::TypeKind::Infer,
            }),
            kind: tt::ExpressionKind::MemberAccess {
                operand: Box::new(type_expression(operand, result, variables, names, module)?),
                name,
            },
        },
    })
}

fn type_argument<'ast>(
    &ast::Argument { location, ref kind }: &'ast ast::Argument,
    result: &mut TypingResult<'ast>,
    variables: &mut IdVec<tt::VariableId, tt::Variable>,
    names: &FxHashMap<InternedStr, Name>,
    module: BindingId,
) -> Result<tt::Argument, TypingError> {
    Ok(tt::Argument {
        location,
        kind: match *kind {
            ast::ArgumentKind::ValueOrType(ref expression) => match expression.kind {
                ast::ExpressionKind::Path(ref path) => match type_path(path, result, names)? {
                    Name::Binding(id) => loop {
                        break match result.bindings[id].kind {
                            BindingKind::UnresolvedItem { .. } => {
                                resolve_item(result, id)?;
                                continue;
                            }

                            BindingKind::Resolving => {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::CyclicDependency,
                                });
                            }

                            BindingKind::Function(id) => tt::ArgumentKind::Value(tt::Expression {
                                location: expression.location,
                                typ: result.function_signatures[id].typ,
                                kind: tt::ExpressionKind::Function(id),
                            }),

                            BindingKind::Type(id) => tt::ArgumentKind::Type(id),

                            BindingKind::Module { .. } => {
                                return Err(TypingError {
                                    location,
                                    kind: TypingErrorKind::ExpectedValue,
                                });
                            }
                        };
                    },

                    Name::Variable(id) => tt::ArgumentKind::Value(tt::Expression {
                        location: expression.location,
                        typ: variables[id].typ,
                        kind: tt::ExpressionKind::Variable(id),
                    }),
                },

                _ => tt::ArgumentKind::Value(type_expression(
                    expression, result, variables, names, module,
                )?),
            },

            ast::ArgumentKind::Lifetime { name: _ } => {
                todo!("Unimplemented lifetimes at {location}")
            }
        },
    })
}

fn type_statement<'ast>(
    &ast::Statement { location, ref kind }: &'ast ast::Statement,
    result: &mut TypingResult<'ast>,
    variables: &mut IdVec<tt::VariableId, tt::Variable>,
    names: &mut FxHashMap<InternedStr, Name>,
    module: BindingId,
) -> Result<Option<tt::Statement>, TypingError> {
    Ok(match *kind {
        ast::StatementKind::Item(_) => None,

        ast::StatementKind::Expression(ref expression) => Some(tt::Statement {
            location,
            kind: tt::StatementKind::Expression(Box::new(type_expression(
                expression, result, variables, names, module,
            )?)),
        }),

        ast::StatementKind::Assignment {
            ref pattern,
            ref value,
        } => Some(tt::Statement {
            location,
            kind: tt::StatementKind::Assignment {
                value: Box::new(type_expression(value, result, variables, names, module)?),
                pattern: Box::new(type_pattern(pattern, result, variables, names)?),
            },
        }),
    })
}

fn type_pattern<'ast>(
    &ast::Pattern { location, ref kind }: &'ast ast::Pattern,
    result: &mut TypingResult<'ast>,
    variables: &mut IdVec<tt::VariableId, tt::Variable>,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<tt::Pattern, TypingError> {
    Ok(match *kind {
        ast::PatternKind::Path(ref path) => match type_path(path, result, names)? {
            Name::Binding(id) => loop {
                break match result.bindings[id].kind {
                    BindingKind::UnresolvedItem { .. } => {
                        resolve_item(result, id)?;
                        continue;
                    }

                    BindingKind::Resolving => {
                        return Err(TypingError {
                            location,
                            kind: TypingErrorKind::CyclicDependency,
                        });
                    }

                    BindingKind::Function(id) => tt::Pattern {
                        location,
                        typ: result.function_signatures[id].typ,
                        kind: tt::PatternKind::Function(id),
                    },

                    BindingKind::Type { .. } | BindingKind::Module { .. } => {
                        return Err(TypingError {
                            location,
                            kind: TypingErrorKind::ExpectedValue,
                        });
                    }
                };
            },

            Name::Variable(id) => tt::Pattern {
                location,
                typ: variables[id].typ,
                kind: tt::PatternKind::Variable(id),
            },
        },

        ast::PatternKind::Integer(value) => tt::Pattern {
            location,
            typ: result
                .i64_type
                .expect("the builtin i64 type should have already been declared"),
            kind: tt::PatternKind::Integer(value),
        },

        ast::PatternKind::Destructor {
            ref typ,
            ref members,
        } => tt::Pattern {
            location,
            typ: type_type(typ, result, names)?,
            kind: tt::PatternKind::Destructor {
                members: members
                    .iter()
                    .map(|&ast::DestructorMember { name, ref pattern }| {
                        Ok(tt::DestructorMember {
                            name,
                            pattern: type_pattern(pattern, result, variables, names)?,
                        })
                    })
                    .collect::<Result<_, TypingError>>()?,
            },
        },

        ast::PatternKind::Let { name, ref typ } => {
            let typ = typ
                .as_ref()
                .map(|typ| type_type(typ, result, names))
                .transpose()?
                .unwrap_or_else(|| {
                    result.types.push(tt::Type {
                        location,
                        name: None,
                        kind: tt::TypeKind::Infer,
                    })
                });
            let variable = variables.push(tt::Variable {
                name: Some(name),
                typ,
            });
            names.insert(name, Name::Variable(variable));
            tt::Pattern {
                location,
                typ,
                kind: tt::PatternKind::Let(variable),
            }
        }
    })
}
