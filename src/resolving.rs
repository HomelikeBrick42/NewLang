use crate::{
    ast,
    interning::InternedStr,
    lexer::SourceLocation,
    resolved_tree::{self as rt, ResolvedProgram},
};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy)]
enum Name {
    TypeAlias(rt::TypeAliasId),
    Struct(rt::StructId),
    GenericType(rt::GenericTypeId),
    ParameterizedType(rt::ParameterizedTypeId),
    GenericDyn(rt::GenericDynId),
    ParameterizedDyn(rt::ParameterizedDynId),
    Function(rt::FunctionId),
    Variable(rt::VariableId),
}

pub fn resolve_file(
    items: &[ast::Item],
    program: &mut ResolvedProgram,
    errors: &mut Vec<ResolvingError>,
) -> Box<[rt::Item]> {
    let mut names = FxHashMap::default();

    items
        .iter()
        .filter_map(|item| match resolve_item(item, program, &mut names) {
            Ok(resolved_item) => Some(resolved_item),
            Err(error) => {
                errors.push(error);
                None
            }
        })
        .collect()
}

fn resolve_item(
    item: &ast::Item,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Item, ResolvingError> {
    Ok(rt::Item {
        location: item.location,
        kind: match item.kind {
            ast::ItemKind::TypeAlias {
                name,
                ref parameters,
                ref typ,
            } => {
                if let Some(parameters) = parameters {
                    let (parameters, typ) = {
                        let mut names = names.clone();
                        names.retain(|_, name| !matches!(name, Name::Variable(_)));

                        let parameters = parameters
                            .iter()
                            .map(|parameter| resolve_parameter(parameter, program, &mut names))
                            .collect::<Result<Box<[_]>, _>>()?;
                        let typ = resolve_type(typ, program, &mut names)?;
                        (parameters, typ)
                    };
                    let type_alias = program.type_aliases.insert(rt::TypeAlias {
                        location: item.location,
                        name,
                        typ,
                    });
                    let parameterized_type =
                        program.parameterized_types.insert(rt::ParameterizedType {
                            location: item.location,
                            name,
                            parameters,
                            underlying_type: rt::UnderlyingType::TypeAlias(type_alias),
                        });
                    names.insert(name, Name::ParameterizedType(parameterized_type));
                    rt::ItemKind::ParameterizedType(parameterized_type)
                } else {
                    let typ = resolve_type(typ, program, names)?;
                    let type_alias = program.type_aliases.insert(rt::TypeAlias {
                        location: item.location,
                        name,
                        typ,
                    });
                    names.insert(name, Name::TypeAlias(type_alias));
                    rt::ItemKind::TypeAlias(type_alias)
                }
            }

            ast::ItemKind::Struct {
                ref builtin_type,
                name,
                ref parameters,
                ref members,
            } => {
                if let Some(parameters) = parameters {
                    let (parameters, members) = {
                        let mut names = names.clone();
                        names.retain(|_, name| !matches!(name, Name::Variable(_)));

                        let parameters = parameters
                            .iter()
                            .map(|parameter| resolve_parameter(parameter, program, &mut names))
                            .collect::<Result<Box<[_]>, _>>()?;
                        let members = members
                            .iter()
                            .map(
                                |&ast::StructMember {
                                     location,
                                     name,
                                     ref typ,
                                 }| {
                                    Ok(rt::StructMember {
                                        location,
                                        name,
                                        typ: resolve_type(typ, program, &mut names)?,
                                    })
                                },
                            )
                            .collect::<Result<Box<[_]>, _>>()?;
                        (parameters, members)
                    };
                    let structt = program.structs.insert(rt::Struct {
                        location: item.location,
                        name,
                        members,
                    });
                    let parameterized_type =
                        program.parameterized_types.insert(rt::ParameterizedType {
                            location: item.location,
                            name,
                            parameters,
                            underlying_type: rt::UnderlyingType::Struct(structt),
                        });
                    names.insert(name, Name::ParameterizedType(parameterized_type));
                    rt::ItemKind::ParameterizedType(parameterized_type)
                } else {
                    let members = members
                        .iter()
                        .map(
                            |&ast::StructMember {
                                 location,
                                 name,
                                 ref typ,
                             }| {
                                Ok(rt::StructMember {
                                    location,
                                    name,
                                    typ: resolve_type(typ, program, names)?,
                                })
                            },
                        )
                        .collect::<Result<Box<[_]>, _>>()?;
                    let structt = program.structs.insert(rt::Struct {
                        location: item.location,
                        name,
                        members,
                    });

                    if let Some(builtin_type) = builtin_type {
                        match builtin_type {
                            ast::BuiltinStruct::Unit => {
                                assert!(program.unit_type.is_none());
                                program.unit_type = Some(structt);
                            }
                        }
                    }

                    names.insert(name, Name::Struct(structt));
                    rt::ItemKind::Struct(structt)
                }
            }

            ast::ItemKind::Function {
                is_unsafe,
                name,
                ref parameters,
                ref return_type,
                ref body,
            } => rt::ItemKind::Function(resolve_function(
                item.location,
                is_unsafe,
                Some(name),
                parameters,
                return_type,
                body,
                program,
                names,
            )?),
        },
    })
}

fn resolve_function(
    location: SourceLocation,
    is_unsafe: bool,
    name: Option<InternedStr>,
    parameters: &[ast::Parameter],
    return_type: &ast::Type,
    body: &ast::FunctionBody,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::FunctionId, ResolvingError> {
    let (parameters, return_type, body) = {
        let mut names = names.clone();
        names.retain(|_, name| !matches!(name, Name::Variable(_)));

        let parameters = parameters
            .iter()
            .map(|parameter| resolve_parameter(parameter, program, &mut names))
            .collect::<Result<Box<[_]>, _>>()?;
        let return_type = resolve_type(return_type, program, &mut names)?;

        let body = match *body {
            ast::FunctionBody::Expression(ref expression) => {
                let expression = Box::new(resolve_expression(expression, program, &mut names)?);
                rt::FunctionBody::Expression(expression)
            }

            ast::FunctionBody::Builtin(builtin) => rt::FunctionBody::Builtin(builtin),
        };

        (parameters, return_type, body)
    };
    let function = program.functions.insert(rt::Function {
        location,
        is_unsafe,
        name,
        parameters,
        return_type,
        body,
    });
    if let Some(name) = name {
        names.insert(name, Name::Function(function));
    }
    Ok(function)
}

fn resolve_parameter(
    parameter: &ast::Parameter,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Parameter, ResolvingError> {
    Ok(rt::Parameter {
        location: parameter.location,
        kind: match parameter.kind {
            ast::ParameterKind::Value { name, ref typ } => {
                let typ = resolve_type(typ, program, names)?;
                let variable = program.variables.insert(rt::Variable {
                    location: parameter.location,
                    name: Some(name),
                    typ,
                });
                names.insert(name, Name::Variable(variable));
                rt::ParameterKind::Value(variable)
            }

            ast::ParameterKind::Type {
                ref parameters,
                name,
            } => {
                let generic_type = program.generic_types.insert(rt::GenericType {
                    location: parameter.location,
                    name,
                });
                if let Some(parameters) = parameters {
                    let parameters = {
                        let mut names = names.clone();
                        parameters
                            .iter()
                            .map(|parameter| resolve_parameter(parameter, program, &mut names))
                            .collect::<Result<Box<[_]>, _>>()?
                    };
                    let parameterized_type =
                        program.parameterized_types.insert(rt::ParameterizedType {
                            location: parameter.location,
                            name,
                            parameters,
                            underlying_type: rt::UnderlyingType::GenericType(generic_type),
                        });
                    names.insert(name, Name::ParameterizedType(parameterized_type));
                    rt::ParameterKind::ParameterizedType(parameterized_type)
                } else {
                    names.insert(name, Name::GenericType(generic_type));
                    rt::ParameterKind::Type(generic_type)
                }
            }

            ast::ParameterKind::Dyn {
                ref parameters,
                name,
            } => {
                let generic_dyn = program.generic_dyns.insert(rt::GenericDyn {
                    location: parameter.location,
                    name,
                });
                if let Some(parameters) = parameters {
                    let parameters = {
                        let mut names = names.clone();
                        parameters
                            .iter()
                            .map(|parameter| resolve_parameter(parameter, program, &mut names))
                            .collect::<Result<Box<[_]>, _>>()?
                    };
                    let parameterized_dyn =
                        program.parameterized_dyns.insert(rt::ParameterizedDyn {
                            location: parameter.location,
                            name,
                            parameters,
                            underlying_dyn: rt::UnderlyingDyn::GenericDyn(generic_dyn),
                        });
                    names.insert(name, Name::ParameterizedDyn(parameterized_dyn));
                    rt::ParameterKind::ParameterizedDyn(parameterized_dyn)
                } else {
                    names.insert(name, Name::GenericDyn(generic_dyn));
                    rt::ParameterKind::Dyn(generic_dyn)
                }
            }
        },
    })
}

fn resolve_type(
    typ: &ast::Type,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Type, ResolvingError> {
    Ok(rt::Type {
        location: typ.location,
        kind: match typ.kind {
            ast::TypeKind::Name(name) => {
                let Some(name) = names.get(&name) else {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::UnknownName { name },
                    });
                };
                match *name {
                    Name::TypeAlias(type_alias) => rt::TypeKind::TypeAlias(type_alias),
                    Name::Struct(structt) => rt::TypeKind::Struct(structt),
                    Name::GenericType(generic_type) => rt::TypeKind::Generic(generic_type),

                    Name::ParameterizedType(_)
                    | Name::GenericDyn(_)
                    | Name::ParameterizedDyn(_)
                    | Name::Function(_)
                    | Name::Variable(_) => {
                        return Err(ResolvingError {
                            location: typ.location,
                            kind: ResolvingErrorKind::ExpectedType,
                        });
                    }
                }
            }

            ast::TypeKind::Builtin(ref builtin) => match builtin {
                ast::BuiltinType::Unit => rt::TypeKind::Struct(
                    program
                        .unit_type
                        .expect("the builtin unit type should have already been declared"),
                ),

                ast::BuiltinType::Runtime => rt::TypeKind::Runtime,
                ast::BuiltinType::I64 => rt::TypeKind::I64,
            },

            ast::TypeKind::Function {
                is_unsafe,
                ref parameters,
                ref return_type,
            } => {
                let mut names = names.clone();
                let parameters = parameters
                    .iter()
                    .map(|parameter| resolve_parameter(parameter, program, &mut names))
                    .collect::<Result<Box<[_]>, _>>()?;
                let return_type = Box::new(resolve_type(return_type, program, &mut names)?);
                rt::TypeKind::Function {
                    is_unsafe,
                    parameters,
                    return_type,
                }
            }

            ast::TypeKind::Arguments {
                ref typ,
                ref arguments,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| resolve_argument(argument, program, names))
                    .collect::<Result<Box<[_]>, _>>()?;

                let ast::TypeKind::Name(name) = typ.kind else {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::ExpectedParameterizedType,
                    });
                };
                let Some(name) = names.get(&name) else {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::UnknownName { name },
                    });
                };
                match *name {
                    Name::ParameterizedType(parameterized_type) => rt::TypeKind::Instantiation {
                        parameterized_type,
                        arguments,
                    },

                    Name::TypeAlias(_)
                    | Name::Struct(_)
                    | Name::GenericType(_)
                    | Name::GenericDyn(_)
                    | Name::ParameterizedDyn(_)
                    | Name::Function(_)
                    | Name::Variable(_) => {
                        return Err(ResolvingError {
                            location: typ.location,
                            kind: ResolvingErrorKind::ExpectedParameterizedType,
                        });
                    }
                }
            }
        },
    })
}

fn resolve_dyn(
    r#dyn: &ast::Type,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Dyn, ResolvingError> {
    Ok(rt::Dyn {
        location: r#dyn.location,
        kind: match r#dyn.kind {
            ast::TypeKind::Name(name) => {
                let Some(name) = names.get(&name) else {
                    return Err(ResolvingError {
                        location: r#dyn.location,
                        kind: ResolvingErrorKind::UnknownName { name },
                    });
                };
                match *name {
                    Name::GenericDyn(generic_dyn) => rt::DynKind::Generic(generic_dyn),

                    Name::TypeAlias(_)
                    | Name::Struct(_)
                    | Name::GenericType(_)
                    | Name::ParameterizedType(_)
                    | Name::ParameterizedDyn(_)
                    | Name::Function(_)
                    | Name::Variable(_) => {
                        return Err(ResolvingError {
                            location: r#dyn.location,
                            kind: ResolvingErrorKind::ExpectedDyn,
                        });
                    }
                }
            }

            ast::TypeKind::Arguments {
                ref typ,
                ref arguments,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| resolve_argument(argument, program, names))
                    .collect::<Result<Box<[_]>, _>>()?;

                let ast::TypeKind::Name(name) = typ.kind else {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::ExpectedParameterizedDyn,
                    });
                };
                let Some(name) = names.get(&name) else {
                    return Err(ResolvingError {
                        location: typ.location,
                        kind: ResolvingErrorKind::UnknownName { name },
                    });
                };
                match *name {
                    Name::ParameterizedDyn(parameterized_dyn) => rt::DynKind::Instantiation {
                        parameterized_dyn,
                        arguments,
                    },

                    Name::TypeAlias(_)
                    | Name::Struct(_)
                    | Name::GenericType(_)
                    | Name::ParameterizedType(_)
                    | Name::GenericDyn(_)
                    | Name::Function(_)
                    | Name::Variable(_) => {
                        return Err(ResolvingError {
                            location: typ.location,
                            kind: ResolvingErrorKind::ExpectedParameterizedDyn,
                        });
                    }
                }
            }

            ast::TypeKind::Builtin(_) | ast::TypeKind::Function { .. } => {
                return Err(ResolvingError {
                    location: r#dyn.location,
                    kind: ResolvingErrorKind::ExpectedDyn,
                });
            }
        },
    })
}

fn resolve_expression(
    expression: &ast::Expression,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Expression, ResolvingError> {
    Ok(rt::Expression {
        location: expression.location,
        kind: match expression.kind {
            ast::ExpressionKind::Place(ref place) => {
                rt::ExpressionKind::Place(Box::new(resolve_place(place, program, names)?))
            }

            ast::ExpressionKind::Integer(value) => rt::ExpressionKind::Integer(value),

            ast::ExpressionKind::Block {
                is_unsafe,
                end_location,
                ref statements,
                ref last_expression,
            } => {
                let mut names = names.clone();
                rt::ExpressionKind::Block {
                    is_unsafe,
                    end_location,
                    statements: statements
                        .iter()
                        .map(|statement| resolve_statement(statement, program, &mut names))
                        .collect::<Result<Box<[_]>, _>>()?,
                    last_expression: Box::new(resolve_expression(
                        last_expression,
                        program,
                        &mut names,
                    )?),
                }
            }

            ast::ExpressionKind::Call {
                ref operand,
                ref arguments,
            } => rt::ExpressionKind::Call {
                operand: Box::new(resolve_expression(operand, program, names)?),
                arguments: arguments
                    .iter()
                    .map(|argument| resolve_argument(argument, program, names))
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            ast::ExpressionKind::Constructor {
                ref typ,
                ref members,
            } => rt::ExpressionKind::Constructor {
                typ: Box::new(resolve_type(typ, program, names)?),
                members: members
                    .iter()
                    .map(
                        |&ast::ConstructorMember {
                             location,
                             name,
                             ref value,
                         }| {
                            Ok(rt::ConstructorMember {
                                location,
                                name,
                                value: resolve_expression(value, program, names)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },

            ast::ExpressionKind::Function {
                is_unsafe,
                ref parameters,
                ref return_type,
                ref body,
            } => rt::ExpressionKind::Function(resolve_function(
                expression.location,
                is_unsafe,
                None,
                parameters,
                return_type,
                body,
                program,
                names,
            )?),
        },
    })
}

fn resolve_argument(
    argument: &ast::Argument,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Argument, ResolvingError> {
    Ok(rt::Argument {
        location: argument.location,
        kind: match argument.kind {
            ast::ArgumentKind::Value { ref expression } => rt::ArgumentKind::Value {
                expression: Box::new(resolve_expression(expression, program, names)?),
            },

            ast::ArgumentKind::Type {
                ref parameters,
                ref typ,
            } => {
                if let Some(parameters) = parameters {
                    let mut names = names.clone();
                    let parameters = parameters
                        .iter()
                        .map(|parameter| resolve_parameter(parameter, program, &mut names))
                        .collect::<Result<Box<[_]>, _>>()?;
                    let typ = resolve_type(typ, program, &mut names)?;
                    rt::ArgumentKind::ParameterizedTypeFunction { parameters, typ }
                } else {
                    rt::ArgumentKind::Type(resolve_type(typ, program, names)?)
                }
            }

            ast::ArgumentKind::Dyn {
                ref parameters,
                ref r#dyn,
            } => {
                if let Some(parameters) = parameters {
                    let mut names = names.clone();
                    let parameters = parameters
                        .iter()
                        .map(|parameter| resolve_parameter(parameter, program, &mut names))
                        .collect::<Result<Box<[_]>, _>>()?;
                    let r#dyn = resolve_dyn(r#dyn, program, &mut names)?;
                    rt::ArgumentKind::ParameterizedDynFunction { parameters, r#dyn }
                } else {
                    rt::ArgumentKind::Dyn(resolve_dyn(r#dyn, program, names)?)
                }
            }
        },
    })
}

fn resolve_place(
    place: &ast::Place,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Place, ResolvingError> {
    Ok(rt::Place {
        location: place.location,
        kind: match place.kind {
            ast::PlaceKind::Name(name) => {
                let Some(name) = names.get(&name) else {
                    return Err(ResolvingError {
                        location: place.location,
                        kind: ResolvingErrorKind::UnknownName { name },
                    });
                };
                match *name {
                    Name::Function(function) => rt::PlaceKind::Function(function),
                    Name::Variable(variable) => rt::PlaceKind::Variable(variable),

                    Name::TypeAlias(_)
                    | Name::Struct(_)
                    | Name::GenericType(_)
                    | Name::ParameterizedType(_)
                    | Name::GenericDyn(_)
                    | Name::ParameterizedDyn(_) => {
                        return Err(ResolvingError {
                            location: place.location,
                            kind: ResolvingErrorKind::ExpectedPlace,
                        });
                    }
                }
            }

            ast::PlaceKind::Let { name, ref typ } => {
                let typ = resolve_type(typ, program, names)?;
                let variable = program.variables.insert(rt::Variable {
                    location: place.location,
                    name: Some(name),
                    typ,
                });
                names.insert(name, Name::Variable(variable));
                rt::PlaceKind::Let(variable)
            }

            ast::PlaceKind::MemberAccess {
                ref operand,
                member_name,
            } => rt::PlaceKind::MemberAccess {
                operand: Box::new(resolve_expression(operand, program, names)?),
                member_name,
            },
        },
    })
}

fn resolve_statement(
    statement: &ast::Statement,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Statement, ResolvingError> {
    Ok(rt::Statement {
        location: statement.location,
        kind: match statement.kind {
            ast::StatementKind::Item(ref item) => {
                rt::StatementKind::Item(Box::new(resolve_item(item, program, names)?))
            }

            ast::StatementKind::Expression(ref expression) => rt::StatementKind::Expression(
                Box::new(resolve_expression(expression, program, names)?),
            ),

            ast::StatementKind::Assignment {
                ref pattern,
                ref value,
            } => {
                let value = Box::new(resolve_expression(value, program, names)?);
                let pattern = Box::new(resolve_pattern(pattern, program, names)?);
                rt::StatementKind::Assignment { pattern, value }
            }
        },
    })
}

fn resolve_pattern(
    pattern: &ast::Pattern,
    program: &mut ResolvedProgram,
    names: &mut FxHashMap<InternedStr, Name>,
) -> Result<rt::Pattern, ResolvingError> {
    Ok(rt::Pattern {
        location: pattern.location,
        kind: match pattern.kind {
            ast::PatternKind::Place(ref place) => {
                rt::PatternKind::Place(Box::new(resolve_place(place, program, names)?))
            }

            ast::PatternKind::Integer(value) => rt::PatternKind::Integer(value),

            ast::PatternKind::Deconstructor {
                ref typ,
                ref members,
            } => rt::PatternKind::Deconstructor {
                typ: Box::new(resolve_type(typ, program, names)?),
                members: members
                    .iter()
                    .map(
                        |&ast::DeconstructorMember {
                             location,
                             name,
                             ref pattern,
                         }| {
                            Ok(rt::DeconstructorMember {
                                location,
                                name,
                                pattern: resolve_pattern(pattern, program, names)?,
                            })
                        },
                    )
                    .collect::<Result<Box<[_]>, _>>()?,
            },
        },
    })
}

#[derive(Debug)]
pub struct ResolvingError {
    pub location: SourceLocation,
    pub kind: ResolvingErrorKind,
}

#[derive(Debug)]
pub enum ResolvingErrorKind {
    UnknownName { name: InternedStr },
    ExpectedType,
    ExpectedParameterizedType,
    ExpectedDyn,
    ExpectedParameterizedDyn,
    ExpectedPlace,
}
