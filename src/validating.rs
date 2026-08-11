use crate::{
    ast,
    lexer::{SourceLocation, TokenKind},
    syntax_tree::{self as st, ParenthesisArguments, ParenthesisParameters},
};
use derive_more::Display;

pub fn validate_item(
    &st::Item {
        ref attributes,
        location,
        ref kind,
    }: &st::Item,
) -> Result<ast::Item, ValidatingError> {
    let mut builtin = false;
    for attribute in attributes {
        match attribute.kind {
            st::AttributeKind::Builtin { builtin_token: _ } => {
                assert!(!builtin, "more than one builtin attribute on a type");
                builtin = true;
            }
        }
    }

    Ok(ast::Item {
        location,
        kind: match kind {
            st::ItemKind::Type {
                type_token: _,
                name_token,
                parameters,
                equals_type,
            } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::ItemKind::Type {
                    name,
                    parameters: if let Some(parameters) = parameters {
                        Some(
                            parameters
                                .parameters
                                .iter()
                                .map(validate_parameter)
                                .collect::<Result<_, ValidatingError>>()?,
                        )
                    } else {
                        None
                    },
                    typ: if let Some(equals_type) = equals_type {
                        assert!(!builtin, "builtin type aliases assigned a type");
                        validate_type(&equals_type.typ)?
                    } else if builtin {
                        ast::Type {
                            location,
                            kind: ast::TypeKind::DeclareBuiltin(match name.as_str() {
                                "Runtime" => ast::BuiltinTypeAlias::Runtime,
                                "I64" => ast::BuiltinTypeAlias::I64,
                                name => unreachable!("unknown builtin type alias '{name}'"),
                            }),
                        }
                    } else {
                        return Err(ValidatingError {
                            location,
                            kind: ValidatingErrorKind::TypeAliasMustBeAssignedType,
                        });
                    },
                }
            }

            st::ItemKind::Struct {
                struct_token: _,
                name_token,
                parameters,
                members:
                    st::Members {
                        open_brace_token: _,
                        members,
                        close_brace_token: _,
                    },
            } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::ItemKind::Struct {
                    builtin_type: if builtin {
                        Some(match name.as_str() {
                            "Unit" => ast::BuiltinStruct::Unit,
                            name => unreachable!("unknown builtin struct '{name}'"),
                        })
                    } else {
                        None
                    },
                    name,
                    parameters: if let Some(parameters) = parameters {
                        Some(
                            parameters
                                .parameters
                                .iter()
                                .map(validate_parameter)
                                .collect::<Result<_, ValidatingError>>()?,
                        )
                    } else {
                        None
                    },
                    members: members
                        .iter()
                        .map(
                            |st::Member {
                                 name_token,
                                 colon_token: _,
                                 typ,
                             }| {
                                let TokenKind::Name(name) = name_token.kind else {
                                    unreachable!()
                                };
                                Ok(ast::StructMember {
                                    location: name_token.location,
                                    name,
                                    typ: validate_type(typ)?,
                                })
                            },
                        )
                        .collect::<Result<_, ValidatingError>>()?,
                }
            }

            st::ItemKind::Function {
                unsafe_token,
                fn_token: _,
                name_token,
                parameters,
                return_type,
                body,
            } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::ItemKind::Function {
                    is_unsafe: unsafe_token.is_some(),
                    name,
                    parameters: parameters
                        .parameters
                        .iter()
                        .map(validate_parameter)
                        .collect::<Result<_, ValidatingError>>()?,
                    return_type: if let Some(return_type) = return_type {
                        validate_type(&return_type.typ)?
                    } else {
                        ast::Type {
                            location: parameters.close_parenthesis_token.location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                        }
                    },
                    body: if let Some(body) = body {
                        assert!(!builtin, "builtin function with a body");
                        ast::FunctionBody::Expression(Box::new(validate_expression(body)?))
                    } else if builtin {
                        ast::FunctionBody::Builtin(match name.as_str() {
                            "print_i64" => ast::BuiltinFunctionBody::PrintI64,
                            "transmute" => ast::BuiltinFunctionBody::Transmute,
                            name => unreachable!("unknown builtin function '{name}'"),
                        })
                    } else {
                        return Err(ValidatingError {
                            location,
                            kind: ValidatingErrorKind::FunctionMustHaveBody,
                        });
                    },
                }
            }
        },
    })
}

fn validate_parameter(
    &st::Parameter { location, ref kind }: &st::Parameter,
) -> Result<ast::Parameter, ValidatingError> {
    Ok(ast::Parameter {
        location,
        kind: match kind {
            st::ParameterKind::Value {
                name_token,
                colon_token: _,
                typ,
            } => ast::ParameterKind::Value {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                typ: Box::new(validate_type(typ)?),
            },

            st::ParameterKind::Type {
                type_token: _,
                parameters,
                name_token,
            } => ast::ParameterKind::Type {
                parameters: if let Some(parameters) = parameters {
                    Some(
                        parameters
                            .parameters
                            .iter()
                            .map(validate_parameter)
                            .collect::<Result<_, ValidatingError>>()?,
                    )
                } else {
                    None
                },
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ParameterKind::Dyn {
                dyn_token: _,
                parameters,
                name_token,
            } => ast::ParameterKind::Dyn {
                parameters: if let Some(parameters) = parameters {
                    Some(
                        parameters
                            .parameters
                            .iter()
                            .map(validate_parameter)
                            .collect::<Result<_, ValidatingError>>()?,
                    )
                } else {
                    None
                },
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },
        },
    })
}

fn validate_statement(
    &st::Statement { location, ref kind }: &st::Statement,
) -> Result<ast::Statement, ValidatingError> {
    Ok(ast::Statement {
        location,
        kind: match kind {
            st::StatementKind::Item(item) => {
                ast::StatementKind::Item(Box::new(validate_item(item)?))
            }

            st::StatementKind::Expression(expression) => {
                ast::StatementKind::Expression(Box::new(validate_expression(expression)?))
            }

            st::StatementKind::Assignment {
                pattern,
                equal_token: _,
                value,
            } => ast::StatementKind::Assignment {
                pattern: Box::new(validate_pattern(pattern)?),
                value: Box::new(validate_expression(value)?),
            },
        },
    })
}

fn validate_expression(
    expression @ &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Expression, ValidatingError> {
    Ok(ast::Expression {
        location,
        kind: match kind {
            st::ExpressionKind::ParenthesisedExpression {
                open_parenthesis_token: _,
                expression,
                close_parenthesis_token: _,
            } => return validate_expression(expression),

            st::ExpressionKind::Block {
                unsafe_token,
                open_brace_token: _,
                statements,
                close_brace_token,
            } => {
                let mut statements = statements.iter().map(validate_statement).collect::<Result<
                    Vec<_>,
                    ValidatingError,
                >>(
                )?;

                let last_expression = if let Some(statement) = statements
                    .pop_if(|statement| matches!(statement.kind, ast::StatementKind::Expression(_)))
                {
                    let ast::StatementKind::Expression(expression) = statement.kind else {
                        unreachable!()
                    };
                    expression
                } else {
                    Box::new(ast::Expression {
                        location: close_brace_token.location,
                        kind: ast::ExpressionKind::Constructor {
                            typ: Box::new(ast::Type {
                                location: close_brace_token.location,
                                kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                            }),
                            members: Box::new([]),
                        },
                    })
                };

                ast::ExpressionKind::Block {
                    is_unsafe: unsafe_token.is_some(),
                    end_location: close_brace_token.location,
                    statements: statements.into_boxed_slice(),
                    last_expression,
                }
            }

            st::ExpressionKind::Name { .. }
            | st::ExpressionKind::Let { .. }
            | st::ExpressionKind::MemberAccess { .. } => {
                ast::ExpressionKind::Place(Box::new(validate_place(expression)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::ExpressionKind::Integer(value)
            }

            st::ExpressionKind::ParenthesisCall {
                operand,
                arguments:
                    ParenthesisArguments {
                        open_parenthesis_token: _,
                        arguments,
                        close_parenthesis_token: _,
                    },
            } => ast::ExpressionKind::Call {
                operand: Box::new(validate_expression(operand)?),
                arguments: arguments
                    .iter()
                    .map(validate_argument)
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::Constructor { typ, members } => ast::ExpressionKind::Constructor {
                typ: Box::new(validate_type(typ)?),
                members: members
                    .members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ: value,
                         }| {
                            Ok(ast::ConstructorMember {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!()
                                    };
                                    name
                                },
                                value: validate_expression(value)?,
                            })
                        },
                    )
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::Function {
                unsafe_token,
                fn_token: _,
                parameters:
                    ParenthesisParameters {
                        open_parenthesis_token: _,
                        parameters,
                        close_parenthesis_token,
                    },
                return_type,
                body,
            } => ast::ExpressionKind::Function {
                is_unsafe: unsafe_token.is_some(),
                parameters: parameters
                    .iter()
                    .map(validate_parameter)
                    .collect::<Result<_, ValidatingError>>()?,
                return_type: if let Some(return_type) = return_type {
                    Box::new(validate_type(&return_type.typ)?)
                } else {
                    Box::new(ast::Type {
                        location: close_parenthesis_token.location,
                        kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                    })
                },
                body: if let Some(body) = body {
                    Box::new(validate_expression(body)?)
                } else {
                    return Err(ValidatingError {
                        location,
                        kind: ValidatingErrorKind::ExpectedExpression,
                    });
                },
            },
        },
    })
}

fn validate_argument(argument: &st::Argument) -> Result<ast::Argument, ValidatingError> {
    Ok(match argument {
        st::Argument::Value { expression } => ast::Argument {
            location: expression.location,
            kind: ast::ArgumentKind::Value {
                expression: validate_expression(expression)?,
            },
        },

        st::Argument::Type {
            type_token,
            parameters,
            typ,
        } => ast::Argument {
            location: type_token.location,
            kind: ast::ArgumentKind::Type {
                parameters: if let Some(parameters) = parameters {
                    Some(
                        parameters
                            .parameters
                            .iter()
                            .map(validate_parameter)
                            .collect::<Result<_, ValidatingError>>()?,
                    )
                } else {
                    None
                },
                typ: Box::new(validate_type(typ)?),
            },
        },

        st::Argument::Dyn {
            dyn_token,
            parameters,
            typ,
        } => ast::Argument {
            location: dyn_token.location,
            kind: ast::ArgumentKind::Dyn {
                parameters: if let Some(parameters) = parameters {
                    Some(
                        parameters
                            .parameters
                            .iter()
                            .map(validate_parameter)
                            .collect::<Result<_, ValidatingError>>()?,
                    )
                } else {
                    None
                },
                typ: Box::new(validate_type(typ)?),
            },
        },
    })
}

fn validate_pattern(
    expression @ &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Pattern, ValidatingError> {
    Ok(ast::Pattern {
        location,
        kind: match kind {
            st::ExpressionKind::ParenthesisedExpression {
                open_parenthesis_token: _,
                expression,
                close_parenthesis_token: _,
            } => return validate_pattern(expression),

            st::ExpressionKind::Name { .. }
            | st::ExpressionKind::Let { .. }
            | st::ExpressionKind::MemberAccess { .. } => {
                ast::PatternKind::Place(Box::new(validate_place(expression)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::PatternKind::Integer(value)
            }

            st::ExpressionKind::Constructor { typ, members } => ast::PatternKind::Deconstructor {
                typ: Box::new(validate_type(typ)?),
                members: members
                    .members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ: pattern,
                         }| {
                            Ok(ast::DeconstructorMember {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!()
                                    };
                                    name
                                },
                                pattern: validate_pattern(pattern)?,
                            })
                        },
                    )
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::Block { .. }
            | st::ExpressionKind::ParenthesisCall { .. }
            | st::ExpressionKind::Function { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedPattern,
                });
            }
        },
    })
}

fn validate_place(
    &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Place, ValidatingError> {
    Ok(ast::Place {
        location,
        kind: match kind {
            st::ExpressionKind::Name { name_token } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::PlaceKind::Name(name)
            }

            st::ExpressionKind::Let {
                let_token: _,
                name_token,
                colon_type,
            } => ast::PlaceKind::Let {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                typ: Box::new(validate_type(&colon_type.typ)?),
            },

            st::ExpressionKind::MemberAccess {
                operand,
                dot_token: _,
                name_token,
            } => ast::PlaceKind::MemberAccess {
                operand: Box::new(validate_expression(operand)?),
                member_name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ExpressionKind::ParenthesisedExpression { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::ParenthesisCall { .. }
            | st::ExpressionKind::Constructor { .. }
            | st::ExpressionKind::Function { .. } => unreachable!(),
        },
    })
}

fn validate_type(
    &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Type, ValidatingError> {
    Ok(ast::Type {
        location,
        kind: match kind {
            st::ExpressionKind::Name { name_token } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::TypeKind::Name(name)
            }

            st::ExpressionKind::Function {
                unsafe_token,
                fn_token: _,
                parameters,
                return_type,
                body,
            } => {
                if let Some(body) = body {
                    return Err(ValidatingError {
                        location: body.location,
                        kind: ValidatingErrorKind::ExpectedType,
                    });
                };
                ast::TypeKind::Function {
                    is_unsafe: unsafe_token.is_some(),
                    parameters: parameters
                        .parameters
                        .iter()
                        .map(validate_parameter)
                        .collect::<Result<_, ValidatingError>>()?,
                    return_type: if let Some(return_type) = return_type {
                        Box::new(validate_type(&return_type.typ)?)
                    } else {
                        Box::new(ast::Type {
                            location: parameters.close_parenthesis_token.location,
                            kind: ast::TypeKind::Builtin(ast::BuiltinType::Unit),
                        })
                    },
                }
            }

            st::ExpressionKind::ParenthesisCall {
                operand,
                arguments:
                    ParenthesisArguments {
                        open_parenthesis_token: _,
                        arguments,
                        close_parenthesis_token: _,
                    },
            } => ast::TypeKind::Arguments {
                typ: Box::new(validate_type(operand)?),
                arguments: arguments
                    .iter()
                    .map(validate_argument)
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::ParenthesisedExpression { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::Let { .. }
            | st::ExpressionKind::Constructor { .. }
            | st::ExpressionKind::MemberAccess { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedType,
                });
            }
        },
    })
}

#[derive(Debug, Display)]
#[display("{location}: {kind}")]
pub struct ValidatingError {
    pub location: SourceLocation,
    pub kind: ValidatingErrorKind,
}

#[derive(Debug, Display)]
pub enum ValidatingErrorKind {
    #[display("A type alias must be assigned a type")]
    TypeAliasMustBeAssignedType,
    #[display("A function must have a function body")]
    FunctionMustHaveBody,
    #[display("Expected an expression")]
    ExpectedExpression,
    #[display("Expected a pattern")]
    ExpectedPattern,
    #[display("Expected a type")]
    ExpectedType,
}
