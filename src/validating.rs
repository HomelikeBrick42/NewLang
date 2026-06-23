use crate::{
    ast,
    lexer::{SourceLocation, TokenKind},
    syntax_tree as st,
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
                equals_type,
            } => {
                let TokenKind::Name(name) = name_token.kind else {
                    unreachable!()
                };
                ast::ItemKind::Type {
                    name,
                    typ: if let Some(equals_type) = equals_type {
                        assert!(!builtin, "builtin type aliases assigned a type");
                        validate_type(&equals_type.typ)?
                    } else if builtin {
                        ast::Type {
                            location,
                            kind: ast::TypeKind::DeclareBuiltin(match name.as_str() {
                                "Unit" => ast::BuiltinType::Unit,
                                "Runtime" => ast::BuiltinType::Runtime,
                                "I64" => ast::BuiltinType::I64,
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

            st::ItemKind::Function {
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
                    name,
                    parameters: parameters
                        .parameters
                        .iter()
                        .map(|&st::Parameter { location, ref kind }| {
                            Ok(ast::Parameter {
                                location,
                                kind: match kind {
                                    st::ParameterKind::Value {
                                        name_token,
                                        colon_token: _,
                                        typ,
                                    } => {
                                        let TokenKind::Name(name) = name_token.kind else {
                                            unreachable!()
                                        };
                                        ast::ParameterKind::Value {
                                            name,
                                            typ: Box::new(validate_type(typ)?),
                                        }
                                    }
                                },
                            })
                        })
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

pub fn validate_statement(
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

pub fn validate_expression(
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
                open_brace_token: _,
                statements,
                close_brace_token: _,
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
                    todo!()
                };

                ast::ExpressionKind::Block {
                    statements: statements.into_boxed_slice(),
                    last_expression,
                }
            }

            st::ExpressionKind::Name { .. } | st::ExpressionKind::Let { .. } => {
                ast::ExpressionKind::Place(Box::new(validate_place(expression)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::ExpressionKind::Integer(value)
            }

            st::ExpressionKind::Call {
                operand,
                open_parenthesis_token: _,
                arguments,
                close_parenthesis_token: _,
            } => ast::ExpressionKind::Call {
                operand: Box::new(validate_expression(operand)?),
                arguments: arguments
                    .iter()
                    .map(|argument| {
                        Ok(match argument {
                            st::Argument::Value { expression } => ast::Argument::Value {
                                expression: validate_expression(expression)?,
                            },
                        })
                    })
                    .collect::<Result<_, ValidatingError>>()?,
            },
        },
    })
}

pub fn validate_pattern(
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

            st::ExpressionKind::Name { .. } | st::ExpressionKind::Let { .. } => {
                ast::PatternKind::Place(Box::new(validate_place(expression)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::PatternKind::Integer(value)
            }

            st::ExpressionKind::Block { .. } | st::ExpressionKind::Call { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedPattern,
                });
            }
        },
    })
}

pub fn validate_place(
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
                typ: Box::new(if let Some(colon_type) = colon_type {
                    validate_type(&colon_type.typ)?
                } else {
                    ast::Type {
                        location,
                        kind: ast::TypeKind::Infer,
                    }
                }),
            },

            st::ExpressionKind::ParenthesisedExpression { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::Call { .. } => unreachable!(),
        },
    })
}

pub fn validate_type(
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

            st::ExpressionKind::ParenthesisedExpression { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::Call { .. }
            | st::ExpressionKind::Let { .. } => {
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
