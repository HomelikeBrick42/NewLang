use crate::{
    ast,
    interning::InternedStr,
    lexing::{SourceLocation, TokenKind},
    syntax_tree as st,
};
use derive_more::Display;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{location}: {kind}")]
pub struct ValidatingError {
    pub location: SourceLocation,
    pub kind: ValidatingErrorKind,
}

#[derive(Debug, Display)]
pub enum ValidatingErrorKind {
    #[display("Unknown attribute '{_0}'")]
    UnknownAttribute(InternedStr),
    #[display("Expected value")]
    ExpectedValue,
    #[display("Expected pattern")]
    ExpectedPattern,
    #[display("Expected type")]
    ExpectedType,
    #[display("Expected path")]
    ExpectedPath,
}

pub fn validate_items(items: &[st::Item]) -> Result<Box<[ast::Item]>, ValidatingError> {
    items.iter().map(validate_item).collect()
}

pub fn validate_item(
    &st::Item {
        ref attributes,
        location,
        ref kind,
    }: &st::Item,
) -> Result<ast::Item, ValidatingError> {
    let mut builtin = false;
    for st::Attribute {
        hash_token: _,
        open_bracket_token: _,
        name_token,
        close_bracket_token: _,
    } in attributes
    {
        let TokenKind::Name(name) = name_token.kind else {
            unreachable!()
        };
        match name.as_str() {
            "builtin" => builtin = true,

            _ => {
                return Err(ValidatingError {
                    location: name_token.location,
                    kind: ValidatingErrorKind::UnknownAttribute(name),
                });
            }
        }
    }

    Ok(ast::Item {
        location,
        builtin,
        kind: match kind {
            st::ItemKind::Module {
                module_token: _,
                name_token,
                items,
            } => ast::ItemKind::Module {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                items: items
                    .iter()
                    .map(validate_item)
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ItemKind::Fn {
                fn_token: _,
                name_token,
                parameters,
                return_type,
                body,
            } => ast::ItemKind::Fn {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                parameters: parameters
                    .parameters
                    .iter()
                    .map(|parameter| {
                        Ok(match parameter {
                            st::Parameter::Value {
                                name_token,
                                colon_token: _,
                                typ,
                            } => ast::Parameter {
                                location: name_token.location,
                                kind: ast::ParameterKind::Value {
                                    name: {
                                        let TokenKind::Name(name) = name_token.kind else {
                                            unreachable!()
                                        };
                                        name
                                    },
                                    typ: validate_type(typ)?,
                                },
                            },

                            st::Parameter::Type { name_token } => ast::Parameter {
                                location: name_token.location,
                                kind: ast::ParameterKind::Type {
                                    name: {
                                        let TokenKind::Name(name) = name_token.kind else {
                                            unreachable!()
                                        };
                                        name
                                    },
                                },
                            },

                            st::Parameter::Lifetime { lifetime_token } => ast::Parameter {
                                location: lifetime_token.location,
                                kind: ast::ParameterKind::Lifetime {
                                    name: {
                                        let TokenKind::Lifetime(name) = lifetime_token.kind else {
                                            unreachable!()
                                        };
                                        name
                                    },
                                },
                            },
                        })
                    })
                    .collect::<Result<_, ValidatingError>>()?,
                return_type: Box::new(
                    return_type
                        .as_ref()
                        .map(|return_type| validate_type(&return_type.typ))
                        .transpose()?
                        .unwrap_or(ast::Type {
                            location: parameters.close_parenthesis_token.location,
                            kind: ast::TypeKind::Unit,
                        }),
                ),
                body: body
                    .as_ref()
                    .map(|body| validate_expression(body))
                    .transpose()?
                    .map(Box::new),
            },

            st::ItemKind::Struct {
                struct_token: _,
                name_token,
                members,
            } => ast::ItemKind::Struct {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                members: members
                    .members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::Member {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!()
                                    };
                                    name
                                },
                                typ: validate_type(typ)?,
                            })
                        },
                    )
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ItemKind::Enum {
                enum_token: _,
                name_token,
                members,
            } => ast::ItemKind::Enum {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                members: members
                    .members
                    .iter()
                    .map(
                        |st::Member {
                             name_token,
                             colon_token: _,
                             typ,
                         }| {
                            Ok(ast::Member {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!()
                                    };
                                    name
                                },
                                typ: validate_type(typ)?,
                            })
                        },
                    )
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ItemKind::Type {
                type_token: _,
                name_token,
                equals_type,
            } => ast::ItemKind::Type {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                typ: equals_type
                    .as_ref()
                    .map(|equals_type| validate_type(&equals_type.typ))
                    .transpose()?
                    .map(Box::new),
            },
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
                equals_token: _,
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
            st::ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token: _,
                expression,
                close_parenthesis_token: _,
            } => return validate_expression(expression),

            st::ExpressionKind::Name { .. } | st::ExpressionKind::PathAccess { .. } => {
                ast::ExpressionKind::Path(Box::new(validate_path(expression)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::ExpressionKind::Integer(value)
            }

            st::ExpressionKind::Block {
                open_brace_token: _,
                statements,
                close_brace_token,
            } => {
                let mut statements = statements.iter().map(validate_statement).collect::<Result<
                    Vec<_>,
                    ValidatingError,
                >>(
                )?;
                let last_expression = if let Some(ast::Statement {
                    kind: ast::StatementKind::Expression(last_expression),
                    ..
                }) = statements
                    .pop_if(|statement| matches!(statement.kind, ast::StatementKind::Expression(_)))
                {
                    last_expression
                } else {
                    Box::new(unit_expression(close_brace_token.location))
                };
                ast::ExpressionKind::Block {
                    statements: statements.into_boxed_slice(),
                    last_expression,
                }
            }

            st::ExpressionKind::Constructor {
                typ,
                open_brace_token: _,
                members,
                close_brace_token: _,
            } => ast::ExpressionKind::Constructor {
                typ: Box::new(validate_type(typ)?),
                members: members
                    .iter()
                    .map(
                        |st::ConstructorArgument {
                             name_token,
                             colon_token: _,
                             value,
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

            st::ExpressionKind::UnaryOperator {
                operator_token,
                operand,
            } => ast::ExpressionKind::Unary {
                operator: match operator_token.kind {
                    TokenKind::Plus => ast::UnaryOperator::Plus,
                    TokenKind::Minus => ast::UnaryOperator::Negate,
                    _ => unreachable!("unexpected unary operator"),
                },
                operand: Box::new(validate_expression(operand)?),
            },

            st::ExpressionKind::BinaryOperator {
                left,
                operator_token,
                right,
            } => ast::ExpressionKind::Binary {
                left: Box::new(validate_expression(left)?),
                operator: match operator_token.kind {
                    TokenKind::Plus => ast::BinaryOperator::Add,
                    TokenKind::Minus => ast::BinaryOperator::Subtract,
                    TokenKind::Asterisk => ast::BinaryOperator::Multiply,
                    TokenKind::Slash => ast::BinaryOperator::Divide,
                    _ => unreachable!("unexpected binary operator"),
                },
                right: Box::new(validate_expression(right)?),
            },

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
                            st::Argument::ValueOrType(expression) => ast::Argument {
                                location: expression.location,
                                kind: ast::ArgumentKind::ValueOrType(validate_expression(
                                    expression,
                                )?),
                            },
                            st::Argument::Lifetime { lifetime_token } => ast::Argument {
                                location: lifetime_token.location,
                                kind: ast::ArgumentKind::Lifetime {
                                    name: {
                                        let TokenKind::Lifetime(name) = lifetime_token.kind else {
                                            unreachable!()
                                        };
                                        name
                                    },
                                },
                            },
                        })
                    })
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::MemberAccess {
                operand,
                dot_token: _,
                name_token,
            } => ast::ExpressionKind::MemberAccess {
                operand: Box::new(validate_expression(operand)?),
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ExpressionKind::Let { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedValue,
                });
            }
        },
    })
}

fn unit_expression(location: SourceLocation) -> ast::Expression {
    ast::Expression {
        location,
        kind: ast::ExpressionKind::Constructor {
            typ: Box::new(ast::Type {
                location,
                kind: ast::TypeKind::Unit,
            }),
            members: Box::new([]),
        },
    }
}

pub fn validate_pattern(
    pattern @ &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Pattern, ValidatingError> {
    Ok(ast::Pattern {
        location,
        kind: match kind {
            st::ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token: _,
                expression,
                close_parenthesis_token: _,
            } => return validate_pattern(expression),

            st::ExpressionKind::Name { .. } | st::ExpressionKind::PathAccess { .. } => {
                ast::PatternKind::Path(Box::new(validate_path(pattern)?))
            }

            st::ExpressionKind::Integer { integer_token } => {
                let TokenKind::Integer(value) = integer_token.kind else {
                    unreachable!()
                };
                ast::PatternKind::Integer(value)
            }

            st::ExpressionKind::Constructor {
                typ,
                open_brace_token: _,
                members,
                close_brace_token: _,
            } => ast::PatternKind::Destructor {
                typ: Box::new(validate_type(typ)?),
                members: members
                    .iter()
                    .map(
                        |st::ConstructorArgument {
                             name_token,
                             colon_token: _,
                             value,
                         }| {
                            Ok(ast::DestructorMember {
                                location: name_token.location,
                                name: {
                                    let TokenKind::Name(name) = name_token.kind else {
                                        unreachable!()
                                    };
                                    name
                                },
                                pattern: validate_pattern(value)?,
                            })
                        },
                    )
                    .collect::<Result<_, ValidatingError>>()?,
            },

            st::ExpressionKind::MemberAccess {
                operand,
                dot_token: _,
                name_token,
            } => ast::PatternKind::MemberAccess {
                operand: Box::new(validate_expression(operand)?),
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ExpressionKind::Let {
                let_token: _,
                name_token,
                typ,
            } => ast::PatternKind::Let {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
                typ: typ
                    .as_ref()
                    .map(|typ| validate_type(&typ.typ))
                    .transpose()?
                    .map(Box::new),
            },

            st::ExpressionKind::Block { .. }
            | st::ExpressionKind::UnaryOperator { .. }
            | st::ExpressionKind::BinaryOperator { .. }
            | st::ExpressionKind::Call { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedPattern,
                });
            }
        },
    })
}

pub fn validate_type(
    typ @ &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Type, ValidatingError> {
    Ok(ast::Type {
        location,
        kind: match kind {
            st::ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token: _,
                expression,
                close_parenthesis_token: _,
            } => return validate_type(expression),

            st::ExpressionKind::Name { .. } | st::ExpressionKind::PathAccess { .. } => {
                ast::TypeKind::Path(Box::new(validate_path(typ)?))
            }

            st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Constructor { .. }
            | st::ExpressionKind::UnaryOperator { .. }
            | st::ExpressionKind::BinaryOperator { .. }
            | st::ExpressionKind::Call { .. }
            | st::ExpressionKind::Let { .. }
            | st::ExpressionKind::MemberAccess { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedType,
                });
            }
        },
    })
}

pub fn validate_path(
    &st::Expression { location, ref kind }: &st::Expression,
) -> Result<ast::Path, ValidatingError> {
    Ok(ast::Path {
        location,
        kind: match kind {
            st::ExpressionKind::Name { name_token } => ast::PathKind::Name {
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ExpressionKind::PathAccess {
                operand,
                colon_colon_token: _,
                name_token,
            } => ast::PathKind::PathAccess {
                operand: Box::new(validate_path(operand)?),
                name: {
                    let TokenKind::Name(name) = name_token.kind else {
                        unreachable!()
                    };
                    name
                },
            },

            st::ExpressionKind::ParenthesizedExpression { .. }
            | st::ExpressionKind::Integer { .. }
            | st::ExpressionKind::Block { .. }
            | st::ExpressionKind::Constructor { .. }
            | st::ExpressionKind::UnaryOperator { .. }
            | st::ExpressionKind::BinaryOperator { .. }
            | st::ExpressionKind::Call { .. }
            | st::ExpressionKind::MemberAccess { .. }
            | st::ExpressionKind::Let { .. } => {
                return Err(ValidatingError {
                    location,
                    kind: ValidatingErrorKind::ExpectedPath,
                });
            }
        },
    })
}
