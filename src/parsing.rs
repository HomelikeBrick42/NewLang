use crate::{
    interning::InternedStr,
    lexer::{Lexer, LexerError, LexerErrorKind, SourceLocation, Token, TokenKind},
    syntax_tree::{
        Argument, Attribute, AttributeKind, ColonType, EqualsType, Expression, ExpressionKind,
        Item, ItemKind, Member, Members, Parameter, ParameterKind, Parameters, ReturnType,
        Statement, StatementKind,
    },
};
use derive_more::Display;

macro_rules! expect_token {
    ($lexer:ident, $pattern:pat) => {{
        match $lexer.next_token() {
            Ok(token @ Token { kind: $pattern, .. }) => Ok(token),
            Ok(token) => Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            }),
            Err(error) => Err(ParsingError::from(error)),
        }
    }};
}

macro_rules! eat_token {
    ($lexer:ident, $pattern:pat) => {{
        let mut lexer = $lexer.clone();
        match lexer.next_token() {
            Ok(token @ Token { kind: $pattern, .. }) => {
                *$lexer = lexer;
                Ok(Some(token))
            }
            Ok(_) => Ok(None),
            Err(LexerError {
                kind: LexerErrorKind::UnexpectedEOF,
                ..
            }) => Ok(None),
            Err(error) => Err(ParsingError::from(error)),
        }
    }};
}

pub fn parse_file(filepath: InternedStr, source: &str) -> Result<Box<[Item]>, ParsingError> {
    let lexer = &mut Lexer::new(filepath, source);

    let mut items = vec![];
    loop {
        while eat_token!(lexer, TokenKind::Newline)?.is_some() {}
        if lexer.is_at_eof() {
            break;
        }
        items.push(parse_item(lexer)?);
        expect_token!(lexer, TokenKind::Newline)?;
    }
    Ok(items.into_boxed_slice())
}

pub fn parse_item(lexer: &mut Lexer<'_>) -> Result<Item, ParsingError> {
    let mut attributes = vec![];
    while let Some(hash_token) = eat_token!(lexer, TokenKind::Hash)? {
        attributes.push(Attribute {
            location: hash_token.location,
            hash_token,
            open_bracket_token: expect_token!(lexer, TokenKind::OpenBracket)?,
            kind: match lexer.next_token()? {
                builtin_token @ Token {
                    location: _,
                    kind: TokenKind::BuiltinKeyword,
                } => AttributeKind::Builtin { builtin_token },

                token => {
                    return Err(ParsingError {
                        location: token.location,
                        kind: ParsingErrorKind::ExpectedAttributeContentsButGot(token),
                    });
                }
            },
            close_bracket_token: expect_token!(lexer, TokenKind::CloseBracket)?,
        });
        eat_token!(lexer, TokenKind::Newline)?;
    }
    let attributes = attributes.into_boxed_slice();

    Ok(match lexer.next_token()? {
        type_token @ Token {
            location,
            kind: TokenKind::TypeKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Type {
                type_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                equals_type: if let Some(equal_token) = eat_token!(lexer, TokenKind::Equal)? {
                    Some(Box::new(EqualsType {
                        equal_token,
                        typ: parse_expression(lexer)?,
                    }))
                } else {
                    None
                },
            },
        },

        struct_token @ Token {
            location,
            kind: TokenKind::StructKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Struct {
                struct_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                members: {
                    let open_brace_token = expect_token!(lexer, TokenKind::OpenBrace)?;
                    let mut members = vec![];
                    let close_brace_token = loop {
                        while eat_token!(lexer, TokenKind::Newline)?.is_some() {}
                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
                            break close_brace_token;
                        }
                        members.push(Member {
                            name_token: expect_token!(lexer, TokenKind::Name(_))?,
                            colon_token: expect_token!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer)?,
                        });
                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
                            break close_brace_token;
                        }
                        eat_token!(lexer, TokenKind::Comma)?;
                    };
                    Members {
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        fn_token @ Token {
            location,
            kind: TokenKind::FnKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Function {
                fn_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                parameters: {
                    let open_parenthesis_token = expect_token!(lexer, TokenKind::OpenParenthesis)?;
                    let mut parameters = vec![];
                    let close_parenthesis_token = loop {
                        eat_token!(lexer, TokenKind::Newline)?;
                        if let Some(close_parenthesis_token) =
                            eat_token!(lexer, TokenKind::CloseParenthesis)?
                        {
                            break close_parenthesis_token;
                        }
                        parameters.push(parse_parameter(lexer)?);
                        if let Some(close_parenthesis_token) =
                            eat_token!(lexer, TokenKind::CloseParenthesis)?
                        {
                            break close_parenthesis_token;
                        }
                        eat_token!(lexer, TokenKind::Comma)?;
                    };
                    Parameters {
                        open_parenthesis_token,
                        parameters: parameters.into_boxed_slice(),
                        close_parenthesis_token,
                    }
                },
                return_type: if let Some(right_arrow_token) =
                    eat_token!(lexer, TokenKind::RightArrow)?
                {
                    Some(Box::new(ReturnType {
                        right_arrow_token,
                        typ: parse_expression(lexer)?,
                    }))
                } else {
                    None
                },
                body: if let Some(open_brace_token) = eat_token!(lexer, TokenKind::OpenBrace)? {
                    Some(Box::new(parse_block(lexer, open_brace_token)?))
                } else {
                    None
                },
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::ExpectedItemButGot(token),
            });
        }
    })
}

pub fn parse_parameter(lexer: &mut Lexer<'_>) -> Result<Parameter, ParsingError> {
    Ok(match lexer.next_token()? {
        name_token @ Token {
            location,
            kind: TokenKind::Name(_),
        } => Parameter {
            location,
            kind: ParameterKind::Value {
                name_token,
                colon_token: expect_token!(lexer, TokenKind::Colon)?,
                typ: parse_expression(lexer)?,
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::ExpectedParameterButGot(token),
            });
        }
    })
}

pub fn parse_statement(lexer: &mut Lexer<'_>) -> Result<Statement, ParsingError> {
    Ok(match lexer.clone().next_token()? {
        Token {
            location,
            kind:
                TokenKind::Hash
                | TokenKind::TypeKeyword
                | TokenKind::FnKeyword
                | TokenKind::StructKeyword
                | TokenKind::EnumKeyword,
        } => Statement {
            location,
            kind: StatementKind::Item(Box::new(parse_item(lexer)?)),
        },

        _ => {
            let expression = Box::new(parse_expression(lexer)?);
            if let Some(equal_token) = eat_token!(lexer, TokenKind::Equal)? {
                Statement {
                    location: equal_token.location,
                    kind: StatementKind::Assignment {
                        pattern: expression,
                        equal_token,
                        value: Box::new(parse_expression(lexer)?),
                    },
                }
            } else {
                Statement {
                    location: expression.location,
                    kind: StatementKind::Expression(expression),
                }
            }
        }
    })
}

pub fn parse_expression(lexer: &mut Lexer<'_>) -> Result<Expression, ParsingError> {
    let mut expression = match lexer.next_token()? {
        placeholder_token @ Token {
            location,
            kind: TokenKind::Placeholder,
        } => Expression {
            location,
            kind: ExpressionKind::Placeholder { placeholder_token },
        },

        name_token @ Token {
            location,
            kind: TokenKind::Name(_),
        } => Expression {
            location,
            kind: ExpressionKind::Name { name_token },
        },

        integer_token @ Token {
            location,
            kind: TokenKind::Integer(_),
        } => Expression {
            location,
            kind: ExpressionKind::Integer { integer_token },
        },

        open_parenthesis_token @ Token {
            location,
            kind: TokenKind::OpenParenthesis,
        } => Expression {
            location,
            kind: ExpressionKind::ParenthesisedExpression {
                open_parenthesis_token,
                expression: Box::new(parse_expression(lexer)?),
                close_parenthesis_token: expect_token!(lexer, TokenKind::CloseParenthesis)?,
            },
        },

        open_brace_token @ Token {
            location: _,
            kind: TokenKind::OpenBrace,
        } => parse_block(lexer, open_brace_token)?,

        let_token @ Token {
            location,
            kind: TokenKind::LetKeyword,
        } => Expression {
            location,
            kind: ExpressionKind::Let {
                let_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                colon_type: if let Some(colon_token) = eat_token!(lexer, TokenKind::Colon)? {
                    Some(Box::new(ColonType {
                        colon_token,
                        typ: parse_expression(lexer)?,
                    }))
                } else {
                    None
                },
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            });
        }
    };
    loop {
        expression = match lexer.clone().next_token().ok() {
            Some(
                open_parenthesis_token @ Token {
                    location,
                    kind: TokenKind::OpenParenthesis,
                },
            ) => {
                _ = lexer.next_token();

                let mut arguments = vec![];
                let close_parenthesis_token = loop {
                    eat_token!(lexer, TokenKind::Newline)?;
                    if let Some(close_parenthesis_token) =
                        eat_token!(lexer, TokenKind::CloseParenthesis)?
                    {
                        break close_parenthesis_token;
                    }
                    arguments.push(parse_argument(lexer)?);
                    if let Some(close_parenthesis_token) =
                        eat_token!(lexer, TokenKind::CloseParenthesis)?
                    {
                        break close_parenthesis_token;
                    }
                    eat_token!(lexer, TokenKind::Comma)?;
                };

                Expression {
                    location,
                    kind: ExpressionKind::Call {
                        operand: Box::new(expression),
                        open_parenthesis_token,
                        arguments: arguments.into_boxed_slice(),
                        close_parenthesis_token,
                    },
                }
            }

            Some(
                open_brace_token @ Token {
                    location,
                    kind: TokenKind::OpenBrace,
                },
            ) => {
                _ = lexer.next_token();

                let mut members = vec![];
                let close_brace_token = loop {
                    eat_token!(lexer, TokenKind::Newline)?;
                    if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
                        break close_brace_token;
                    }
                    members.push(Member {
                        name_token: expect_token!(lexer, TokenKind::Name(_))?,
                        colon_token: expect_token!(lexer, TokenKind::Colon)?,
                        typ: parse_expression(lexer)?,
                    });
                    if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
                        break close_brace_token;
                    }
                    eat_token!(lexer, TokenKind::Comma)?;
                };

                Expression {
                    location,
                    kind: ExpressionKind::Constructor {
                        typ: Box::new(expression),
                        members: Members {
                            open_brace_token,
                            members: members.into_boxed_slice(),
                            close_brace_token,
                        },
                    },
                }
            }

            _ => break,
        };
    }
    Ok(expression)
}

pub fn parse_block(
    lexer: &mut Lexer<'_>,
    open_brace_token: Token,
) -> Result<Expression, ParsingError> {
    let mut statements = vec![];
    let close_brace_token = loop {
        while eat_token!(lexer, TokenKind::Newline)?.is_some() {}
        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
            break close_brace_token;
        }
        statements.push(parse_statement(lexer)?);
        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace)? {
            break close_brace_token;
        }
        expect_token!(lexer, TokenKind::Newline)?;
    };
    Ok(Expression {
        location: open_brace_token.location,
        kind: ExpressionKind::Block {
            open_brace_token,
            statements: statements.into_boxed_slice(),
            close_brace_token,
        },
    })
}

pub fn parse_argument(lexer: &mut Lexer<'_>) -> Result<Argument, ParsingError> {
    #[expect(clippy::match_single_binding)]
    Ok(match lexer.clone().next_token()? {
        _ => Argument::Value {
            expression: parse_expression(lexer)?,
        },
    })
}

#[derive(Debug, Display)]
#[display("{location}: {kind}")]
pub struct ParsingError {
    pub location: SourceLocation,
    pub kind: ParsingErrorKind,
}

#[derive(Debug, Display)]
pub enum ParsingErrorKind {
    #[display("{_0}")]
    LexerError(LexerErrorKind),
    #[display("Unexpected {_0}")]
    UnexpectedToken(Token),
    #[display("Expected attribute contents but got {_0}")]
    ExpectedAttributeContentsButGot(Token),
    #[display("Expected item but got {_0}")]
    ExpectedItemButGot(Token),
    #[display("Expected parameter but got {_0}")]
    ExpectedParameterButGot(Token),
}

impl From<LexerError> for ParsingError {
    fn from(LexerError { location, kind }: LexerError) -> Self {
        Self {
            location,
            kind: ParsingErrorKind::LexerError(kind),
        }
    }
}
