use crate::{
    FILE_EXTENSION,
    interning::InternedStr,
    lexing::{Lexer, LexerErrorKind, LexingError, SourceLocation, Token, TokenKind},
    syntax_tree::{
        Argument, Attribute, ColonType, ConstructorArgument, EqualsType, Expression,
        ExpressionKind, Item, ItemKind, Member, Members, Parameter, Parameters, ReturnType,
        Statement, StatementKind,
    },
};
use derive_more::Display;
use std::path::Path;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{location}: {kind}")]
pub struct ParsingError {
    pub location: SourceLocation,
    pub kind: ParsingErrorKind,
}

#[derive(Debug, Display)]
pub enum ParsingErrorKind {
    #[display("{_0}")]
    LexerError(LexerErrorKind),
    #[display("Unable to open file '{_0}'")]
    UnableToOpenFile(InternedStr),
    #[display("Unexpected {_0}")]
    UnexpectedToken(Token),
    #[display("Expected item but got {_0}")]
    ExpectedItemButGot(Token),
    #[display("Expected expression or type or pattern but got {_0}")]
    ExpectedExpressionButGot(Token),
}

impl From<LexingError> for ParsingError {
    fn from(LexingError { location, kind }: LexingError) -> Self {
        ParsingError {
            location,
            kind: ParsingErrorKind::LexerError(kind),
        }
    }
}

macro_rules! eat_token {
    ($lexer:ident, $pattern:pat) => {{
        let mut lexer = $lexer.clone();
        match lexer.next_token() {
            Ok(token @ Token { kind: $pattern, .. }) => {
                *$lexer = lexer;
                Some(token)
            }
            _ => None,
        }
    }};
}

macro_rules! expect_token {
    ($lexer:ident, $pattern:pat $(, $names:ident)*) => {{
        #[allow(clippy::double_parens)]
        match $lexer.next_token() {
            Ok(token @ Token { kind: $pattern, .. }) => Ok((token $(, $names)*)),
            Ok(token) => Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::UnexpectedToken(token),
            }),
            Err(error) => Err(ParsingError {
                location: error.location,
                kind: ParsingErrorKind::LexerError(error.kind),
            }),
        }
    }};
}

pub fn parse_file(filepath: InternedStr, source: &str) -> Result<Box<[Item]>, ParsingError> {
    let mut items = vec![];

    let lexer = &mut Lexer::new(filepath, source);
    loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if lexer.is_empty() {
            break;
        }

        items.push(parse_item(lexer)?);

        if lexer.is_empty() {
            break;
        }
        expect_token!(lexer, TokenKind::Newline)?;
    }

    Ok(items.into_boxed_slice())
}

pub fn parse_attribute(lexer: &mut Lexer, hash_token: Token) -> Result<Attribute, ParsingError> {
    Ok(Attribute {
        hash_token,
        open_bracket_token: expect_token!(lexer, TokenKind::OpenBracket)?,
        name_token: expect_token!(lexer, TokenKind::Name(_))?,
        close_bracket_token: expect_token!(lexer, TokenKind::CloseBracket)?,
    })
}

pub fn parse_item(lexer: &mut Lexer) -> Result<Item, ParsingError> {
    let mut attributes = vec![];
    while let Some(hash_token) = eat_token!(lexer, TokenKind::Hash) {
        attributes.push(parse_attribute(lexer, hash_token)?);
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
    }
    let attributes = attributes.into_boxed_slice();

    Ok(match lexer.next_token()? {
        module_token @ Token {
            location,
            kind: TokenKind::ModuleKeyword,
        } => {
            let (name_token, name) = expect_token!(lexer, TokenKind::Name(name), name)?;

            let items = if eat_token!(lexer, TokenKind::OpenBrace).is_some() {
                let mut items = vec![];
                loop {
                    if eat_token!(lexer, TokenKind::CloseBrace).is_some() {
                        break;
                    }
                    while eat_token!(lexer, TokenKind::Newline).is_some() {}
                    if eat_token!(lexer, TokenKind::CloseBrace).is_some() {
                        break;
                    }

                    items.push(parse_item(lexer)?);
                }
                items.into_boxed_slice()
            } else {
                let filepath: InternedStr = Path::new(location.filepath.as_str())
                    .with_file_name(name.as_str())
                    .with_extension(FILE_EXTENSION)
                    .to_str()
                    .unwrap()
                    .into();
                let Ok(source) = std::fs::read_to_string(filepath.as_str()) else {
                    return Err(ParsingError {
                        location,
                        kind: ParsingErrorKind::UnableToOpenFile(filepath),
                    });
                };
                parse_file(filepath, &source)?
            };

            Item {
                attributes,
                location,
                kind: ItemKind::Module {
                    module_token,
                    name_token,
                    items,
                },
            }
        }

        fn_token @ Token {
            location,
            kind: TokenKind::FnKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Fn {
                fn_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                parameters: {
                    let open_parenthesis_token = expect_token!(lexer, TokenKind::OpenParenthesis)?;

                    let mut parameters = vec![];
                    let close_parenthesis_token = loop {
                        while eat_token!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_parenthesis_token) =
                            eat_token!(lexer, TokenKind::CloseParenthesis)
                        {
                            break close_parenthesis_token;
                        }

                        parameters.push(
                            if let Some(lifetime_token) = eat_token!(lexer, TokenKind::Lifetime(_))
                            {
                                Parameter::Lifetime { lifetime_token }
                            } else {
                                let name_token = expect_token!(lexer, TokenKind::Name(_))?;
                                if let Some(colon_token) = eat_token!(lexer, TokenKind::Colon) {
                                    Parameter::Value {
                                        name_token,
                                        colon_token,
                                        typ: Box::new(parse_expression(lexer, true)?),
                                    }
                                } else {
                                    Parameter::Type { name_token }
                                }
                            },
                        );

                        if let Some(close_parenthesis_token) =
                            eat_token!(lexer, TokenKind::CloseParenthesis)
                        {
                            break close_parenthesis_token;
                        }
                        expect_token!(lexer, TokenKind::Comma)?;
                    };

                    Parameters {
                        open_parenthesis_token,
                        parameters: parameters.into_boxed_slice(),
                        close_parenthesis_token,
                    }
                },
                return_type: if let Some(right_arrow_token) =
                    eat_token!(lexer, TokenKind::RightArrow)
                {
                    Some(Box::new(ReturnType {
                        right_arrow_token,
                        typ: parse_expression(lexer, false)?,
                    }))
                } else {
                    None
                },
                body: if let Some(open_brace_token) = eat_token!(lexer, TokenKind::OpenBrace) {
                    Some(Box::new(parse_block(lexer, open_brace_token)?))
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
                        while eat_token!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }

                        members.push(Member {
                            name_token: expect_token!(lexer, TokenKind::Name(_))?,
                            colon_token: expect_token!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer, true)?,
                        });

                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }
                        expect_token!(lexer, TokenKind::Comma)?;
                    };

                    Members {
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        enum_token @ Token {
            location,
            kind: TokenKind::EnumKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Enum {
                enum_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                members: {
                    let open_brace_token = expect_token!(lexer, TokenKind::OpenBrace)?;

                    let mut members = vec![];
                    let close_brace_token = loop {
                        while eat_token!(lexer, TokenKind::Newline).is_some() {}
                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }

                        members.push(Member {
                            name_token: expect_token!(lexer, TokenKind::Name(_))?,
                            colon_token: expect_token!(lexer, TokenKind::Colon)?,
                            typ: parse_expression(lexer, true)?,
                        });

                        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                            break close_brace_token;
                        }
                        expect_token!(lexer, TokenKind::Comma)?;
                    };

                    Members {
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    }
                },
            },
        },

        type_token @ Token {
            location,
            kind: TokenKind::TypeKeyword,
        } => Item {
            attributes,
            location,
            kind: ItemKind::Type {
                type_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                equals_type: if let Some(equals_token) = eat_token!(lexer, TokenKind::Equals) {
                    Some(Box::new(EqualsType {
                        equals_token,
                        typ: parse_expression(lexer, true)?,
                    }))
                } else {
                    None
                },
            },
        },

        Token {
            location,
            kind: TokenKind::ConstKeyword,
        } => todo!("unimplemented const syntax at {location}"),

        Token {
            location,
            kind: TokenKind::UsingKeyword,
        } => todo!("unimplemented using syntax at {location}"),

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::ExpectedItemButGot(token),
            });
        }
    })
}

pub fn parse_statement(lexer: &mut Lexer) -> Result<Statement, ParsingError> {
    Ok(match lexer.peek_token()?.kind {
        TokenKind::Hash
        | TokenKind::FnKeyword
        | TokenKind::StructKeyword
        | TokenKind::EnumKeyword
        | TokenKind::TypeKeyword
        | TokenKind::ConstKeyword
        | TokenKind::UsingKeyword => {
            let item = Box::new(parse_item(lexer)?);
            Statement {
                location: item.location,
                kind: StatementKind::Item(item),
            }
        }

        _ => {
            let expression = Box::new(parse_expression(lexer, true)?);
            if let Some(equals_token) = eat_token!(lexer, TokenKind::Equals) {
                Statement {
                    location: equals_token.location,
                    kind: StatementKind::Assignment {
                        pattern: expression,
                        equals_token,
                        value: Box::new(parse_expression(lexer, true)?),
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

pub fn parse_expression(
    lexer: &mut Lexer,
    optional_open_brace_allowed: bool,
) -> Result<Expression, ParsingError> {
    parse_ary_expression(lexer, 0, optional_open_brace_allowed)
}

fn parse_ary_expression(
    lexer: &mut Lexer,
    parent_precedence: usize,
    optional_open_brace_allowed: bool,
) -> Result<Expression, ParsingError> {
    let mut left = if let Some(unary_precendence) = match lexer.peek_token()?.kind {
        TokenKind::Plus | TokenKind::Minus => Some(usize::MAX),
        _ => None,
    } {
        let operator_token = lexer.next_token()?;
        Expression {
            location: operator_token.location,
            kind: ExpressionKind::UnaryOperator {
                operator_token,
                operand: Box::new(parse_ary_expression(
                    lexer,
                    unary_precendence,
                    optional_open_brace_allowed,
                )?),
            },
        }
    } else {
        parse_primary_expression(lexer, optional_open_brace_allowed)?
    };

    while let Some(binary_precedence) = match lexer.peek_token()?.kind {
        TokenKind::Asterisk | TokenKind::Slash => Some(2),
        TokenKind::Plus | TokenKind::Minus => Some(1),
        _ => None,
    } && binary_precedence > parent_precedence
    {
        let operator_token = lexer.next_token()?;
        left = Expression {
            location: operator_token.location,
            kind: ExpressionKind::BinaryOperator {
                left: Box::new(left),
                operator_token,
                right: Box::new(parse_ary_expression(
                    lexer,
                    binary_precedence,
                    optional_open_brace_allowed,
                )?),
            },
        };
    }

    Ok(left)
}

fn parse_primary_expression(
    lexer: &mut Lexer,
    optional_open_brace_allowed: bool,
) -> Result<Expression, ParsingError> {
    let mut expression = match lexer.next_token()? {
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
            kind: TokenKind::CloseParenthesis,
        } => Expression {
            location,
            kind: ExpressionKind::ParenthesizedExpression {
                open_parenthesis_token,
                expression: Box::new(parse_expression(lexer, true)?),
                close_parenthesis_token: expect_token!(lexer, TokenKind::CloseParenthesis)?,
            },
        },

        let_token @ Token {
            location,
            kind: TokenKind::LetKeyword,
        } => Expression {
            location,
            kind: ExpressionKind::Let {
                let_token,
                name_token: expect_token!(lexer, TokenKind::Name(_))?,
                typ: if let Some(colon_token) = eat_token!(lexer, TokenKind::Colon) {
                    Some(Box::new(ColonType {
                        colon_token,
                        typ: parse_expression(lexer, optional_open_brace_allowed)?,
                    }))
                } else {
                    None
                },
            },
        },

        token => {
            return Err(ParsingError {
                location: token.location,
                kind: ParsingErrorKind::ExpectedExpressionButGot(token),
            });
        }
    };

    loop {
        expression = match lexer.peek_token()?.kind {
            TokenKind::OpenParenthesis => {
                let open_parenthesis_token = expect_token!(lexer, TokenKind::OpenParenthesis)?;

                let mut arguments = vec![];
                let close_parenthesis_token = loop {
                    while eat_token!(lexer, TokenKind::Newline).is_some() {}
                    if let Some(close_parenthesis_token) =
                        eat_token!(lexer, TokenKind::CloseParenthesis)
                    {
                        break close_parenthesis_token;
                    }

                    arguments.push(
                        if let Some(lifetime_token) = eat_token!(lexer, TokenKind::Lifetime(_)) {
                            Argument::Lifetime { lifetime_token }
                        } else {
                            Argument::ValueOrType(parse_expression(lexer, true)?)
                        },
                    );

                    if let Some(close_parenthesis_token) =
                        eat_token!(lexer, TokenKind::CloseParenthesis)
                    {
                        break close_parenthesis_token;
                    }
                    expect_token!(lexer, TokenKind::Comma)?;
                };

                Expression {
                    location: open_parenthesis_token.location,
                    kind: ExpressionKind::Call {
                        operand: Box::new(expression),
                        open_parenthesis_token,
                        arguments: arguments.into_boxed_slice(),
                        close_parenthesis_token,
                    },
                }
            }

            TokenKind::OpenBrace if optional_open_brace_allowed => {
                let open_brace_token = expect_token!(lexer, TokenKind::OpenBrace)?;

                let mut members = vec![];
                let close_brace_token = loop {
                    while eat_token!(lexer, TokenKind::Newline).is_some() {}
                    if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                        break close_brace_token;
                    }

                    members.push(ConstructorArgument {
                        name_token: expect_token!(lexer, TokenKind::Name(_))?,
                        colon_token: expect_token!(lexer, TokenKind::Colon)?,
                        value: parse_expression(lexer, true)?,
                    });

                    if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
                        break close_brace_token;
                    }
                    expect_token!(lexer, TokenKind::Comma)?;
                };

                Expression {
                    location: open_brace_token.location,
                    kind: ExpressionKind::Constructor {
                        typ: Box::new(expression),
                        open_brace_token,
                        members: members.into_boxed_slice(),
                        close_brace_token,
                    },
                }
            }

            TokenKind::Dot => {
                let dot_token = expect_token!(lexer, TokenKind::Dot)?;
                let name_token = expect_token!(lexer, TokenKind::Name(_))?;
                Expression {
                    location: dot_token.location,
                    kind: ExpressionKind::MemberAccess {
                        operand: Box::new(expression),
                        dot_token,
                        name_token,
                    },
                }
            }

            TokenKind::ColonColon => {
                let colon_colon_token = expect_token!(lexer, TokenKind::ColonColon)?;
                let name_token = expect_token!(lexer, TokenKind::Name(_))?;
                Expression {
                    location: colon_colon_token.location,
                    kind: ExpressionKind::PathAccess {
                        operand: Box::new(expression),
                        colon_colon_token,
                        name_token,
                    },
                }
            }

            _ => break,
        };
    }

    Ok(expression)
}

fn parse_block(lexer: &mut Lexer, open_brace_token: Token) -> Result<Expression, ParsingError> {
    let mut statements = vec![];

    let close_brace_token = loop {
        while eat_token!(lexer, TokenKind::Newline).is_some() {}
        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
            break close_brace_token;
        }

        statements.push(parse_statement(lexer)?);

        if let Some(close_brace_token) = eat_token!(lexer, TokenKind::CloseBrace) {
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
