use crate::lexer::{SourceLocation, Token};

#[derive(Debug)]
pub struct Attribute {
    pub location: SourceLocation,
    pub hash_token: Token,
    pub open_bracket_token: Token,
    pub kind: AttributeKind,
    pub close_bracket_token: Token,
}

#[derive(Debug)]
pub enum AttributeKind {
    Builtin { builtin_token: Token },
}

#[derive(Debug)]
pub struct Item {
    pub attributes: Box<[Attribute]>,
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Type {
        type_token: Token,
        name_token: Token,
        equals_type: Option<Box<EqualsType>>,
    },
    Struct {
        struct_token: Token,
        name_token: Token,
        members: Members,
    },
    Function {
        fn_token: Token,
        name_token: Token,
        parameters: Parameters,
        return_type: Option<Box<ReturnType>>,
        body: Option<Box<Expression>>,
    },
}

#[derive(Debug)]
pub struct EqualsType {
    pub equal_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct Members {
    pub open_brace_token: Token,
    pub members: Box<[Member]>,
    pub close_brace_token: Token,
}

#[derive(Debug)]
pub struct Member {
    pub name_token: Token,
    pub colon_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct Parameters {
    pub open_parenthesis_token: Token,
    pub parameters: Box<[Parameter]>,
    pub close_parenthesis_token: Token,
}

#[derive(Debug)]
pub struct Parameter {
    pub location: SourceLocation,
    pub kind: ParameterKind,
}

#[derive(Debug)]
pub enum ParameterKind {
    Value {
        name_token: Token,
        colon_token: Token,
        typ: Expression,
    },
}

#[derive(Debug)]
pub struct ReturnType {
    pub right_arrow_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Item(Box<Item>),
    Expression(Box<Expression>),
    Assignment {
        pattern: Box<Expression>,
        equal_token: Token,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    ParenthesisedExpression {
        open_parenthesis_token: Token,
        expression: Box<Expression>,
        close_parenthesis_token: Token,
    },
    Block {
        open_brace_token: Token,
        statements: Box<[Statement]>,
        close_brace_token: Token,
    },
    Name {
        name_token: Token,
    },
    Integer {
        integer_token: Token,
    },
    Call {
        operand: Box<Expression>,
        open_parenthesis_token: Token,
        arguments: Box<[Argument]>,
        close_parenthesis_token: Token,
    },
    Let {
        let_token: Token,
        name_token: Token,
        colon_type: Option<Box<ColonType>>,
    },
    Constructor {
        typ: Box<Expression>,
        members: Members,
    },
}

#[derive(Debug)]
pub enum Argument {
    Value { expression: Expression },
}

#[derive(Debug)]
pub struct ColonType {
    pub colon_token: Token,
    pub typ: Expression,
}
