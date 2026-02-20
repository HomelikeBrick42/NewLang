use crate::lexing::{SourceLocation, Token};

#[derive(Debug)]
pub struct Attribute {
    pub hash_token: Token,
    pub open_bracket_token: Token,
    pub name_token: Token,
    pub close_bracket_token: Token,
}

#[derive(Debug)]
pub struct Item {
    pub attributes: Box<[Attribute]>,
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Module {
        module_token: Token,
        name_token: Token,
        items: Box<[Item]>,
    },
    Fn {
        fn_token: Token,
        name_token: Token,
        parameters: Parameters,
        return_type: Option<Box<ReturnType>>,
        body: Option<Box<Expression>>,
    },
    Struct {
        struct_token: Token,
        name_token: Token,
        members: Members,
    },
    Enum {
        enum_token: Token,
        name_token: Token,
        members: Members,
    },
    Type {
        type_token: Token,
        name_token: Token,
        equals_type: Option<Box<EqualsType>>,
    },
    Const {
        const_token: Token,
        name_token: Token,
        colon_type: Box<ColonType>,
        equals_value: Option<Box<EqualsValue>>,
    },
}

#[derive(Debug)]
pub struct Parameters {
    pub open_parenthesis_token: Token,
    pub parameters: Box<[Parameter]>,
    pub close_parenthesis_token: Token,
}

#[derive(Debug)]
pub enum Parameter {
    Value {
        name_token: Token,
        colon_token: Token,
        typ: Box<Expression>,
    },
    Type {
        name_token: Token,
    },
    Lifetime {
        lifetime_token: Token,
    },
}

#[derive(Debug)]
pub struct ReturnType {
    pub right_arrow_token: Token,
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
pub struct EqualsType {
    pub equals_token: Token,
    pub typ: Expression,
}

#[derive(Debug)]
pub struct EqualsValue {
    pub equals_token: Token,
    pub value: Expression,
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
        equals_token: Token,
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
    ParenthesizedExpression {
        open_parenthesis_token: Token,
        expression: Box<Expression>,
        close_parenthesis_token: Token,
    },
    Name {
        name_token: Token,
    },
    PathAccess {
        operand: Box<Expression>,
        colon_colon_token: Token,
        name_token: Token,
    },
    Integer {
        integer_token: Token,
    },
    Block {
        open_brace_token: Token,
        statements: Box<[Statement]>,
        close_brace_token: Token,
    },
    Constructor {
        typ: Box<Expression>,
        open_brace_token: Token,
        members: Box<[ConstructorArgument]>,
        close_brace_token: Token,
    },
    UnaryOperator {
        operator_token: Token,
        operand: Box<Expression>,
    },
    BinaryOperator {
        left: Box<Expression>,
        operator_token: Token,
        right: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        open_parenthesis_token: Token,
        arguments: Box<[Argument]>,
        close_parenthesis_token: Token,
    },
    MemberAccess {
        operand: Box<Expression>,
        dot_token: Token,
        name_token: Token,
    },
    Let {
        let_token: Token,
        name_token: Token,
        typ: Option<Box<ColonType>>,
    },
}

#[derive(Debug)]
pub struct ConstructorArgument {
    pub name_token: Token,
    pub colon_token: Token,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Argument {
    ValueOrType(Expression),
    Lifetime { lifetime_token: Token },
}

#[derive(Debug)]
pub struct ColonType {
    pub colon_token: Token,
    pub typ: Expression,
}
