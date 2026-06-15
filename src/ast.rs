use crate::{interning::InternedStr, lexer::SourceLocation};

#[derive(Debug)]
pub struct Item {
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Type {
        name: InternedStr,
        typ: Type,
    },
    Function {
        name: InternedStr,
        parameters: Box<[Parameter]>,
        return_type: Type,
        body: FunctionBody,
    },
}

#[derive(Debug)]
pub struct Parameter {
    pub location: SourceLocation,
    pub kind: ParameterKind,
}

#[derive(Debug)]
pub enum ParameterKind {
    Value { name: InternedStr, typ: Box<Type> },
}

#[derive(Debug)]
pub enum FunctionBody {
    Expression(Box<Expression>),
    Builtin(BuiltinFunctionBody),
}

#[derive(Debug)]
pub enum BuiltinFunctionBody {
    PrintI64,
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
        pattern: Box<Pattern>,
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
    Place(Box<Place>),
    Integer(u64),
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Argument]>,
    },
}

#[derive(Debug)]
pub enum Argument {
    Value { expression: Expression },
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Place(Box<Place>),
    Integer(u64),
    Let { name: InternedStr, typ: Box<Type> },
}

#[derive(Debug)]
pub struct Place {
    pub location: SourceLocation,
    pub kind: PlaceKind,
}

#[derive(Debug)]
pub enum PlaceKind {
    Name(InternedStr),
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Infer,
    Name(InternedStr),
    DeclareBuiltin(BuiltinType),
    Builtin(BuiltinType),
}

#[derive(Debug)]
pub enum BuiltinType {
    Unit,
    Runtime,
    I64,
}
