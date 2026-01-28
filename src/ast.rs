use crate::{interning::InternedStr, lexing::SourceLocation};

#[derive(Debug)]
pub struct Item {
    pub location: SourceLocation,
    pub builtin: bool,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    Module {
        name: InternedStr,
        items: Box<[Item]>,
    },
    Fn {
        name: InternedStr,
        parameters: Box<[Parameter]>,
        return_type: Option<Box<Type>>,
        body: Option<Box<Expression>>,
    },
    Struct {
        name: InternedStr,
        members: Box<[Member]>,
    },
    Enum {
        name: InternedStr,
        members: Box<[Member]>,
    },
    Type {
        name: InternedStr,
        typ: Option<Box<Type>>,
    },
}

#[derive(Debug)]
pub enum Parameter {
    Value { name: InternedStr, typ: Type },
    Type { name: InternedStr },
    Lifetime { name: InternedStr },
}

#[derive(Debug)]
pub struct Member {
    pub name: InternedStr,
    pub typ: Type,
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
    Path(Box<Path>),
    Integer(u128),
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Constructor {
        typ: Box<Type>,
        members: Box<[ConstructorMember]>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Argument]>,
    },
    MemberAccess {
        operand: Box<Expression>,
        name: InternedStr,
    },
}

#[derive(Debug)]
pub struct ConstructorMember {
    pub name: InternedStr,
    pub value: Expression,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Plus,
    Negate,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum Argument {
    ValueOrType(Expression),
    Lifetime { name: InternedStr },
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Path(Box<Path>),
    Integer(u128),
    Destructor {
        typ: Box<Type>,
        members: Box<[DestructorMember]>,
    },
    Let {
        name: InternedStr,
        typ: Option<Box<Type>>,
    },
}

#[derive(Debug)]
pub struct DestructorMember {
    pub name: InternedStr,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Path(Box<Path>),
    Unit,
}

#[derive(Debug)]
pub struct Path {
    pub location: SourceLocation,
    pub kind: PathKind,
}

#[derive(Debug)]
pub enum PathKind {
    Name {
        name: InternedStr,
    },
    PathAccess {
        operand: Box<Path>,
        name: InternedStr,
    },
}
