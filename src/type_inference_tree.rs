use crate::{
    ast::{BinaryOperator, UnaryOperator},
    idvec::{IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
};
use rustc_hash::FxHashMap;

new_id_type!(pub struct FunctionId);

#[derive(Debug)]
pub struct FunctionSignature {
    pub name: Option<InternedStr>,
    pub parameters: Box<[FunctionParameter]>,
    pub return_type: TypeId,
    pub typ: TypeId,
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub location: SourceLocation,
    pub kind: FunctionParameterKind,
}

#[derive(Debug)]
pub enum FunctionParameterKind {
    Value { name: InternedStr, typ: TypeId },
}

#[derive(Debug)]
pub enum FunctionBody {
    Builtin(BuiltinFunctionBody),
    Expression {
        variables: IdVec<VariableId, Variable>,
        parameter_variables: Box<[Option<VariableId>]>,
        expression: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum BuiltinFunctionBody {
    PrintI64,
}

new_id_type!(pub struct VariableId);

#[derive(Debug)]
pub struct Variable {
    pub name: Option<InternedStr>,
    pub typ: TypeId,
}

new_id_type!(pub struct TypeId);

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Resolving,

    Infer(InferTypeKind),
    Inferred(TypeId),

    Runtime,
    Integer(IntegerTypeKind),
    FunctionItem(FunctionId),
    Struct { members: Box<[TypeMember]> },
    Enum { members: Box<[TypeMember]> },
    Generic,
}

#[derive(Debug)]
pub enum InferTypeKind {
    Anything,
    Number,
    StructLike {
        members: FxHashMap<InternedStr, TypeId>,
    },
}

#[derive(Debug)]
pub enum IntegerTypeKind {
    I64,
}

#[derive(Debug, Clone)]
pub struct TypeMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: TypeId,
}

#[derive(Debug)]
pub struct Statement {
    pub location: SourceLocation,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Expression(Box<Expression>),
    Assignment {
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub location: SourceLocation,
    pub typ: TypeId,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Variable(VariableId),
    Function(FunctionId),
    Integer(u128),
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Constructor {
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
    pub location: SourceLocation,
    pub name: InternedStr,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Argument {
    pub location: SourceLocation,
    pub kind: ArgumentKind,
}

#[derive(Debug)]
pub enum ArgumentKind {
    Value(Box<Expression>),
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub typ: TypeId,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Variable(VariableId),
    Function(FunctionId),
    Integer(u128),
    Deconstructor {
        members: Box<[DeconstructorMember]>,
    },
    MemberAccess {
        operand: Box<Expression>,
        name: InternedStr,
    },
    Let(VariableId),
}

#[derive(Debug)]
pub struct DeconstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub pattern: Pattern,
}
