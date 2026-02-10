use crate::{
    idvec::{IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
    type_inference_tree::{BuiltinFunctionBody, FunctionId, IntegerTypeKind},
};

#[derive(Debug)]
pub struct FunctionSignature {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub value_parameters: Box<[TypeId]>,
    pub return_type: TypeId,
    pub typ: TypeId,
}

#[derive(Debug)]
pub enum FunctionBody {
    Builtin(BuiltinFunctionBody),
    Expression {
        variables: IdVec<VariableId, Variable>,
        value_parameter_variables: Box<[VariableId]>,
        expression: Box<Expression>,
    },
}

new_id_type!(pub struct VariableId);

#[derive(Debug)]
pub struct Variable {
    pub location: SourceLocation,
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
    Runtime,
    Integer(IntegerTypeKind),
    FunctionItem(FunctionId),
    Struct { members: Box<[TypeMember]> },
    Enum { members: Box<[TypeMember]> },
    Generic,
}

#[derive(Debug)]
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
    Integer(IntegerValue),
    StructConstructor {
        members: Box<[ConstructorMember]>,
    },
    EnumConstructor {
        member: Box<ConstructorMember>,
    },
    StructMemberAccess {
        operand: Box<Expression>,
        member_index: usize,
    },
    Block {
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
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
        value_arguments: Box<[Expression]>,
    },
}

#[derive(Debug)]
pub enum IntegerValue {
    I64(i64),
}

#[derive(Debug)]
pub struct ConstructorMember {
    pub location: SourceLocation,
    pub member_index: usize,
    pub value: Expression,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Identity,
    NegateI64,
}

#[derive(Debug)]
pub enum BinaryOperator {
    AddI64,
    SubtractI64,
    MultiplyI64,
    DivideI64,
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
    Integer(IntegerValue),
    StructDeconstructor {
        members: Box<[DeconstructorMember]>,
    },
    EnumDeconstructor {
        member: Box<DeconstructorMember>,
    },
    StructMemberAccess {
        operand: Box<Expression>,
        member_index: usize,
    },
    Let(VariableId),
}

#[derive(Debug)]
pub struct DeconstructorMember {
    pub location: SourceLocation,
    pub member_index: usize,
    pub pattern: Pattern,
}
