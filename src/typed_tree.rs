use crate::{
    idvec::{IdSlice, IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
    type_inference_tree::{BuiltinFunctionBody, FunctionId, IntegerTypeKind},
};
use std::fmt::Display;

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
    Place(Box<Place>),
    Integer(IntegerValue),
    StructConstructor {
        members: Box<[ConstructorMember]>,
    },
    EnumConstructor {
        member: Box<ConstructorMember>,
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

#[derive(Debug, Clone, Copy)]
pub enum IntegerValue {
    I64(i64),
}

#[derive(Debug)]
pub struct ConstructorMember {
    pub location: SourceLocation,
    pub member_index: usize,
    pub value: Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Identity,
    NegateI64,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    AddI64,
    SubtractI64,
    MultiplyI64,
    DivideI64,
}

#[derive(Debug)]
pub struct Place {
    pub location: SourceLocation,
    pub typ: TypeId,
    pub kind: PlaceKind,
}

#[derive(Debug)]
pub enum PlaceKind {
    Variable(VariableId),
    Function(FunctionId),
    Expression(Box<Expression>),
    StructMemberAccess {
        operand: Box<Place>,
        member_index: usize,
    },
}

#[derive(Debug)]
pub struct Pattern {
    pub location: SourceLocation,
    pub typ: TypeId,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Place(Box<Place>),
    Integer(IntegerValue),
    StructDeconstructor { members: Box<[DeconstructorMember]> },
    EnumDeconstructor { member: Box<DeconstructorMember> },
    Let(VariableId),
}

#[derive(Debug)]
pub struct DeconstructorMember {
    pub location: SourceLocation,
    pub member_index: usize,
    pub pattern: Pattern,
}

pub struct PrettyPrintType<'a> {
    pub typ: TypeId,
    pub types: &'a IdSlice<TypeId, Type>,
}

impl Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let typ = &self.types[self.typ];
        match typ.kind {
            TypeKind::Resolving => write!(f, "_"),
            TypeKind::Runtime => write!(f, "Runtime"),
            TypeKind::Integer(ref integer_type) => match *integer_type {
                IntegerTypeKind::I64 => write!(f, "I64"),
            },
            TypeKind::FunctionItem(_) => {
                write!(f, "{{{{function item for '{}'}}}}", typ.name.unwrap())
            }
            TypeKind::Struct { members: _ } => {
                write!(f, "{}", typ.name.unwrap())
            }
            TypeKind::Enum { members: _ } => {
                write!(f, "{}", typ.name.unwrap())
            }
            TypeKind::Generic => write!(f, "{}", typ.name.unwrap()),
        }
    }
}
