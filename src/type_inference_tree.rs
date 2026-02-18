use std::fmt::Display;

use crate::{
    ast::{BinaryOperator, UnaryOperator},
    idvec::{IdSlice, IdVec, new_id_type},
    interning::InternedStr,
    lexing::SourceLocation,
};
use rustc_hash::FxHashMap;

new_id_type!(pub struct FunctionId);

#[derive(Debug)]
pub struct FunctionSignature {
    pub location: SourceLocation,
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

#[derive(Debug, Clone, Copy)]
pub enum BuiltinFunctionBody {
    PrintI64,
}

new_id_type!(pub struct VariableId);

#[derive(Debug)]
pub struct Variable {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
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
    MemberAccess {
        operand: Box<Place>,
        name: InternedStr,
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
    Integer(u128),
    Deconstructor {
        members: Box<[DeconstructorMember]>,
    },
    Let(VariableId),
}

#[derive(Debug)]
pub struct DeconstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub pattern: Pattern,
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
    FunctionLike {
        parameters: Box<[InferFunctionParameter]>,
        return_type: TypeId,
    },
    StructLike {
        members: FxHashMap<InternedStr, TypeId>,
    },
}

#[derive(Debug, Clone)]
pub enum InferFunctionParameter {
    Value { typ: TypeId },
}

#[derive(Debug, Clone, Copy)]
pub enum IntegerTypeKind {
    I64,
}

#[derive(Debug, Clone)]
pub struct TypeMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: TypeId,
}

pub struct PrettyPrintType<'a> {
    pub typ: TypeId,
    pub types: &'a IdSlice<TypeId, Type>,
}

impl Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut typ = &self.types[self.typ];
        loop {
            break match typ.kind {
                TypeKind::Resolving => write!(f, "_"),
                TypeKind::Infer(ref infer_type_kind) => match *infer_type_kind {
                    InferTypeKind::Anything => write!(f, "_"),
                    InferTypeKind::Number => write!(f, "{{{{number}}}}"),
                    InferTypeKind::FunctionLike {
                        ref parameters,
                        return_type,
                    } => {
                        write!(f, "_(")?;
                        for (i, parameter) in parameters.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            match *parameter {
                                InferFunctionParameter::Value { typ } => write!(
                                    f,
                                    " _: {}",
                                    PrettyPrintType {
                                        typ,
                                        types: self.types
                                    }
                                )?,
                            }
                        }
                        write!(
                            f,
                            ") -> {}",
                            PrettyPrintType {
                                typ: return_type,
                                types: self.types
                            }
                        )
                    }
                    InferTypeKind::StructLike { ref members } => {
                        write!(f, "_ {{")?;
                        for (i, (&name, &typ)) in members.iter().enumerate() {
                            if i > 0 {
                                write!(f, ",")?;
                            }
                            write!(
                                f,
                                " {name}: {}",
                                PrettyPrintType {
                                    typ,
                                    types: self.types
                                }
                            )?;
                        }
                        if !members.is_empty() {
                            write!(f, " ")?;
                        }
                        write!(f, "}}")
                    }
                },
                TypeKind::Inferred(id) => {
                    typ = &self.types[id];
                    continue;
                }
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
            };
        }
    }
}
