use crate::{ast, interning::InternedStr, lexer::SourceLocation};
use enum_map::{Enum, EnumMap};
use rustc_hash::FxHashMap;
use slotmap::{SecondaryMap, SlotMap, new_key_type};
use std::fmt::Display;

#[derive(Debug)]
pub struct Program {
    pub functions: SlotMap<FunctionId, Function>,
    pub function_bodies: SecondaryMap<FunctionId, FunctionBody>,
    pub types: SlotMap<TypeId, Type>,
    pub builtin_types: EnumMap<BuiltinType, Option<TypeId>>,
}

#[derive(Debug, Clone, Copy, Enum)]
pub enum BuiltinType {
    Unit,
    Runtime,
    I64,
}

new_key_type! {
    pub struct FunctionId;
}

#[derive(Debug)]
pub struct Function {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub parameters: Box<[Parameter]>,
    pub return_type: TypeId,
    pub function_type: TypeId,
}

#[derive(Debug)]
pub struct Parameter {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub kind: ParameterKind,
}

#[derive(Debug)]
pub enum ParameterKind {
    Value { typ: TypeId },
}

#[derive(Debug)]
pub enum FunctionBody {
    Expression {
        variables: SlotMap<VariableId, Variable>,
        parameter_variables: Box<[Option<VariableId>]>,
        expression: Box<Expression>,
    },
    Builtin(ast::BuiltinFunctionBody),
}

new_key_type! {
    pub struct VariableId;
}

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
    Integer(u64),
    Block {
        end_location: SourceLocation,
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Argument]>,
    },
    Constructor {
        members: Box<[ConstructorMember]>,
    },
}

#[derive(Debug)]
pub enum Argument {
    Value { expression: Expression },
}

#[derive(Debug)]
pub struct ConstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub value: Expression,
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
    Integer(u64),
    Deconstructor { members: Box<[DeconstructorMember]> },
}

#[derive(Debug)]
pub struct DeconstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub pattern: Pattern,
}

#[derive(Debug)]
pub struct Place {
    pub location: SourceLocation,
    pub typ: TypeId,
    pub kind: PlaceKind,
}

#[derive(Debug)]
pub enum PlaceKind {
    Function(FunctionId),
    Variable(VariableId),
    Let(VariableId),
}

new_key_type! {
    pub struct TypeId;
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    Resolving,
    Infer(InferTypeKind),
    Inferred(TypeId),
    Unit,
    Runtime,
    I64,
    Struct {
        name: InternedStr,
        members: Box<[TypeMember]>,
    },
    FunctionItem {
        function: FunctionId,
        parameters: Box<[ParameterType]>,
        return_type: TypeId,
    },
}

#[derive(Debug)]
pub enum InferTypeKind {
    Anything,
    StructLike {
        members: FxHashMap<InternedStr, TypeId>,
    },
    FunctionLike {
        parameters: Box<[ParameterType]>,
        return_type: TypeId,
    },
}

#[derive(Debug, Clone)]
pub struct TypeMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: TypeId,
}

#[derive(Debug, Clone)]
pub enum ParameterType {
    Value { typ: TypeId },
}

pub struct PrettyPrintType<'a> {
    pub id: TypeId,
    pub types: &'a SlotMap<TypeId, Type>,
}

impl Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.types[self.id].kind {
            TypeKind::Resolving => write!(f, "{{resolving}}"),
            TypeKind::Infer(ref infer_type_kind) => match *infer_type_kind {
                InferTypeKind::Anything => write!(f, "_"),
                InferTypeKind::StructLike { ref members } => {
                    write!(f, "_ {{ ")?;
                    for (i, (&name, &typ)) in members.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(
                            f,
                            "{name}: {}",
                            PrettyPrintType {
                                id: typ,
                                types: self.types,
                            },
                        )?;
                    }
                    write!(f, " }}")
                }
                InferTypeKind::FunctionLike {
                    ref parameters,
                    return_type,
                } => {
                    write!(f, "fn(")?;
                    for (i, parameter) in parameters.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        match *parameter {
                            ParameterType::Value { typ } => write!(
                                f,
                                "_: {}",
                                PrettyPrintType {
                                    id: typ,
                                    types: self.types,
                                },
                            )?,
                        }
                    }
                    write!(
                        f,
                        ") -> {}",
                        PrettyPrintType {
                            id: return_type,
                            types: self.types,
                        },
                    )
                }
            },
            TypeKind::Inferred(id) => write!(
                f,
                "{}",
                PrettyPrintType {
                    id,
                    types: self.types,
                },
            ),
            TypeKind::Unit => write!(f, "Unit"),
            TypeKind::Runtime => write!(f, "Runtime"),
            TypeKind::I64 => write!(f, "I64"),
            TypeKind::Struct { name, ref members } => {
                write!(f, "{name} {{ ")?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}: {}",
                        member.name,
                        PrettyPrintType {
                            id: member.typ,
                            types: self.types,
                        },
                    )?;
                }
                write!(f, " }}")
            }
            TypeKind::FunctionItem {
                function: _,
                ref parameters,
                return_type,
            } => {
                write!(f, "fn_item(")?;
                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match *parameter {
                        ParameterType::Value { typ } => write!(
                            f,
                            "_: {}",
                            PrettyPrintType {
                                id: typ,
                                types: self.types,
                            },
                        )?,
                    }
                }
                write!(
                    f,
                    ") -> {}",
                    PrettyPrintType {
                        id: return_type,
                        types: self.types,
                    },
                )
            }
        }
    }
}
