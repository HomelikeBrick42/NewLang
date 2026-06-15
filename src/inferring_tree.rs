use crate::{interning::InternedStr, lexer::SourceLocation};
use enum_map::{Enum, EnumMap};
use slotmap::{SecondaryMap, SlotMap, new_key_type};

#[derive(Debug)]
pub struct Program {
    pub functions: SlotMap<FunctionId, Function>,
    pub function_bodies: SecondaryMap<FunctionId, FunctionBody>,
    pub types: SlotMap<TypeId, Type>,
    pub builtin_types: EnumMap<BuiltinType, Option<TypeId>>,
}

#[derive(Debug, Enum)]
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
    Builtin(BuiltinFunctionBody),
}

#[derive(Debug)]
pub enum BuiltinFunctionBody {
    PrintI64,
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
    pub typ: TypeId,
    pub kind: PatternKind,
}

#[derive(Debug)]
pub enum PatternKind {
    Place(Box<Place>),
    Integer(u64),
    Let(VariableId),
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
    Infer,
    Inferred(TypeId),
    Unit,
    Runtime,
    I64,
    Function(FunctionId),
}
