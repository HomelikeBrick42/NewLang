use crate::{ast::BuiltinFunctionBody, interning::InternedStr, lexer::SourceLocation};
use slotmap::{SlotMap, new_key_type};
use std::fmt::Debug;

new_key_type! {
    pub struct TypeAliasId;
    pub struct StructId;
    pub struct GenericTypeId;
    pub struct GenericDynId;
    pub struct ParameterizedTypeId;
    pub struct ParameterizedDynId;
    pub struct FunctionId;
    pub struct VariableId;
}

pub struct ResolvedProgram {
    pub type_aliases: SlotMap<TypeAliasId, TypeAlias>,
    pub structs: SlotMap<StructId, Struct>,
    pub generic_types: SlotMap<GenericTypeId, GenericType>,
    pub generic_dyns: SlotMap<GenericDynId, GenericDyn>,
    pub parameterized_types: SlotMap<ParameterizedTypeId, ParameterizedType>,
    pub parameterized_dyns: SlotMap<ParameterizedDynId, ParameterizedDyn>,
    pub functions: SlotMap<FunctionId, Function>,
    pub variables: SlotMap<VariableId, Variable>,

    pub unit_type: Option<StructId>,
}

#[derive(Debug)]
pub struct TypeAlias {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Struct {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub members: Box<[StructMember]>,
}

#[derive(Debug)]
pub struct StructMember {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub typ: Type,
}

#[derive(Debug)]
pub struct GenericType {
    pub location: SourceLocation,
    pub name: InternedStr,
}

#[derive(Debug)]
pub struct GenericDyn {
    pub location: SourceLocation,
    pub name: InternedStr,
}

#[derive(Debug)]
pub struct ParameterizedType {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub parameters: Box<[Parameter]>,
    pub underlying_type: UnderlyingType,
}

#[derive(Debug)]
pub enum UnderlyingType {
    TypeAlias(TypeAliasId),
    Struct(StructId),
    GenericType(GenericTypeId),
}

#[derive(Debug)]
pub struct ParameterizedDyn {
    pub location: SourceLocation,
    pub name: InternedStr,
    pub parameters: Box<[Parameter]>,
    pub underlying_dyn: UnderlyingDyn,
}

#[derive(Debug)]
pub enum UnderlyingDyn {
    GenericDyn(GenericDynId),
}

#[derive(Debug)]
pub struct Function {
    pub location: SourceLocation,
    pub is_unsafe: bool,
    pub name: Option<InternedStr>,
    pub parameters: Box<[Parameter]>,
    pub return_type: Type,
    pub body: FunctionBody,
}

#[derive(Debug)]
pub enum FunctionBody {
    Builtin(BuiltinFunctionBody),
    Expression(Box<Expression>),
}

#[derive(Debug)]
pub struct Parameter {
    pub location: SourceLocation,
    pub kind: ParameterKind,
}

#[derive(Debug)]
pub enum ParameterKind {
    Value(VariableId),
    Type(GenericTypeId),
    ParameterizedType(ParameterizedTypeId),
    Dyn(GenericDynId),
    ParameterizedDyn(ParameterizedDynId),
}

#[derive(Debug)]
pub struct Variable {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Type {
    pub location: SourceLocation,
    pub kind: TypeKind,
}

#[derive(Debug)]
pub enum TypeKind {
    TypeAlias(TypeAliasId),
    Struct(StructId),
    Generic(GenericTypeId),
    Instantiation {
        parameterized_type: ParameterizedTypeId,
        arguments: Box<[Argument]>,
    },
    I64,
    Runtime,
    Function {
        is_unsafe: bool,
        parameters: Box<[Parameter]>,
        return_type: Box<Type>,
    },
}

#[derive(Debug)]
pub struct Dyn {
    pub location: SourceLocation,
    pub kind: DynKind,
}

#[derive(Debug)]
pub enum DynKind {
    Generic(GenericDynId),
    Instantiation {
        parameterized_dyn: ParameterizedDynId,
        arguments: Box<[Argument]>,
    },
}

#[derive(Debug)]
pub struct Argument {
    pub location: SourceLocation,
    pub kind: ArgumentKind,
}

#[derive(Debug)]
pub enum ArgumentKind {
    Value {
        expression: Box<Expression>,
    },
    Type(Type),
    ParameterizedType(ParameterizedTypeId),
    ParameterizedTypeFunction {
        parameters: Box<[Parameter]>,
        typ: Type,
    },
    Dyn(Dyn),
    ParameterizedDyn(ParameterizedDynId),
    ParameterizedDynFunction {
        parameters: Box<[Parameter]>,
        r#dyn: Dyn,
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
        is_unsafe: bool,
        end_location: SourceLocation,
        statements: Box<[Statement]>,
        last_expression: Box<Expression>,
    },
    Call {
        operand: Box<Expression>,
        arguments: Box<[Argument]>,
    },
    Constructor {
        typ: Box<Type>,
        members: Box<[ConstructorMember]>,
    },
    Function(FunctionId),
}

#[derive(Debug)]
pub struct ConstructorMember {
    pub location: SourceLocation,
    pub name: InternedStr,
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
        pattern: Box<Pattern>,
        value: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Item {
    pub location: SourceLocation,
    pub kind: ItemKind,
}

#[derive(Debug)]
pub enum ItemKind {
    TypeAlias(TypeAliasId),
    Struct(StructId),
    ParameterizedType(ParameterizedTypeId),
    Function(FunctionId),
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
    Deconstructor {
        typ: Box<Type>,
        members: Box<[DeconstructorMember]>,
    },
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
    pub kind: PlaceKind,
}

#[derive(Debug)]
pub enum PlaceKind {
    Function(FunctionId),
    Variable(VariableId),
    Let(VariableId),
    MemberAccess {
        operand: Box<Expression>,
        member_name: InternedStr,
    },
}

struct FormatSlotMap<'a, K: slotmap::Key, V: Debug> {
    slotmap: &'a SlotMap<K, V>,
}

impl<K: slotmap::Key, V: Debug> Debug for FormatSlotMap<'_, K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct FormatKey<K: Debug> {
            key: K,
        }

        impl<K: Debug> Debug for FormatKey<K> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self.key)
            }
        }

        f.debug_map()
            .entries(
                self.slotmap
                    .iter()
                    .map(|(key, value)| (FormatKey { key }, value)),
            )
            .finish()
    }
}

impl Debug for ResolvedProgram {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedProgram")
            .field("type_aliases", &FormatSlotMap { slotmap: &self.type_aliases })
            .field("structs", &FormatSlotMap { slotmap: &self.structs })
            .field("generic_types", &FormatSlotMap { slotmap: &self.generic_types })
            .field("generic_dyns", &FormatSlotMap { slotmap: &self.generic_dyns })
            .field("parameterized_types", &FormatSlotMap { slotmap: &self.parameterized_types })
            .field("parameterized_dyns", &FormatSlotMap { slotmap: &self.parameterized_dyns })
            .field("functions", &FormatSlotMap { slotmap: &self.functions })
            .field("variables", &FormatSlotMap { slotmap: &self.variables })
            .field("unit_type", &self.unit_type)
            .finish()
    }
}
