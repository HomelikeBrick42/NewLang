use crate::{inferring_tree as it, interning::InternedStr, lexer::SourceLocation};
use enum_map::EnumMap;
use slotmap::{SecondaryMap, SlotMap, new_key_type};
use std::fmt::Display;

#[derive(Debug)]
pub struct Program {
    pub inferring_functions_map: SecondaryMap<it::FunctionId, FunctionId>,
    pub functions: SlotMap<FunctionId, Function>,
    pub inferring_types_map: SecondaryMap<it::TypeId, TypeId>,
    pub types: SlotMap<TypeId, Type>,
    pub builtin_types: EnumMap<it::BuiltinType, Option<TypeId>>,
}

new_key_type! {
    pub struct FunctionId;
}

#[derive(Debug)]
pub struct Function {
    pub location: SourceLocation,
    pub name: Option<InternedStr>,
    pub body: FunctionBody,
}

#[derive(Debug)]
pub enum FunctionBody {
    Resolving,
    Body {
        variables: SlotMap<VariableId, Variable>,
        parameter_variables: Box<[VariableId]>,
        blocks: SlotMap<BlockId, Block>,
        entry_block: BlockId,
    },
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

new_key_type! {
    pub struct BlockId;
}

#[derive(Debug)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub jump: Jump,
}

#[derive(Debug)]
pub struct Instruction {
    pub location: SourceLocation,
    pub kind: InstructionKind,
}

#[derive(Debug)]
pub enum InstructionKind {
    Copy {
        source: VariableId,
        destination: VariableId,
    },
    ConstantI64 {
        destination: VariableId,
        value: i64,
    },
    ConstantFunctionItem {
        destination: VariableId,
    },
    AssumeInit {
        variable: VariableId,
    },
    PrintI64 {
        variable: VariableId,
    },
}

#[derive(Debug)]
pub struct Jump {
    pub location: SourceLocation,
    pub kind: JumpKind,
}

#[derive(Debug)]
pub enum JumpKind {
    Unreachable,
    Next(BlockId),
    Return {
        variable: VariableId,
    },
    Call {
        operand: VariableId,
        arguments: Box<[Argument]>,
        return_variable: VariableId,
        return_to: BlockId,
    },
}

#[derive(Debug)]
pub enum Argument {
    Value { variable: VariableId },
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
    Unit,
    Runtime,
    I64,
    FunctionItem(FunctionId),
}

pub struct PrettyPrintType<'a> {
    pub id: TypeId,
    pub types: &'a SlotMap<TypeId, Type>,
}

impl Display for PrettyPrintType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.types[self.id].kind {
            TypeKind::Resolving => write!(f, "{{resolving}}"),
            TypeKind::Unit => write!(f, "Unit"),
            TypeKind::Runtime => write!(f, "Runtime"),
            TypeKind::I64 => write!(f, "I64"),
            TypeKind::FunctionItem(_) => write!(f, "fn_item(...) -> _"),
        }
    }
}
