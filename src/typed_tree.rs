use crate::{
    location::SourceLocation,
    nodes::{NodeID, Nodes},
    types::TypeID,
};

pub type TypedNodeID<'filepath> = NodeID<TypedNode<'filepath>>;
pub type TypeNodes<'filepath> = Nodes<TypedNode<'filepath>>;

pub type NameID<'filepath> = NodeID<Name<'filepath>>;
pub type Names<'filepath> = Nodes<Name<'filepath>>;

pub struct Name<'filepath> {
    pub name: Option<String>,
    pub node: TypedNodeID<'filepath>,
}

#[derive(Debug, Clone)]
pub enum TypedNode<'filepath> {
    Integer {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        value: u128,
    },
    Procedure {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        parameters: Vec<TypedPattern<'filepath>>,
        body: TypedNodeID<'filepath>,
    },
    Call {
        location: SourceLocation<'filepath>,
        /// this is the return type of the function
        type_: TypeID,
        arguments: Vec<TypedNodeID<'filepath>>,
    },
    Assignment {
        location: SourceLocation<'filepath>,
        /// this should always be unit
        type_: TypeID,
        pattern: TypedPattern<'filepath>,
        value: TypedNodeID<'filepath>,
    },
    Block {
        location: SourceLocation<'filepath>,
        /// the type of the last expression is its type, otherwise its unit
        type_: TypeID,
        label: Option<NameID<'filepath>>,
        expressions: TypedPattern<'filepath>,
    },
    If {
        location: SourceLocation<'filepath>,
        /// the type that both expressions return
        type_: TypeID,
        condition: TypedNodeID<'filepath>,
        then_expr: TypedNodeID<'filepath>,
        /// this is an empty tuple if there is no else block
        else_expr: TypedNodeID<'filepath>,
    },
    While {
        location: SourceLocation<'filepath>,
        /// this is always unit
        type_: TypeID,
        label: NameID<'filepath>,
        condition: TypedNodeID<'filepath>,
        body: TypedNodeID<'filepath>,
    },
    Break {
        location: SourceLocation<'filepath>,
        /// this is always never
        type_: TypeID,
        target: NameID<'filepath>,
        value: TypedNodeID<'filepath>,
    },
    Unit {
        location: SourceLocation<'filepath>,
        /// this is always a type
        type_: TypeID,
    },
}

#[derive(Debug, Clone)]
pub enum TypedPattern<'filepath> {
    Integer {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        value: u128,
    },
    Let {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        name: NameID<'filepath>,
    },
    Const {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        name: NameID<'filepath>,
    },
}

impl<'filepath> TypedNode<'filepath> {
    pub fn get_location(&self) -> SourceLocation<'filepath> {
        match *self {
            TypedNode::Integer { location, .. }
            | TypedNode::Procedure { location, .. }
            | TypedNode::Call { location, .. }
            | TypedNode::Assignment { location, .. }
            | TypedNode::Block { location, .. }
            | TypedNode::If { location, .. }
            | TypedNode::While { location, .. }
            | TypedNode::Break { location, .. }
            | TypedNode::Unit { location, .. } => location,
        }
    }

    pub fn get_type(&self) -> TypeID {
        match *self {
            TypedNode::Integer { type_, .. }
            | TypedNode::Procedure { type_, .. }
            | TypedNode::Call { type_, .. }
            | TypedNode::Assignment { type_, .. }
            | TypedNode::Block { type_, .. }
            | TypedNode::If { type_, .. }
            | TypedNode::While { type_, .. }
            | TypedNode::Break { type_, .. }
            | TypedNode::Unit { type_, .. } => type_,
        }
    }
}
