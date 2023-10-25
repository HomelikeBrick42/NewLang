use crate::{
    location::SourceLocation,
    nodes::{NodeID, Nodes},
    types::TypeID,
};

pub type TypedNodeID<'filepath> = NodeID<TypedNode<'filepath>>;
pub type TypeNodes<'filepath> = Nodes<TypedNode<'filepath>>;

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
        // TODO: label ids
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
        while_location: SourceLocation<'filepath>,
        /// this is always unit
        type_: TypeID,
        condition: TypedNodeID<'filepath>,
        body: TypedNodeID<'filepath>,
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
        // TODO: name ids
    },
    Const {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        // TODO: name ids
    },
}
