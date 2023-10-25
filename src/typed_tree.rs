use crate::{
    location::SourceLocation,
    nodes::{NodeID, Nodes},
    types::TypeID,
};

pub type TypedNodeID<'filepath> = NodeID<TypedNode<'filepath>>;
pub type TypeNodes<'filepath> = Nodes<TypedNode<'filepath>>;

pub enum TypedNode<'filepath> {
    Integer {
        location: SourceLocation<'filepath>,
        type_: TypeID,
        value: u128,
    },
}
