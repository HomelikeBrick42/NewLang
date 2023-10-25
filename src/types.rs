use crate::nodes::{NodeID, Nodes};

pub type TypeID = NodeID<Type>;
pub type Types = Nodes<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved { resolved: Option<TypeID> },
    Tuple { types: Vec<TypeID> },
    ISize,
}

impl Type {
    pub fn equal(&self, other: &Type, types: &Types) -> bool {
        match (self, other) {
            (
                &Type::Unresolved {
                    resolved: Some(resolved),
                },
                _,
            ) => types[resolved].equal(other, types),
            (
                _,
                &Type::Unresolved {
                    resolved: Some(resolved),
                },
            ) => self.equal(&types[resolved], types),
            (Type::Tuple { types: a_types }, Type::Tuple { types: b_types }) => {
                a_types.len() == b_types.len()
                    && a_types
                        .iter()
                        .zip(b_types)
                        .all(|(&a, &b)| types[a].equal(&types[b], types))
            }
            (Type::ISize, Type::ISize) => true,
            (_, _) => false,
        }
    }
}
