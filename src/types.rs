use crate::nodes::{NodeID, Nodes};

pub type TypeID = NodeID<Type>;
pub type Types = Nodes<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved {
        resolved: Option<TypeID>,
    },
    Procedure {
        parameters: Vec<TypeID>,
        return_type: TypeID,
    },
    Type,
    Unit,
    Bool,
    ISize,
}

impl Type {
    pub fn equal(a: &Type, b: &Type, types: &Types) -> bool {
        match (a, b) {
            (
                &Type::Unresolved {
                    resolved: Some(resolved),
                },
                _,
            ) => Type::equal(&types[resolved], b, types),
            (
                _,
                &Type::Unresolved {
                    resolved: Some(resolved),
                },
            ) => Type::equal(a, &types[resolved], types),

            (
                &Type::Procedure {
                    parameters: ref a_parameters,
                    return_type: a_return_type,
                },
                &Type::Procedure {
                    parameters: ref b_parameters,
                    return_type: b_return_type,
                },
            ) => {
                Type::equal(&types[a_return_type], &types[b_return_type], types)
                    && a_parameters.len() == b_parameters.len()
                    && a_parameters
                        .iter()
                        .zip(b_parameters)
                        .all(|(&a, &b)| Type::equal(&types[a], &types[b], types))
            }
            (&Type::Type, &Type::Type) => true,
            (&Type::Unit, &Type::Unit) => true,
            (&Type::Bool, &Type::Bool) => true,
            (&Type::ISize, &Type::ISize) => true,

            (_, _) => false,
        }
    }
}
