use crate::{
    location::SourceLocation,
    nodes::{NodeID, Nodes},
};

pub type SyntaxNodeID<'filepath, 'source> = NodeID<SyntaxNode<'filepath, 'source>>;
pub type SyntaxNodes<'filepath, 'source> = Nodes<SyntaxNode<'filepath, 'source>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    /// -
    Negative,
    /// !
    Inverse,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    /// +
    Add,
    /// -
    Subtract,
    /// *
    Multiply,
    /// /
    Divide,
    /// %
    Remainder,
}

#[derive(Debug, Clone)]
pub struct Parameter<'filepath, 'source> {
    pub pattern: SyntaxNodeID<'filepath, 'source>,
    pub default_value: Option<SyntaxNodeID<'filepath, 'source>>,
}

#[derive(Debug, Clone)]
pub enum SyntaxNode<'filepath, 'source> {
    Integer {
        integer_location: SourceLocation<'filepath>,
        value: u128,
    },
    Name {
        name_location: SourceLocation<'filepath>,
        name: &'source str,
    },
    /// the `_` name
    Placeholder {
        placeholder_location: SourceLocation<'filepath>,
    },
    UnaryOperator {
        operator_location: SourceLocation<'filepath>,
        operator: UnaryOperator,
        operand: SyntaxNodeID<'filepath, 'source>,
    },
    BinaryOperator {
        left: SyntaxNodeID<'filepath, 'source>,
        operator_location: SourceLocation<'filepath>,
        operator: BinaryOperator,
        right: SyntaxNodeID<'filepath, 'source>,
    },
    Block {
        label: Option<&'source str>,
        open_brace_location: SourceLocation<'filepath>,
        expressions: Vec<SyntaxNodeID<'filepath, 'source>>,
        close_brace_location: SourceLocation<'filepath>,
    },
    Procedure {
        proc_location: SourceLocation<'filepath>,
        arguments: Vec<Parameter<'filepath, 'source>>,
        return_type: Option<SyntaxNodeID<'filepath, 'source>>,
        body: SyntaxNodeID<'filepath, 'source>,
    },
    Call {
        operand: SyntaxNodeID<'filepath, 'source>,
        open_parenthesis_location: SourceLocation<'filepath>,
        arguments: Vec<SyntaxNodeID<'filepath, 'source>>,
        close_parenthesis_location: SourceLocation<'filepath>,
    },
    Break {
        break_location: SourceLocation<'filepath>,
        label: Option<&'source str>,
        value: Option<SyntaxNodeID<'filepath, 'source>>,
    },
    Let {
        let_location: SourceLocation<'filepath>,
        name_location: SourceLocation<'filepath>,
        name: &'source str,
        type_: Option<SyntaxNodeID<'filepath, 'source>>,
    },
    Assignment {
        pattern: SyntaxNodeID<'filepath, 'source>,
        equals_location: SourceLocation<'filepath>,
        value: SyntaxNodeID<'filepath, 'source>,
    },
    If {
        if_location: SourceLocation<'filepath>,
        condition: SyntaxNodeID<'filepath, 'source>,
        then_body: SyntaxNodeID<'filepath, 'source>,
        else_body: Option<SyntaxNodeID<'filepath, 'source>>,
    },
    While {
        while_location: SourceLocation<'filepath>,
        condition: SyntaxNodeID<'filepath, 'source>,
        body: SyntaxNodeID<'filepath, 'source>,
    },
}

impl<'filepath, 'source> SyntaxNode<'filepath, 'source> {
    pub fn get_location(&self) -> SourceLocation<'filepath> {
        match *self {
            Self::Integer {
                integer_location, ..
            } => integer_location,
            Self::Name { name_location, .. } => name_location,
            Self::Placeholder {
                placeholder_location,
                ..
            } => placeholder_location,
            Self::UnaryOperator {
                operator_location, ..
            } => operator_location,
            Self::BinaryOperator {
                operator_location, ..
            } => operator_location,
            Self::Block {
                open_brace_location,
                ..
            } => open_brace_location,
            Self::Procedure { proc_location, .. } => proc_location,
            Self::Call {
                open_parenthesis_location,
                ..
            } => open_parenthesis_location,
            Self::Break { break_location, .. } => break_location,
            Self::Let { let_location, .. } => let_location,
            Self::Assignment {
                equals_location, ..
            } => equals_location,
            Self::If { if_location, .. } => if_location,
            Self::While { while_location, .. } => while_location,
        }
    }
}
