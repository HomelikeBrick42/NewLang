use crate::{
    syntax_tree::{SyntaxNode, SyntaxNodeID, SyntaxNodes},
    typed_tree::{TypedNode, TypedNodeID, TypedNodes},
    types::TypeID,
};

pub fn type_syntax_tree<'filepath, 'source>(
    root: SyntaxNodeID<'filepath, 'source>,
    syntax_nodes: &SyntaxNodes<'filepath, 'source>,
) -> (TypedNodeID<'filepath>, TypedNodes<'filepath>) {
    let mut typed_nodes = TypedNodes::new();
    (type_node(root, syntax_nodes, &mut typed_nodes), typed_nodes)
}

fn type_node<'filepath, 'source>(
    syntax_node: SyntaxNodeID<'filepath, 'source>,
    syntax_nodes: &SyntaxNodes<'filepath, 'source>,
    nodes: &mut TypedNodes<'filepath>,
) -> TypedNodeID<'filepath> {
    match syntax_nodes[syntax_node] {
        SyntaxNode::Integer {
            integer_location,
            value,
        } => nodes.add(TypedNode::Integer {
            location: integer_location,
            type_: todo!(),
            value,
        }),
        SyntaxNode::Name {
            name_location: _,
            name: _,
        } => todo!(),
        SyntaxNode::Placeholder {
            placeholder_location: _,
        } => todo!(),
        SyntaxNode::UnaryOperator {
            operator_location: _,
            operator: _,
            operand,
        } => {
            let _operand = type_node(operand, syntax_nodes, nodes);
            todo!()
        }
        SyntaxNode::BinaryOperator {
            left,
            operator_location: _,
            operator: _,
            right,
        } => {
            let _left = type_node(left, syntax_nodes, nodes);
            let _right = type_node(right, syntax_nodes, nodes);
            todo!()
        }
        SyntaxNode::Block {
            label: _,
            open_brace_location: _,
            expressions: _,
            close_brace_location: _,
        } => todo!(),
        SyntaxNode::Procedure {
            proc_location: _,
            arguments: _,
            return_type: _,
            body: _,
        } => todo!(),
        SyntaxNode::Call {
            operand: _,
            open_parenthesis_location: _,
            arguments: _,
            close_parenthesis_location: _,
        } => todo!(),
        SyntaxNode::Break {
            break_location: _,
            label: _,
            value: _,
        } => todo!(),
        SyntaxNode::Let {
            let_location: _,
            name_location: _,
            name: _,
            type_: _,
        } => todo!(),
        SyntaxNode::Const {
            const_location: _,
            name_location: _,
            name: _,
            type_: _,
        } => todo!(),
        SyntaxNode::Unit {
            open_parenthesis_location,
            close_parenthesis_location: _,
        } => nodes.add(TypedNode::Unit {
            location: open_parenthesis_location,
            type_: todo!(),
        }),
        SyntaxNode::Assignment {
            pattern: _,
            equals_location: _,
            value: _,
        } => todo!(),
        SyntaxNode::If {
            if_location: _,
            condition: _,
            then_body: _,
            else_body: _,
        } => todo!(),
        SyntaxNode::While {
            while_location: _,
            condition: _,
            body: _,
        } => todo!(),
    }
}
