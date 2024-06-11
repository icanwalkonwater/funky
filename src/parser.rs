use std::cell::RefCell;

use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op},
};

use crate::{
    ast::{self, AstNodeId},
    CompilerContext, Rule,
};

pub fn parse_file(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::File);

    let functions = pair
        .into_inner()
        .take_while(|p| p.as_rule() != Rule::EOI)
        .map(|p| parse_function_definition(ctx, p))
        .collect::<Vec<_>>();

    ctx.ast_nodes.push(ast::File { functions })
}

pub fn parse_function_definition(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::FunctionDefinition);

    let [name, body] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();

    assert_eq!(name.as_rule(), Rule::Identifier);
    let name = ctx
        .ast_nodes
        .push(ast::Identifier(name.as_str().to_string()));

    let body = parse_block(ctx, body);

    ctx.ast_nodes.push(ast::FunctionDefinition {
        name,
        return_type: None,
        body,
    })
}

pub fn parse_block(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::Block);

    let (statements, rest) = pair
        .into_inner()
        .partition::<Vec<_>, _>(|p| p.as_rule() == Rule::Statement);
    let statements = statements
        .into_iter()
        .map(|p| parse_statement(ctx, p))
        .collect();

    assert!(rest.len() <= 1);
    let final_expression = rest.into_iter().next().map(|p| parse_expression(ctx, p));

    ctx.ast_nodes.push(ast::Block {
        statements,
        final_expression,
    })
}

pub fn parse_statement(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::Statement);

    let mut inner = pair.into_inner();
    assert_eq!(inner.len(), 1);

    inner
        .next()
        .map(|p| match p.as_rule() {
            Rule::AssignementStatement => parse_assignement_statement(ctx, p),
            Rule::VariableDeclarationStatement => parse_variable_declaration_statement(ctx, p),
            Rule::ExpressionStatement => {
                let e = parse_expression(ctx, p.into_inner().next().unwrap());
                ctx.ast_nodes.push(ast::Statement::Expression(e))
            }
            _ => unreachable!(),
        })
        .unwrap()
}

pub fn parse_assignement_statement(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::AssignementStatement);

    let [lvalue, operator, rvalue] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();

    let lvalue = parse_expression(ctx, lvalue);
    let rvalue = parse_expression(ctx, rvalue);

    let operator = match operator.as_rule() {
        Rule::Assign => ast::AssignOperator::Assign,
        Rule::AddAssign => ast::AssignOperator::AddAssign,
        Rule::SubstractAssign => ast::AssignOperator::SubstractAssign,
        Rule::MultiplyAssign => ast::AssignOperator::MultiplyAssign,
        Rule::DivideAssign => ast::AssignOperator::DivideAssign,
        Rule::BitAndAssign => ast::AssignOperator::BitAndAssign,
        Rule::BitOrAssign => ast::AssignOperator::BitOrAssign,
        Rule::ShiftLeftAssign => ast::AssignOperator::ShiftLeftAssign,
        Rule::ShiftRightAssign => ast::AssignOperator::ShiftRightAssign,
        _ => todo!("Unsupported operator desugaring"),
    };

    ctx.ast_nodes.push(ast::Statement::Assignement {
        lvalue,
        operator,
        rvalue,
    })
}

pub fn parse_variable_declaration_statement(
    ctx: &mut CompilerContext,
    pair: Pair<Rule>,
) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::VariableDeclarationStatement);

    let [name, ty, initialization] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();
    assert_eq!(name.as_rule(), Rule::Identifier);
    let name = ctx
        .ast_nodes
        .push(ast::Identifier(name.as_str().to_string()));
    let ty = parse_type(ctx, ty);
    let initialization = parse_expression(ctx, initialization);

    ctx.ast_nodes.push(ast::Statement::VariableDeclaration {
        identifier: name,
        ty,
        initialization: Some(initialization),
    })
}

pub fn parse_type(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::Type);

    let [ident] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();
    assert_eq!(ident.as_rule(), Rule::Identifier);

    let ident = ctx
        .ast_nodes
        .push(ast::Identifier(ident.as_str().to_string()));
    ctx.ast_nodes.push(ast::Type(ident))
}

pub fn parse_expression(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert_eq!(pair.as_rule(), Rule::Expression);

    let ctx = RefCell::new(ctx);

    let pratt = pest::pratt_parser::PrattParser::new()
        .op(Op::infix(Rule::Or, Assoc::Left))
        .op(Op::infix(Rule::And, Assoc::Left))
        .op(Op::infix(Rule::LowerThan, Assoc::Left)
            | Op::infix(Rule::LowerEqual, Assoc::Left)
            | Op::infix(Rule::GreaterThan, Assoc::Left)
            | Op::infix(Rule::GreaterEqual, Assoc::Left)
            | Op::infix(Rule::Equal, Assoc::Left)
            | Op::infix(Rule::NotEqual, Assoc::Left))
        .op(Op::infix(Rule::BitAnd, Assoc::Left)
            | Op::infix(Rule::BitOr, Assoc::Left)
            | Op::infix(Rule::BitXor, Assoc::Left))
        .op(Op::infix(Rule::ShiftLeft, Assoc::Left) | Op::infix(Rule::ShiftRight, Assoc::Left))
        .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Substract, Assoc::Left))
        .op(Op::infix(Rule::Multiply, Assoc::Left) | Op::infix(Rule::Divide, Assoc::Left))
        .op(Op::prefix(Rule::Noop)
            | Op::prefix(Rule::Negate)
            | Op::prefix(Rule::Complement)
            | Op::prefix(Rule::Not));

    let expr = pratt
        .map_primary(|primary| parse_expression_atom(&mut ctx.borrow_mut(), primary))
        .map_prefix(|operation, operand| {
            let operation = match operation.as_rule() {
                Rule::Noop => ast::UnaryOperator::Nothing,
                Rule::Negate => ast::UnaryOperator::Negate,
                Rule::Complement => ast::UnaryOperator::Complement,
                Rule::Not => ast::UnaryOperator::Not,
                _ => unreachable!(),
            };
            ctx.borrow_mut()
                .ast_nodes
                .push(ast::Expression::UnaryOperation { operation, operand })
        })
        .map_infix(|lhs, operation, rhs| {
            let operation = match operation.as_rule() {
                Rule::Or => ast::BinaryOperator::Or,
                Rule::And => ast::BinaryOperator::And,
                Rule::LowerThan => ast::BinaryOperator::LowerThan,
                Rule::LowerEqual => ast::BinaryOperator::LowerThanOrEqual,
                Rule::GreaterThan => ast::BinaryOperator::GreaterThan,
                Rule::GreaterEqual => ast::BinaryOperator::GreaterThanOrEqual,
                Rule::Equal => ast::BinaryOperator::Equal,
                Rule::NotEqual => ast::BinaryOperator::NotEqual,
                Rule::BitAnd => ast::BinaryOperator::BitAnd,
                Rule::BitOr => ast::BinaryOperator::BitOr,
                Rule::BitXor => ast::BinaryOperator::BitXor,
                Rule::ShiftLeft => ast::BinaryOperator::ShiftLeft,
                Rule::ShiftRight => ast::BinaryOperator::ShiftRight,
                Rule::Add => ast::BinaryOperator::Add,
                Rule::Substract => ast::BinaryOperator::Substract,
                Rule::Multiply => ast::BinaryOperator::Multiply,
                Rule::Divide => ast::BinaryOperator::Divivide,
                _ => unreachable!(),
            };
            ctx.borrow_mut()
                .ast_nodes
                .push(ast::Expression::BinaryOperation {
                    left: lhs,
                    operation,
                    right: rhs,
                })
        })
        .parse(pair.into_inner());
    expr
}

pub fn parse_expression_atom(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert!(pair.as_rule() == Rule::ExpressionAtom);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::Expression => parse_expression(ctx, inner),
        Rule::Block => {
            let block = parse_block(ctx, inner);
            ctx.ast_nodes.push(ast::Expression::Block(block))
        }
        Rule::Identifier => {
            let ident = ctx
                .ast_nodes
                .push(ast::Identifier(inner.as_str().to_string()));
            ctx.ast_nodes.push(ast::Expression::Identifier(ident))
        }
        Rule::Literal => {
            let lit = parse_literal(ctx, inner);
            ctx.ast_nodes.push(ast::Expression::Literal(lit))
        }
        _ => unreachable!(),
    }
}

pub fn parse_literal(ctx: &mut CompilerContext, pair: Pair<Rule>) -> AstNodeId {
    assert!(pair.as_rule() == Rule::Literal);

    let inner = pair.into_inner().next().unwrap();
    ctx.ast_nodes.push(match inner.as_rule() {
        Rule::LiteralInteger => ast::Literal::Integer(inner.as_str().to_string()),
        Rule::LiteralFloat => ast::Literal::Float(inner.as_str().to_string()),
        Rule::LiteralString => ast::Literal::String(inner.as_str().to_string()),
        _ => unreachable!(),
    })
}
