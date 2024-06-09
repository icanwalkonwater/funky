use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op},
};

use crate::{
    ast::{self},
    Rule,
};

pub fn parse_file(pair: Pair<Rule>) -> ast::File {
    assert_eq!(pair.as_rule(), Rule::File);

    let functions = pair
        .into_inner()
        .take_while(|p| p.as_rule() != Rule::EOI)
        .map(parse_function_definition)
        .collect::<Vec<_>>();

    ast::File { functions }
}

pub fn parse_function_definition(pair: Pair<Rule>) -> ast::FunctionDefinition {
    assert_eq!(pair.as_rule(), Rule::FunctionDefinition);

    let [name, body] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();

    assert_eq!(name.as_rule(), Rule::Identifier);
    let name = ast::Identifier(name.as_str().to_string());

    let body = parse_block(body);

    ast::FunctionDefinition {
        name,
        parameters: vec![],
        return_type: None,
        body,
    }
}

pub fn parse_block(pair: Pair<Rule>) -> ast::Block {
    assert_eq!(pair.as_rule(), Rule::Block);

    let (statements, rest) = pair
        .into_inner()
        .partition::<Vec<_>, _>(|p| p.as_rule() == Rule::Statement);
    let statements = statements.into_iter().map(parse_statement).collect();

    assert!(rest.len() <= 1);
    let final_expression = rest.into_iter().next().map(parse_expression);

    ast::Block {
        statements,
        final_expression,
    }
}

pub fn parse_statement(pair: Pair<Rule>) -> ast::Statement {
    assert_eq!(pair.as_rule(), Rule::Statement);

    let mut inner = pair.into_inner();
    assert_eq!(inner.len(), 1);

    inner
        .next()
        .map(|p| match p.as_rule() {
            Rule::AssignementStatement => parse_assignement_statement(p),
            Rule::VariableDeclarationStatement => parse_variable_declaration_statement(p),
            Rule::ExpressionStatement => {
                ast::Statement::Expression(parse_expression(p.into_inner().next().unwrap()))
            }
            _ => unreachable!(),
        })
        .unwrap()
}

pub fn parse_assignement_statement(pair: Pair<Rule>) -> ast::Statement {
    assert_eq!(pair.as_rule(), Rule::AssignementStatement);

    let [lvalue, operator, rvalue] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();

    let lvalue = parse_expression(lvalue);
    let rvalue = parse_expression(rvalue);

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

    ast::Statement::Assignement {
        lvalue,
        operator,
        rvalue,
    }
}

pub fn parse_variable_declaration_statement(pair: Pair<Rule>) -> ast::Statement {
    assert_eq!(pair.as_rule(), Rule::VariableDeclarationStatement);

    let [name, initialization] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();
    assert!(name.as_rule() == Rule::Identifier);
    let initialization = parse_expression(initialization);

    ast::Statement::VariableDeclaration {
        identifier: ast::Identifier(name.as_str().to_string()),
        initialization: Some(initialization),
    }
}

pub fn parse_expression(pair: Pair<Rule>) -> ast::Expression {
    assert_eq!(pair.as_rule(), Rule::Expression);

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

    pratt
        .map_primary(|primary| parse_expression_atom(primary))
        .map_prefix(|operation, operand| {
            let operation = match operation.as_rule() {
                Rule::Noop => ast::UnaryOperator::Nothing,
                Rule::Negate => ast::UnaryOperator::Negate,
                Rule::Complement => ast::UnaryOperator::Complement,
                Rule::Not => ast::UnaryOperator::Not,
                _ => unreachable!(),
            };
            ast::Expression::UnaryOperation {
                operation,
                operand: Box::new(operand),
            }
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
            ast::Expression::BinaryOperation {
                left: Box::new(lhs),
                operation,
                right: Box::new(rhs),
            }
        })
        .parse(pair.into_inner())
}

pub fn parse_expression_atom(pair: Pair<Rule>) -> ast::Expression {
    assert!(pair.as_rule() == Rule::ExpressionAtom);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::Expression => parse_expression(inner),
        Rule::Block => ast::Expression::Block(Box::new(parse_block(inner))),
        Rule::Identifier => {
            ast::Expression::Identifier(ast::Identifier(inner.as_str().to_string()))
        }
        Rule::Literal => ast::Expression::Literal(parse_literal(inner)),
        _ => unreachable!(),
    }
}

pub fn parse_literal(pair: Pair<Rule>) -> ast::Literal {
    assert!(pair.as_rule() == Rule::Literal);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::LiteralInteger => ast::Literal::Integer(inner.as_str().to_string()),
        Rule::LiteralFloat => ast::Literal::Float(inner.as_str().to_string()),
        Rule::LiteralString => ast::Literal::String(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}
