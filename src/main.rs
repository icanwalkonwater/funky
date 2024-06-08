use std::{fs::File, io::Read, path::PathBuf};

use funky::{ast, Rule};
use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op},
    Parser,
};

#[derive(Debug, clap::Parser)]
struct CliOpts {
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = <CliOpts as clap::Parser>::parse();

    let mut source_file = File::open(&opts.filename)?;
    let mut code = String::new();
    source_file.read_to_string(&mut code)?;

    let mut res = funky::FunkyParser::parse(Rule::File, &code)?;
    assert_eq!(res.len(), 1);
    let file = res.next().unwrap();

    assert!(file.as_rule() == funky::Rule::File);
    let file = parse_file(file);

    dbg!(file);

    Ok(())
}

fn parse_file(pair: Pair<Rule>) -> ast::File {
    assert!(pair.as_rule() == funky::Rule::File);

    let functions = pair
        .into_inner()
        .take_while(|p| p.as_rule() != Rule::EOI)
        .map(|p| match p.as_rule() {
            Rule::FunctionDefinition => parse_function(p),
            _ => panic!("Unexpected {p}"),
        })
        .collect::<Vec<_>>();

    ast::File { functions }
}

fn parse_function(pair: Pair<Rule>) -> ast::FunctionDefinition {
    assert!(pair.as_rule() == funky::Rule::FunctionDefinition);

    let mut pairs = pair.into_inner();

    let name = pairs
        .next()
        .filter(|p| p.as_rule() == Rule::Identifier)
        .unwrap()
        .as_str()
        .to_string();
    let name = ast::Identifier(name);

    let body = parse_block(pairs.next().unwrap());
    assert!(pairs.next().is_none());

    ast::FunctionDefinition {
        name,
        parameters: vec![],
        return_type: None,
        body,
    }
}

fn parse_block(pair: Pair<Rule>) -> ast::Block {
    assert!(pair.as_rule() == funky::Rule::Block);

    let statements = pair
        .into_inner()
        .map(|p| match p.as_rule() {
            Rule::Statement => parse_statement(p),
            _ => panic!("Unexpected {:?}", p.as_rule()),
        })
        .collect();

    // TODO: final expr
    ast::Block {
        statements,
        final_expression: None,
    }
}

fn parse_statement(pair: Pair<Rule>) -> ast::Statement {
    assert!(pair.as_rule() == funky::Rule::Statement);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::AssignementStatement => parse_assignement_statement(inner),
        Rule::VariableDeclarationStatement => parse_variable_declaration_statement(inner),
        Rule::ExpressionStatement => {
            ast::Statement::Expression(parse_expression(inner.into_inner().next().unwrap()))
        }
        _ => unreachable!(),
    }
}

fn parse_assignement_statement(pair: Pair<Rule>) -> ast::Statement {
    assert!(pair.as_rule() == funky::Rule::AssignementStatement);

    let mut inner = pair.into_inner();

    let lvalue = parse_expression(inner.next().unwrap());

    let operator = inner.next().unwrap();
    assert!(operator.as_rule() == funky::Rule::AssignementOperator);
    match operator.as_str() {
        "=" => {}
        _ => todo!("Unsupported operator desugaring"),
    }

    let rvalue = parse_expression(inner.next().unwrap());

    ast::Statement::Assignement { lvalue, rvalue }
}

fn parse_variable_declaration_statement(pair: Pair<Rule>) -> ast::Statement {
    assert!(pair.as_rule() == funky::Rule::VariableDeclarationStatement);

    let mut inner = pair.into_inner();

    let name = inner.next().unwrap();
    assert!(name.as_rule() == funky::Rule::Identifier);
    let initialization = inner.next().map(parse_expression);

    ast::Statement::VariableDeclaration {
        identifier: ast::Identifier(name.as_str().to_string()),
        initialization,
    }
}

fn parse_expression(pair: Pair<Rule>) -> ast::Expression {
    assert!(pair.as_rule() == Rule::Expression);

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

fn parse_expression_atom(pair: Pair<Rule>) -> ast::Expression {
    assert!(pair.as_rule() == funky::Rule::ExpressionAtom);

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

fn parse_literal(pair: Pair<Rule>) -> ast::Literal {
    assert!(pair.as_rule() == funky::Rule::Literal);

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::LiteralInteger => ast::Literal::Integer(inner.as_str().to_string()),
        Rule::LiteralFloat => ast::Literal::Float(inner.as_str().to_string()),
        Rule::LiteralString => ast::Literal::String(inner.as_str().to_string()),
        _ => unreachable!(),
    }
}
