use std::{fs::File, io::Read, path::PathBuf};

use pest::{iterators::Pair, Parser};

#[derive(Debug, clap::Parser)]
struct CliOpts {
    filename: PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opts = <CliOpts as clap::Parser>::parse();

    let mut source_file = File::open(&opts.filename)?;
    let mut code = String::new();
    source_file.read_to_string(&mut code)?;

    let mut res = funky::FunkyParser::parse(funky::Rule::File, &code)?;
    assert_eq!(res.len(), 1);
    let file = res.next().unwrap();

    assert!(file.as_rule() == funky::Rule::File);
    let file = parse_file(file);

    dbg!(file);

    Ok(())
}

fn parse_file(pair: Pair<funky::Rule>) -> funky::ast::File {
    assert!(pair.as_rule() == funky::Rule::File);

    let functions = pair
        .into_inner()
        .take_while(|p| p.as_rule() != funky::Rule::EOI)
        .map(|p| match p.as_rule() {
            funky::Rule::Function => parse_function(p),
            _ => panic!("Unexpected {p}"),
        })
        .collect::<Vec<_>>();

    funky::ast::File { functions }
}

fn parse_function(pair: Pair<funky::Rule>) -> funky::ast::Function {
    assert!(pair.as_rule() == funky::Rule::Function);

    let mut pairs = pair.into_inner();

    let name = pairs
        .next()
        .filter(|p| p.as_rule() == funky::Rule::Identifier)
        .unwrap()
        .as_str()
        .to_string();
    let name = funky::ast::Identifier(name);

    let parameters = {
        let mut parameters = Vec::new();
        while let Some(funky::Rule::FunctionParameter) = pairs.peek().map(|p| p.as_rule()) {
            parameters.push(parse_function_parameter(pairs.next().unwrap()));
        }
        parameters
    };

    let body = parse_block(pairs.next().unwrap());
    assert!(pairs.next().is_none());

    funky::ast::Function {
        name,
        parameters,
        return_type: None,
        body,
    }
}

fn parse_function_parameter(pair: Pair<funky::Rule>) -> funky::ast::FunctionParameter {
    todo!()
}

fn parse_block(pair: Pair<funky::Rule>) -> funky::ast::Block {
    assert!(pair.as_rule() == funky::Rule::Block);

    let statements = vec![];
    // let statements = pair
    //     .into_inner()
    //     .map(|p| match p.as_rule() {
    //         funky::Rule::Statement => parse_statement(p),
    //         _ => panic!("Unexpected {:?}", p.as_rule()),
    //     })
    //     .collect();

    // TODO: final expr
    funky::ast::Block {
        statements,
        final_expression: None,
    }
}

fn parse_statement(pair: Pair<funky::Rule>) -> funky::ast::Statement {
    todo!()
}
