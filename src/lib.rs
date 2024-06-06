use pest_derive::Parser;

pub mod ast;
pub mod lexer;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct FunkyParser;
