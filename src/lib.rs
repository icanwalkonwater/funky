use pest_derive::Parser;

pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct FunkyParser;

#[derive(Default)]
pub struct CompilerContext {
    pub ast_nodes: ast::AstNodeStorage,
}
