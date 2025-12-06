pub mod lexer;
pub mod parser;

use std::path::PathBuf;

use lexer::AtlasLexer;
use parser::{arena::AstArena, ast::AstProgram, error::ParseResult};

pub fn parse<'ast>(
    path: &'static str,
    arena: &'ast AstArena<'ast>,
    source: String,
) -> ParseResult<AstProgram<'ast>> {
    let mut lex = AtlasLexer::new(path, source.clone());
    let token_res = lex.tokenize();
    let tokens = match token_res {
        Ok(tokens) => tokens,
        Err(e) => panic!("Error while lexing: {:?}", e),
    };
    let mut parser = parser::Parser::new(arena, tokens, PathBuf::from(path));
    parser.parse()
}
