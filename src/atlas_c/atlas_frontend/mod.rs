pub mod lexer;
pub mod parser;

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
        Err(e) => return Err(Box::new(e.into())),
    };
    let mut parser = parser::Parser::new(arena, tokens, path, source);
    parser.parse()
}
