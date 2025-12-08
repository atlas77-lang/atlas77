use crate::atlas_c::atlas_frontend::lexer::token::{LexingError, Token, TokenKind};
use logos::Logos;

use crate::atlas_c::utils::Span;
pub mod token;

#[derive(Debug)]
pub struct AtlasLexer {
    path: &'static str,
    pub source: String,
}

impl AtlasLexer {
    pub fn new(path: &'static str, source: String) -> Self {
        AtlasLexer { path, source }
    }
    pub fn tokenize(&mut self) -> Result<Vec<Token>, (LexingError, Span)> {
        let lex = TokenKind::lexer(&self.source);
        let mut res: Vec<Result<Token, (LexingError, Span)>> = lex
            .spanned()
            .map(|(kind, span)| match kind {
                Ok(kind) => Ok(Token::new(
                    Span {
                        start: span.start,
                        end: span.end,
                        path: self.path,
                    },
                    kind,
                )),
                Err(e) => Err((
                    e,
                    Span {
                        start: span.start,
                        end: span.end,
                        path: self.path,
                    },
                )),
            })
            .collect::<Vec<_>>();
        res.push(Ok(Token::new(Span::default(), TokenKind::EoI)));
        res.into_iter().collect::<Result<_, _>>()
    }
}

mod test {
    #[test]
    fn test_lexer() {
        let source = r#"
package result; 

struct Result<T, E> {
  private:
    data: T?
    err: E? 
  public:
      //Special case function like __init__() in Python
      fun init(data: T?, err: E?) -> Result<T, E> {
        this.data = data; 
        this.err = err; 
      }
      fun ok(data: T) -> Result<T, E> {
        return this.init(data, null); 
      }
      fun err(err: E) -> Result<T, E> { 
        return this.init(null, err); 
      }
      fun is_ok(this) -> bool { 
        return this.data != null; 
      } 
      fun unwrap(this) -> T { 
        if this.is_ok() {
          return this.data; 
        } 
        panic("Unwrap called on an Err"); 
      } 
}"#;
        let mut lexer = super::AtlasLexer::new("test.atlas".into(), source.to_string());
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err((e, span)) => {
                panic!("Lexing error: {:?} at {:?}", e, span);
            }
        };
        for token in tokens {
            println!("{:?} at {:?}", token.kind(), token.span());
        }
    }
}

pub trait Spanned {
    fn union_span(&self, other: &Self) -> Self;
}

impl Spanned for Span {
    /// Returns a new Span that covers both self and other
    ///
    /// If the paths are different, the path of self is used
    fn union_span(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            path: self.path,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TokenKind::KwElse => {
                write!(f, "else")
            }
            TokenKind::KwEnum => {
                write!(f, "enum")
            }
            TokenKind::KwExtern => {
                write!(f, "extern")
            }
            _ => {
                write!(f, "{:?}", self.kind())
            }
        }
    }
}

#[derive(Debug)]
pub struct TokenVec(pub Vec<TokenKind>);

impl std::fmt::Display for TokenVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.0 {
            write!(f, "{:?} ", token)?;
        }
        Ok(())
    }
}
