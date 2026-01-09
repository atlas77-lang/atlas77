use logos::Logos;
use std::num::{ParseFloatError, ParseIntError};
use std::str::ParseBoolError;

use crate::atlas_c::utils::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    #[inline(always)]
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
    #[inline(always)]
    pub fn kind(&self) -> TokenKind {
        self.kind.clone()
    }
    #[inline(always)]
    pub fn span(&self) -> Span {
        self.span
    }
    #[inline(always)]
    pub fn start(&self) -> usize {
        self.span.start
    }
    #[inline(always)]
    pub fn end(&self) -> usize {
        self.span.end
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum LexingError {
    InvalidInteger(String),
    InvalidFloat(String),
    InvalidUnsignedInteger(String),
    InvalidBool(String),
    #[default]
    NonAsciiChar,
}

impl From<ParseIntError> for LexingError {
    fn from(e: ParseIntError) -> Self {
        LexingError::InvalidInteger(e.to_string())
    }
}

impl From<ParseFloatError> for LexingError {
    fn from(e: ParseFloatError) -> Self {
        LexingError::InvalidFloat(e.to_string())
    }
}

impl From<ParseBoolError> for LexingError {
    fn from(e: ParseBoolError) -> Self {
        LexingError::InvalidBool(e.to_string())
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexingError)]
//Skip whitespace regex
#[logos(skip r"[ \t\n\f\r]+")]
pub enum TokenKind {
    //We need to be able to capture '\t', '\n', '\\', '\'', '\"', etc.
    #[regex("\"[^\"]*\"", |lex| {
        let raw = &lex.slice()[1..lex.slice().len()-1];
        let mut result = String::new();
        let mut chars = raw.chars();
        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some('r') => result.push('\r'),
                    Some('\\') => result.push('\\'),
                    Some('\'') => result.push('\''),
                    Some('\"') => result.push('\"'),
                    Some('0') => result.push('\0'),
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => result.push('\\'),
                }
            } else {
                result.push(ch);
            }
        }
        result
    })]
    StringLiteral(String),
    #[regex("'.'", |lex| lex.slice().chars().nth(1).unwrap())]
    Char(char),
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),
    // Let's add it with trailing as in 123f
    #[regex("[0-9]+\\.[0-9]+|[0-9]+f", |lex| {
        let slice = lex.slice();
        if let Some(stripped) = slice.strip_suffix('f') {
            stripped.parse()
        } else {
            slice.parse()
        }
    })]
    Float(f64),
    /// Let's add it with trailing as in 123u
    #[regex("[0-9]+u", |lex| {
        let slice = &lex.slice()[..lex.slice().len() - 1]; // Remove the trailing 'u'
        slice.parse()
    })]
    UnsignedInteger(u64),
    #[regex("true|false", |lex| lex.slice().parse())]
    Bool(bool),
    #[regex(r"//.*", |lex| lex.slice().to_string(), allow_greedy = true)]
    Comments(String),
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token("+=")]
    OpAssignAdd,
    #[token("-")]
    Minus,
    #[token("-=")]
    OpAssignSub,
    #[token("/")]
    Slash,
    #[token("/=")]
    OpAssignDiv,
    #[token("*")]
    Star,
    #[token("*=")]
    OpAssignMul,
    #[token("%")]
    Percent,
    #[token("%=")]
    OpAssignMod,
    #[token("=")]
    OpAssign,
    #[token("\\")]
    BackSlash,
    #[token(";")]
    Semicolon,
    #[token("'")]
    Quote,
    #[token("?")]
    Interrogation,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NEq,
    #[token("!")]
    Bang,
    #[token("..")]
    DoubleDot,
    #[token(".")]
    Dot,
    #[token("#")]
    Hash,
    #[token("::")]
    DoubleColon,
    #[token(":")]
    Colon,
    #[token("->")]
    RArrow,
    #[token("<-")]
    LArrow,
    #[token("<=")]
    LFatArrow,
    #[token("<")]
    LAngle,
    #[token(">=")]
    OpGreaterThanEq,
    #[token(">")]
    RAngle,
    #[token("&&")]
    OpAnd,
    #[token("&")]
    Ampersand,
    #[token("||")]
    OpOr,
    #[token("|")]
    Pipe,
    #[token("=>")]
    RFatArrow,
    #[token("~")]
    Tilde,
    #[token("this")]
    KwThis,
    #[token("operator")]
    KwOperator,
    #[token("class")]
    KwClass,
    #[token("new")]
    KwNew,
    #[token("delete")]
    KwDelete,
    #[token("fun")]
    KwFunc,
    #[token("where")]
    //Used for generics constraints and bounds (i.e. fn foo<T>(arg: T) -> T where T: Add)
    KwWhere,
    #[token("extern")]
    KwExtern,
    #[token("struct")]
    KwStruct,
    #[token("concept")]
    KwConcept,
    #[token("enum")]
    KwEnum,
    #[token("union")]
    KwUnion,
    #[token("import")]
    KwImport,
    //Visibility
    #[token("public")]
    KwPublic,
    #[token("private")]
    KwPrivate,
    //Control Flow
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("match")]
    KwMatch,
    //Loops
    #[token("while")]
    KwWhile,
    #[token("break")]
    KwBreak,
    #[token("continue")]
    KwContinue,
    #[token("return")]
    KwReturn,
    //Variables
    #[token("let")]
    KwLet,
    #[token("const")]
    KwConst,
    //Misc
    #[token("comptime")]
    KwComptime,
    #[token("as")]
    KwAs,
    //Primitive Types
    #[token("extern_ptr")]
    ExternPtr,
    #[token("int64")]
    Int64Ty,
    #[token("float64")]
    Float64Ty,
    #[token("uint64")]
    UInt64Ty,
    #[token("unit")]
    UnitTy,
    #[token("char")]
    CharTy,
    #[token("bool")]
    BoolTy,
    #[token("This")]
    ThisTy,
    #[token("string")]
    StrTy,
    EoI,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    //Only with trailing i.e. 1_u64
    UnsignedInteger(u64),
    Bool(bool),
    Char(char),
    Identifier(String),
    StringLiteral(String),
}
