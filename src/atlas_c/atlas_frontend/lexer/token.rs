use logos::Logos;
use std::num::{ParseFloatError, ParseIntError};
use std::str::ParseBoolError;

use crate::atlas_c::utils::Span;

fn parse_string_literal(lex: &mut logos::Lexer<'_, TokenKind>) -> String {
    let raw = &lex.slice()[1..lex.slice().len() - 1];
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
                Some('"') => result.push('"'),
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
}

fn parse_char_literal(lex: &mut logos::Lexer<'_, TokenKind>) -> char {
    let c = lex.slice().chars().nth(1).unwrap();
    if c == '\\' {
        match lex.slice().chars().nth(2) {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('r') => '\r',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            Some('0') => '\0',
            Some(c) => c,
            None => '\\',
        }
    } else {
        c
    }
}

fn parse_identifier(lex: &mut logos::Lexer<'_, TokenKind>) -> String {
    lex.slice().to_string()
}

fn parse_integer(lex: &mut logos::Lexer<'_, TokenKind>) -> Result<i64, ParseIntError> {
    parse_integer_literal(lex.slice())
}

fn parse_float(lex: &mut logos::Lexer<'_, TokenKind>) -> Result<f64, ParseFloatError> {
    let slice = lex.slice();
    if let Some(stripped) = slice.strip_suffix('f') {
        stripped.parse()
    } else {
        slice.parse()
    }
}

fn parse_unsigned_integer(lex: &mut logos::Lexer<'_, TokenKind>) -> Result<u64, ParseIntError> {
    parse_unsigned_integer_literal(lex.slice())
}

fn parse_integer_literal(slice: &str) -> Result<i64, ParseIntError> {
    let (radix, digits) = split_integer_literal(slice);
    i64::from_str_radix(&digits, radix)
}

fn parse_unsigned_integer_literal(slice: &str) -> Result<u64, ParseIntError> {
    let slice = slice.strip_suffix('u').unwrap();
    let (radix, digits) = split_integer_literal(slice);
    u64::from_str_radix(&digits, radix)
}

fn split_integer_literal(slice: &str) -> (u32, String) {
    let (radix, digits) = if let Some(digits) = slice.strip_prefix("0b") {
        (2, digits)
    } else if let Some(digits) = slice.strip_prefix("0B") {
        (2, digits)
    } else if let Some(digits) = slice.strip_prefix("0o") {
        (8, digits)
    } else if let Some(digits) = slice.strip_prefix("0O") {
        (8, digits)
    } else if let Some(digits) = slice.strip_prefix("0x") {
        (16, digits)
    } else if let Some(digits) = slice.strip_prefix("0X") {
        (16, digits)
    } else {
        (10, slice)
    };

    (radix, digits.replace('_', ""))
}

fn parse_bool(lex: &mut logos::Lexer<'_, TokenKind>) -> Result<bool, ParseBoolError> {
    lex.slice().parse()
}

fn parse_comment(lex: &mut logos::Lexer<'_, TokenKind>) -> String {
    lex.slice().to_string()
}

fn parse_doc_comment(lex: &mut logos::Lexer<'_, TokenKind>) -> String {
    let slice = lex.slice();
    if slice.len() > 4 && &slice[3..4] == " " {
        slice[4..].to_string()
    } else {
        slice[3..].to_string()
    }
}

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
    #[regex("\"[^\"]*\"", parse_string_literal)]
    StringLiteral(String),
    #[regex("'[^\']*'", parse_char_literal)]
    Char(char),
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", parse_identifier)]
    Identifier(String),
    #[regex(
        "0[bB][01]+(?:_[01]+)*|0[oO][0-7]+(?:_[0-7]+)*|0[xX][0-9a-fA-F]+(?:_[0-9a-fA-F]+)*|[0-9]+(?:_[0-9]+)*",
        parse_integer
    )]
    Integer(i64),
    #[regex("[0-9]+\\.[0-9]+|[0-9]+f", parse_float)]
    Float(f64),
    /// Let's add it with trailing as in 123u
    #[regex(
        "0[bB][01]+(?:_[01]+)*u|0[oO][0-7]+(?:_[0-7]+)*u|0[xX][0-9a-fA-F]+(?:_[0-9a-fA-F]+)*u|[0-9]+(?:_[0-9]+)*u",
        parse_unsigned_integer
    )]
    UnsignedInteger(u64),
    #[regex("true|false", parse_bool)]
    Bool(bool),
    #[regex(r"//.*|/\*[\s\S]*?\*/", parse_comment, allow_greedy = true)]
    Comments(String),
    /// ``//! This is a doc comment``
    #[regex(r"//!.*", parse_doc_comment, allow_greedy = true)]
    Docs(String),
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
    #[token("...")]
    Ellipsis,
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
    #[token("^")]
    Caret,
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
    #[token("delete")]
    KwDelete,
    #[token("fun")]
    KwFunc,
    #[token("where")]
    //Used for generics constraints and bounds (i.e. fn foo<T>(arg: T) -> T where T: Add)
    KwWhere,
    #[token("null")]
    KwNull,
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
    #[token("namespace")]
    KwNamespace,
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
    #[token("as")]
    KwAs,
    //Signed Types
    #[token("int64")]
    Int64Ty,
    #[token("int32")]
    Int32Ty,
    #[token("int16")]
    Int16Ty,
    #[token("int8")]
    Int8Ty,
    //Float Types
    #[token("float64")]
    Float64Ty,
    #[token("float32")]
    Float32Ty,
    //Unsigned Types
    #[token("uint64")]
    UInt64Ty,
    #[token("uint32")]
    UInt32Ty,
    #[token("uint16")]
    UInt16Ty,
    #[token("uint8")]
    UInt8Ty,
    //other types
    #[token("unit")]
    UnitTy,
    #[token("char")]
    CharTy,
    #[token("bool")]
    BoolTy,
    #[token("This")]
    ThisTy,
    #[token("str")]
    StrTy,
    // === Keywords for compile-time ===
    #[token("comptime")]
    KwComptime,
    #[token("then")] // Then is used in compile-time ifs
    KwThen,
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
