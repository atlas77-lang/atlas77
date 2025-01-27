//todo: Add Parser::sync() to recover from errors
pub mod arena;
pub mod ast;
pub mod error;

use std::path::PathBuf;

use miette::{SourceOffset, SourceSpan};

use crate::atlas_frontend::parser::error::{ParseError, ParseResult, UnexpectedTokenError};
use ast::{
    AstAssignExpr, AstBinaryOp, AstBinaryOpExpr, AstBlock, AstBooleanLiteral, AstBooleanType,
    AstBreakStmt, AstCallExpr, AstConst, AstContinueStmt, AstExpr, AstExternFunction,
    AstFieldAccessExpr, AstFloatLiteral, AstFloatType, AstFunction, AstFunctionType, AstIdentifier,
    AstIfElseExpr, AstImport, AstIndexingExpr, AstIntegerLiteral, AstIntegerType, AstItem, AstLet,
    AstLiteral, AstNamedType, AstObjField, AstPointerType, AstProgram, AstReturnStmt, AstStatement,
    AstStringLiteral, AstStringType, AstStruct, AstType, AstUnaryOp, AstUnaryOpExpr, AstUnitType,
    AstUnsignedIntegerLiteral, AstUnsignedIntegerType, AstWhileExpr,
};

use crate::atlas_frontend::lexer::{token::{Token, TokenKind}, Spanned, TokenVec};
use crate::atlas_frontend::parser::ast::{AstCastingExpr, AstClass, AstListLiteral, AstListType, AstNewObjExpr, AstStaticAccessExpr, AstVisibility};
use arena::AstArena;
use logos::Span;

pub(crate) struct Parser<'ast> {
    arena: &'ast AstArena<'ast>,
    tokens: Vec<Token>,
    //for error reporting
    _file_path: PathBuf,
    pos: usize,
    src: String,
}

pub fn remove_comments(toks: Vec<Token>) -> Vec<Token> {
    toks.into_iter()
        .filter(|t| !matches!(t.kind(), TokenKind::Comments(_)))
        .collect()
}

impl<'ast> Parser<'ast> {
    pub fn new(
        arena: &'ast AstArena<'ast>,
        tokens: Vec<Token>,
        _file_path: PathBuf,
        src: String,
    ) -> Parser<'ast> {
        let tokens = remove_comments(tokens);
        Parser {
            arena,
            tokens,
            _file_path,
            pos: 0,
            src,
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn peek(&self) -> Option<TokenKind> {
        self.tokens.get(self.pos + 1).map(|t| t.kind())
    }

    /// This should maybe return a ParseResult::UnexpectedEndOfFileError
    fn advance(&mut self) -> Token {
        let tok = self.tokens.get(self.pos).cloned();
        if let Some(t) = tok {
            self.pos += 1;
            t
        } else {
            Token::new(Span::default(), TokenKind::EoI)
        }
    }

    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        let tok = self.advance();
        if tok.kind() == kind {
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                token: tok.clone(),
                expected: TokenVec(vec![kind]),
                span: SourceSpan::new(SourceOffset::from(tok.start()), tok.end() - tok.start()),
                src: self.src.clone(),
            }))
        }
    }

    pub fn parse(&mut self) -> ParseResult<AstProgram<'ast>> {
        let mut items: Vec<AstItem> = Vec::new();
        while self.current().kind() != TokenKind::EoI {
            items.push(self.parse_item()?);
        }

        let node = AstProgram {
            items: self.arena.alloc_vec(items),
        };
        Ok(node)
    }

    fn parse_item(&mut self) -> ParseResult<AstItem<'ast>> {
        let start = self.current().start();
        match self.current().kind() {
            TokenKind::KwStruct => Ok(AstItem::Struct(self.parse_struct()?)),
            TokenKind::KwImport => Ok(AstItem::Import(self.parse_import()?)),
            TokenKind::KwExtern => Ok(AstItem::ExternFunction(self.parse_extern_function()?)),
            TokenKind::KwFunc => Ok(AstItem::Func(self.parse_func()?)),
            TokenKind::KwClass => Ok(AstItem::Class(self.parse_class()?)),
            //This does allow for "private public private func foo() {}" which is bad... but it's a start!
            TokenKind::KwPublic => {
                let _ = self.advance();
                let mut item = self.parse_item()?;
                item.set_vis(AstVisibility::Public);
                Ok(item)
            }
            TokenKind::KwPrivate => {
                let _ = self.advance();
                let mut item = self.parse_item()?;
                item.set_vis(AstVisibility::Private);
                Ok(item)
            }
            //Handling comments
            _ => Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                token: self.current().clone(),
                expected: TokenVec(vec![TokenKind::Identifier(
                    "Item".to_string(),
                )]),
                span: SourceSpan::new(SourceOffset::from(start), self.current().end() - start),
                src: self.src.clone(),
            })),
        }
    }

    /// example:
    /// ```
    /// public class Foo {
    ///     public:
    ///         bar: &str;
    ///         baz: f64;
    ///     private:
    ///         qux: i64;
    ///     public:
    ///         func create() -> Foo {
    ///             return new Foo {
    ///                 bar: "Hello World",
    ///                 baz: 3.14,
    ///                 qux: 42,
    ///             }
    ///         }
    /// }
    /// ```
    fn parse_class(&mut self) -> ParseResult<AstClass<'ast>> {
        self.expect(TokenKind::KwClass)?;
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LBrace)?;
        let mut fields = vec![];
        let mut curr_vis = self.parse_current_vis(AstVisibility::Private)?;
        while self.current().kind() != TokenKind::RBrace {
            curr_vis = self.parse_current_vis(curr_vis)?;
            if self.current().kind() == TokenKind::KwFunc {
                break;
            }
            let mut obj_field = self.parse_obj_field()?;
            obj_field.vis = curr_vis;
            fields.push(obj_field);
            self.expect(TokenKind::Semicolon)?;
        }

        let mut methods = vec![];
        while self.current().kind() == TokenKind::KwFunc {
            curr_vis = self.parse_current_vis(curr_vis)?;
            let mut method = self.parse_func()?;
            method.vis = curr_vis;
            methods.push(method);
        }

        self.expect(TokenKind::RBrace)?;

        let node = AstClass {
            span: Span::union_span(&name.span, &self.current().span()),
            name_span: name.span.clone(),
            name: self.arena.alloc(name),
            field_span: Span::union_span(&fields.first().unwrap().span, &fields.last().unwrap().span),
            fields: self.arena.alloc_vec(fields),
            methods: self.arena.alloc_vec(methods),
            vis: AstVisibility::default(),
        };
        println!("{:?}", node);
        Ok(node)
    }

    fn parse_current_vis(&mut self, previous_vis: AstVisibility) -> ParseResult<AstVisibility> {
        match self.current().kind() {
            TokenKind::KwPublic => {
                let _ = self.advance();
                self.expect(TokenKind::Colon)?;
                Ok(AstVisibility::Public)
            }
            TokenKind::KwPrivate => {
                let _ = self.advance();
                self.expect(TokenKind::Colon)?;
                Ok(AstVisibility::Private)
            }
            _ => Ok(previous_vis)
        }
    }

    fn parse_func(&mut self) -> ParseResult<AstFunction<'ast>> {
        let _ = self.advance();
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LParen)?;
        let mut params = vec![];
        while self.current().kind() != TokenKind::RParen {
            params.push(self.parse_obj_field()?);
            //Bad code imo, because programmers could just do: `func foo(bar: i32 baz: i64)` with no comma between the args
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;
        let mut ret_ty = AstType::Unit(AstUnitType {
            span: Span::default(),
        });
        if self.current().kind() == TokenKind::RArrow {
            let _ = self.advance();
            ret_ty = self.parse_type()?;
        }
        let body = self.parse_block()?;
        let node = AstFunction {
            span: Span::union_span(&name.span, &body.span),
            name: self.arena.alloc(name),
            args: self.arena.alloc_vec(params),
            ret: self.arena.alloc(ret_ty),
            body: self.arena.alloc(body),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_block(&mut self) -> ParseResult<AstBlock<'ast>> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = vec![];
        while self.current().kind() != TokenKind::RBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;

        let node = AstBlock {
            span: Span::union_span(&stmts.first().unwrap().span(), &stmts.last().unwrap().span()),
            stmts: self.arena.alloc_vec(stmts),
        };
        Ok(node)
    }

    fn parse_stmt(&mut self) -> ParseResult<AstStatement<'ast>> {
        let start = self.current();
        match start.kind() {
            TokenKind::KwLet => {
                let node = AstStatement::Let(self.parse_let()?);
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }
            TokenKind::KwConst => {
                let node = AstStatement::Const(self.parse_const()?);
                self.expect(TokenKind::Semicolon)?;
                Ok(node)
            }
            TokenKind::KwIf => {
                let node = AstStatement::IfElse(self.parse_if_expr()?);
                Ok(node)
            }
            TokenKind::KwWhile => {
                let node = AstStatement::While(self.parse_while()?);
                Ok(node)
            }
            TokenKind::KwBreak => {
                let node = self.parse_break()?;
                Ok(AstStatement::Break(node))
            }
            TokenKind::KwContinue => {
                let node = self.parse_continue()?;
                Ok(AstStatement::Continue(node))
            }
            TokenKind::KwReturn => {
                let node = AstStatement::Return(self.parse_return()?);
                Ok(node)
            }
            _ => {
                let node = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(AstStatement::Expr(node))
            }
        }
    }

    fn parse_while(&mut self) -> ParseResult<AstWhileExpr<'ast>> {
        let start = self.advance();
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        let node = AstWhileExpr {
            span: Span::union_span(&start.span(), &body.span),
            condition: self.arena.alloc(condition),
            body: self.arena.alloc(body),
        };
        Ok(node)
    }

    fn parse_continue(&mut self) -> ParseResult<AstContinueStmt> {
        let start_span = self.current().span();
        self.expect(TokenKind::KwContinue)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(AstContinueStmt {
            span: Span::union_span(&start_span, &self.current().span()),
        })
    }

    fn parse_break(&mut self) -> ParseResult<AstBreakStmt> {
        let start_span = self.current().span();
        self.expect(TokenKind::KwBreak)?;
        self.expect(TokenKind::Semicolon)?;
        Ok(AstBreakStmt {
            span: Span::union_span(&start_span, &self.current().span()),
        })
    }

    /// This function is mostly used for clarity because calling `parse_binary` feels weird
    fn parse_expr(&mut self) -> ParseResult<AstExpr<'ast>> {
        self.parse_binary()
    }

    fn parse_let(&mut self) -> ParseResult<AstLet<'ast>> {
        let start = self.current().span();
        self.expect(TokenKind::KwLet)?;
        let name = self.parse_identifier()?;

        let ty: Option<&AstType> = if let TokenKind::Colon = self.current().kind() {
            let _ = self.advance();
            let t = self.parse_type()?;
            Some(self.arena.alloc(t))
        } else {
            eprintln!("Type inference is still unstable.");
            None
        };

        self.expect(TokenKind::OpAssign)?;

        let value = self.parse_binary()?;
        let node = AstLet {
            span: Span::union_span(&start, &value.span()),
            name: self.arena.alloc(name),
            ty,
            value: self.arena.alloc(value),
        };
        Ok(node)
    }

    fn parse_const(&mut self) -> ParseResult<AstConst<'ast>> {
        let start = self.current().span();
        self.expect(TokenKind::KwConst)?;
        let name = self.parse_identifier()?;

        self.expect(TokenKind::Colon)?;

        let ty = self.parse_type()?;

        self.expect(TokenKind::OpAssign)?;

        let value = self.parse_binary()?;
        let node = AstConst {
            span: Span::union_span(&start, &value.span()),
            name: self.arena.alloc(name),
            ty: Some(self.arena.alloc(ty)),
            value: self.arena.alloc(value),
        };
        Ok(node)
    }

    fn parse_binary(&mut self) -> ParseResult<AstExpr<'ast>> {
        let left = self.parse_factor()?;
        match self.current().kind() {
            TokenKind::OpAdd | TokenKind::OpSub => {
                let op = match self.current().kind() {
                    TokenKind::OpAdd => AstBinaryOp::Add,
                    TokenKind::OpSub => AstBinaryOp::Sub,
                    _ => unreachable!(),
                };
                let _ = self.advance();
                let right = self.parse_binary()?;
                let node = AstExpr::BinaryOp(AstBinaryOpExpr {
                    span: Span::union_span(&left.span(), &right.span()),
                    op,
                    lhs: self.arena.alloc(left),
                    rhs: self.arena.alloc(right),
                });
                Ok(node)
            }
            _ => Ok(left),
        }
    }

    fn parse_factor(&mut self) -> ParseResult<AstExpr<'ast>> {
        let left = self.parse_condition()?;
        match self.current().kind() {
            TokenKind::OpMul | TokenKind::OpDiv | TokenKind::OpMod => {
                let op = match self.current().kind() {
                    TokenKind::OpMul => AstBinaryOp::Mul,
                    TokenKind::OpDiv => AstBinaryOp::Div,
                    TokenKind::OpMod => AstBinaryOp::Mod,
                    _ => unreachable!(),
                };
                let _ = self.advance();
                let right = self.parse_factor()?;
                let node = AstExpr::BinaryOp(AstBinaryOpExpr {
                    span: Span::union_span(&left.span(), &right.span()),
                    op,
                    lhs: self.arena.alloc(left),
                    rhs: self.arena.alloc(right),
                });
                Ok(node)
            }
            _ => Ok(left),
        }
    }
    //Condition should be switched with parse_binary (because it's the lowest precedence)
    fn parse_condition(&mut self) -> ParseResult<AstExpr<'ast>> {
        let left = self.parse_casting()?;

        match self.current().kind() {
            TokenKind::OpEq
            | TokenKind::OpNEq
            | TokenKind::OpGreaterThan
            | TokenKind::OpGreaterThanEq
            | TokenKind::OpLessThan
            | TokenKind::OpLessThanEq => {
                let op = match self.current().kind() {
                    TokenKind::OpEq => AstBinaryOp::Eq,
                    TokenKind::OpNEq => AstBinaryOp::NEq,
                    TokenKind::OpGreaterThan => AstBinaryOp::Gt,
                    TokenKind::OpGreaterThanEq => AstBinaryOp::Gte,
                    TokenKind::OpLessThan => AstBinaryOp::Lt,
                    TokenKind::OpLessThanEq => AstBinaryOp::Lte,
                    _ => unreachable!(),
                };
                let _ = self.advance();
                let right = self.parse_condition()?;
                let node = AstExpr::BinaryOp(AstBinaryOpExpr {
                    span: Span::union_span(&left.span(), &right.span()),
                    op,
                    lhs: self.arena.alloc(left),
                    rhs: self.arena.alloc(right),
                });
                Ok(node)
            }
            _ => Ok(left),
        }
    }

    fn parse_casting(&mut self) -> ParseResult<AstExpr<'ast>> {
        let left = AstExpr::UnaryOp(self.parse_unary()?);
        match self.current().kind() {
            TokenKind::KwAs => {
                self.expect(TokenKind::KwAs)?;
                let ty = self.parse_type()?;
                let node = AstExpr::Casting(AstCastingExpr {
                    span: Span::union_span(&left.span(), &ty.span()),
                    value: self.arena.alloc(left),
                    ty: self.arena.alloc(ty),
                });

                Ok(node)
            }
            _ => Ok(left),
        }
    }

    fn parse_unary(&mut self) -> ParseResult<AstUnaryOpExpr<'ast>> {
        let start_pos = self.current().span();
        let op = match self.current().kind() {
            TokenKind::OpSub => {
                let _ = self.advance();
                Some(AstUnaryOp::Neg)
            }
            TokenKind::Bang => {
                let _ = self.advance();
                Some(AstUnaryOp::Not)
            }
            _ => None,
        };

        let expr = self.parse_primary()?;
        let node = AstUnaryOpExpr {
            span: Span::union_span(&start_pos, &self.current().span()),
            op,
            expr: self.arena.alloc(expr),
        };
        Ok(node)
    }

    fn parse_primary(&mut self) -> ParseResult<AstExpr<'ast>> {
        let tok = self.current();

        let node = match tok.kind() {
            TokenKind::Bool(b) => {
                let node = AstExpr::Literal(AstLiteral::Boolean(AstBooleanLiteral {
                    span: tok.span(),
                    value: b,
                }));
                let _ = self.advance();
                node
            }
            TokenKind::Float(f) => {
                let node = AstExpr::Literal(AstLiteral::Float(AstFloatLiteral {
                    span: tok.span(),
                    value: f,
                }));
                let _ = self.advance();
                node
            }
            TokenKind::Integer(i) => {
                let node = AstExpr::Literal(AstLiteral::Integer(AstIntegerLiteral {
                    span: tok.span(),
                    value: i,
                }));
                let _ = self.advance();
                node
            }
            TokenKind::UnsignedInteger(u) => {
                let node =
                    AstExpr::Literal(AstLiteral::UnsignedInteger(AstUnsignedIntegerLiteral {
                        span: tok.span(),
                        value: u,
                    }));
                let _ = self.advance();
                node
            }
            TokenKind::StringLiteral(s) => {
                let node = AstExpr::Literal(AstLiteral::String(AstStringLiteral {
                    span: tok.span(),
                    value: self.arena.alloc(s),
                }));
                let _ = self.advance();
                node
            }
            TokenKind::KwNew => {
                let node = AstExpr::NewObj(self.parse_new_obj()?);
                node
            }
            TokenKind::LBracket => {
                let start = self.advance();
                let mut elements = vec![];
                while self.current().kind() != TokenKind::RBracket {
                    elements.push(self.parse_expr()?);
                    if self.current().kind() == TokenKind::Comma {
                        let _ = self.advance();
                    }
                }
                self.expect(TokenKind::RBracket)?;
                let node = AstExpr::Literal(AstLiteral::List(AstListLiteral {
                    span: start.span(),
                    items: self.arena.alloc_vec(elements),
                }));
                node
            }
            TokenKind::Identifier(_) => {
                let mut node = AstExpr::Identifier(self.parse_identifier()?);

                while self.peek().is_some() {
                    match self.current().kind() {
                        TokenKind::LParen => {
                            node = AstExpr::Call(self.parse_fn_call(node)?);
                            //let _ = self.advance();
                        }
                        TokenKind::LBracket => {
                            node = AstExpr::Indexing(self.parse_indexing(node)?);
                        }
                        TokenKind::Dot => {
                            node = AstExpr::FieldAccess(self.parse_field_access(node)?);
                        }
                        TokenKind::OpAssign => {
                            node = AstExpr::Assign(self.parse_assign(node)?);
                            return Ok(node);
                        }
                        TokenKind::DoubleColon => {
                            node = AstExpr::StaticAccess(self.parse_static_access(node)?);
                        }
                        _ => {
                            break;
                        }
                    }
                }

                node
            }
            TokenKind::KwIf => AstExpr::IfElse(self.parse_if_expr()?),
            _ => {
                return Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                    token: tok.clone(),
                    expected: TokenVec(vec![TokenKind::Identifier(
                        "Primary expression".to_string(),
                    )]),
                    span: SourceSpan::new(SourceOffset::from(tok.start()), (tok.end() - tok.start()) as usize),
                    src: self.src.clone(),
                }))
            }
        };
        Ok(node)
    }

    fn parse_static_access(&mut self, node: AstExpr<'ast>) -> ParseResult<AstStaticAccessExpr<'ast>> {
        self.expect(TokenKind::DoubleColon)?;
        let field = self.parse_identifier()?;
        let node = AstStaticAccessExpr {
            span: Span::union_span(&node.span(), &field.span),
            target: self.arena.alloc(node),
            field: self.arena.alloc(field),
        };
        Ok(node)
    }

    fn parse_new_obj(&mut self) -> ParseResult<AstNewObjExpr<'ast>> {
        self.expect(TokenKind::KwNew)?;
        let name = self.parse_identifier()?;

        self.expect(TokenKind::LBrace)?;

        let mut fields = vec![];
        while self.current().kind() != TokenKind::RBrace {
            let field_name = self.parse_identifier()?;
            self.expect(TokenKind::Colon)?;
            let field_value = self.parse_expr()?;
            fields.push(AstObjField {
                vis: AstVisibility::default(),
                span: Span::union_span(&field_name.span, &field_value.span()),
                name: self.arena.alloc(field_name),
                ty: self.arena.alloc(AstType::Unit(AstUnitType { span: Span::default() })),
            });
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RBrace)?;
        let node = AstNewObjExpr {
            span: Span::union_span(&name.span, &self.current().span()),
            ty: self.arena.alloc(name),
            fields: self.arena.alloc(vec![]),
        };

        Ok(node)
    }

    fn parse_if_expr(&mut self) -> ParseResult<AstIfElseExpr<'ast>> {
        let start = self.advance();
        let condition = self.parse_expr()?;
        let if_body = self.parse_block()?;
        let else_body = if self.current().kind() == TokenKind::KwElse {
            let _ = self.advance();
            let else_body = self.parse_block()?;
            Some(else_body)
        } else {
            None
        };

        let node = AstIfElseExpr {
            span: Span::union_span(&start.span(), &if_body.span),
            condition: self.arena.alloc(condition),
            body: self.arena.alloc(if_body),
            else_body: if let Some(e) = else_body {
                Some(self.arena.alloc(e))
            } else {
                None
            },
        };
        Ok(node)
    }

    fn parse_return(&mut self) -> ParseResult<AstReturnStmt<'ast>> {
        let _ = self.advance();
        let expr = self.parse_expr()?;
        let node = AstReturnStmt {
            span: Span::union_span(&self.current().span(), &expr.span()),
            value: self.arena.alloc(expr),
        };
        self.expect(TokenKind::Semicolon)?;
        Ok(node)
    }

    fn parse_extern_function(&mut self) -> ParseResult<AstExternFunction<'ast>> {
        let _ = self.advance();
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LParen)?;
        let mut args_name = vec![];
        let mut args_ty = vec![];
        while self.current().kind() != TokenKind::RParen {
            args_name.push(self.parse_identifier()?);
            self.expect(TokenKind::Colon)?;
            args_ty.push(self.parse_type()?);
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;
        let _ = self.expect(TokenKind::RArrow)?;
        let ret_ty = self.parse_type()?;
        let node = AstExternFunction {
            span: Span::union_span(&name.span, &ret_ty.span()),
            name: self.arena.alloc(name),
            args_name: self.arena.alloc_vec(args_name),
            args_ty: self.arena.alloc_vec(args_ty),
            ret: self.arena.alloc(ret_ty),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_import(&mut self) -> ParseResult<AstImport<'ast>> {
        let start = self.advance();

        let path = match self.current().kind() {
            TokenKind::StringLiteral(s) => s,
            _ => {
                return Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![TokenKind::StringLiteral(
                        "String Literal".to_string(),
                    )]),
                    span: SourceSpan::new(
                        SourceOffset::from(self.current().start()),
                        (self.current().end() - self.current().start()) as usize,
                    ),
                    src: self.src.clone(),
                }));
            }
        };
        let _ = self.advance();

        if let TokenKind::KwAs = self.current().kind() {
            let _ = self.advance();
            let alias = self.parse_identifier()?;
            let node = AstImport {
                span: Span::union_span(&start.span(), &alias.span),
                path: self.arena.alloc(path),
                alias: Some(self.arena.alloc(alias)),
            };
            Ok(node)
        } else {
            let node = AstImport {
                span: Span::union_span(&start.span(), &start.span()),
                path: self.arena.alloc(path),
                alias: None,
            };
            Ok(node)
        }
    }

    fn parse_struct(&mut self) -> ParseResult<AstStruct<'ast>> {
        let _ = self.advance();

        let ident = self.parse_identifier()?;

        self.expect(TokenKind::LBrace)?;

        let mut fields = vec![];
        while self.current().kind() != TokenKind::RBrace {
            fields.push(self.parse_obj_field()?);
            self.expect(TokenKind::Semicolon)?;
        }
        self.expect(TokenKind::RBrace)?;
        let node = AstStruct {
            span: Span::union_span(&ident.span, &self.current().span()),
            name: self.arena.alloc(ident),
            fields: self.arena.alloc_vec(fields),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_obj_field(&mut self) -> ParseResult<AstObjField<'ast>> {
        let name = self.parse_identifier()?;

        self.expect(TokenKind::Colon)?;

        let ty = self.parse_type()?;

        let node = AstObjField {
            vis: AstVisibility::default(),
            span: Span::union_span(&name.span, &ty.span()),
            name: self.arena.alloc(name),
            ty: self.arena.alloc(ty),
        };

        Ok(node)
    }

    fn parse_identifier(&mut self) -> ParseResult<AstIdentifier<'ast>> {
        let token = self.current();

        let node = match token.kind() {
            TokenKind::Identifier(s) => AstIdentifier {
                span: Span::union_span(&self.current().span(), &self.current().span()),
                name: self.arena.alloc(s),
            },
            _ => {
                return Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![TokenKind::Identifier(
                        "Identifier".to_string(),
                    )]),
                    span: SourceSpan::new(
                        SourceOffset::from(self.current().start()),
                        (self.current().end() - self.current().start()) as usize,
                    ),
                    src: self.src.clone(),
                }));
            }
        };
        let _ = self.advance();
        Ok(node)
    }

    ///todo: add support for += -= *= /= %= etc.
    fn parse_assign(&mut self, target: AstExpr<'ast>) -> ParseResult<AstAssignExpr<'ast>> {
        self.expect(TokenKind::OpAssign)?;
        let value = self.parse_expr()?;
        let node = AstAssignExpr {
            span: Span::union_span(&target.span(), &value.span()),
            target: self.arena.alloc(target),
            value: self.arena.alloc(value),
        };
        Ok(node)
    }

    fn parse_fn_call(&mut self, callee: AstExpr<'ast>) -> ParseResult<AstCallExpr<'ast>> {
        self.expect(TokenKind::LParen)?;

        let mut args = vec![];
        while self.current().kind() != TokenKind::RParen {
            args.push(self.parse_expr()?);
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;

        let node = AstCallExpr {
            span: Span::union_span(&callee.span(), &self.current().span()),
            callee: self.arena.alloc(callee),
            args: self.arena.alloc_vec(args),
        };
        Ok(node)
    }

    fn parse_indexing(&mut self, target: AstExpr<'ast>) -> ParseResult<AstIndexingExpr<'ast>> {
        self.expect(TokenKind::LBracket)?;

        let index = self.parse_expr()?;

        self.expect(TokenKind::RBracket)?;

        let node = AstIndexingExpr {
            span: Span::union_span(&target.span(), &self.current().span()),
            target: self.arena.alloc(target),
            index: self.arena.alloc(index),
        };
        Ok(node)
    }

    fn parse_field_access(
        &mut self,
        target: AstExpr<'ast>,
    ) -> ParseResult<AstFieldAccessExpr<'ast>> {
        self.expect(TokenKind::Dot)?;

        let field = self.parse_identifier()?;

        let node = AstFieldAccessExpr {
            span: Span::union_span(&target.span(), &field.span),
            target: self.arena.alloc(target),
            field: self.arena.alloc(field),
        };
        Ok(node)
    }

    //Todo: add List<T>/Map<K, V>/() & function types
    fn parse_type(&mut self) -> ParseResult<AstType<'ast>> {
        let token = self.current();
        let start = self.current().span();
        match token.kind() {
            TokenKind::Int64Ty => {
                let _ = self.advance();
                let node = AstType::Integer(AstIntegerType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::Float64Ty => {
                let _ = self.advance();
                let node = AstType::Float(AstFloatType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::UInt64Ty => {
                let _ = self.advance();
                let node = AstType::UnsignedInteger(AstUnsignedIntegerType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::BoolTy => {
                let _ = self.advance();
                let node = AstType::Boolean(AstBooleanType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::StrTy => {
                let _ = self.advance();
                let node = AstType::String(AstStringType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::UnitTy => {
                let _ = self.advance();
                let node = AstType::Unit(AstUnitType {
                    span: Span::union_span(&start, &self.current().span()),
                });
                Ok(node)
            }
            TokenKind::Ampersand => {
                let _ = self.advance();
                let ty = self.parse_type()?;
                let node = AstType::Pointer(AstPointerType {
                    span: Span::union_span(&start, &ty.span()),
                    inner: self.arena.alloc(ty),
                });
                Ok(node)
            }
            TokenKind::Identifier(_) => {
                let name = self.parse_identifier()?;
                let node = AstType::Named(AstNamedType {
                    span: Span::union_span(&start, &self.current().span()),
                    name: self.arena.alloc(name),
                });
                Ok(node)
            }
            TokenKind::LBracket => {
                let _ = self.advance();
                let ty = self.parse_type()?;
                self.expect(TokenKind::RBracket)?;
                let node = AstType::List(AstListType {
                    span: Span::union_span(&start, &self.current().span()),
                    inner: self.arena.alloc(ty),
                });
                Ok(node)
            }
            TokenKind::LParen => {
                let _ = self.advance();
                let mut types = vec![];
                while self.current().kind() != TokenKind::RParen {
                    types.push(self.parse_type()?);
                    if self.current().kind() == TokenKind::Comma {
                        let _ = self.advance();
                    }
                }
                self.expect(TokenKind::RParen)?;

                self.expect(TokenKind::RArrow)?;

                let ret = self.parse_type()?;

                let node = AstType::Function(AstFunctionType {
                    span: Span::union_span(&start, &self.current().span()),
                    args: self.arena.alloc_vec(types),
                    ret: self.arena.alloc(ret),
                });
                Ok(node)
            }
            _ => Err(ParseError::UnexpectedToken(UnexpectedTokenError {
                token: self.current().clone(),
                expected: TokenVec(vec![
                    TokenKind::Identifier(String::from("Type (e.g. i64, f64, str)")),
                    token.kind(),
                ]),
                span: SourceSpan::new(
                    SourceOffset::from(start.start),
                    self.current().end() - &start.start,
                ),
                src: self.src.clone(),
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use miette::Result;

    use super::*;
    use crate::atlas_frontend::lexer::AtlasLexer;

    #[test]
    fn test_parse_struct() -> Result<()> {
        let input = r#"
        import "foo.atlas" as foo
        import "std/io"
        public class Foo {
            public:
                bar: &str;
                baz: f64;
            private:
                qux: i64;
            public:
                func create() -> Foo {
                    return new Foo {
                        bar: "Hello World",
                        baz: 3.14,
                        qux: 42,
                    };
                }
        }
        //struct fields are public by default
        struct Foo {
            bar: &str;
            baz: f64;
        }
        extern print(val: &str) -> unit
        func main() -> i64 {
            let test: str = "Hello World";
            if test {
                print(test);
            } else {
                print("Goodbye World");
            }
            while test {
                break;
                continue;
                return b;
                let a: i64 = "a";
            }
            print("Hello World");
            index[0];
            0 as i64;
        }
        func cast() -> i64 {
            let a: i64 = 42;
            let b: f64 = a as f64;
            return b as i64;
        }
        "#
            .to_string();
        let mut lexer = AtlasLexer::new("<stdin>", input.clone());
        //lexer.set_source(input.to_string());
        let tokens = match lexer.tokenize() {
            Ok(tokens) => tokens,
            Err(e) => panic!("{:?}", e),
        };
        let bump = Bump::new();
        let arena = &AstArena::new(&bump);
        let mut parser = Parser::new(arena, tokens, PathBuf::from("test"), input);
        let result = parser.parse();
        match result {
            Ok(program) => {
                for item in program.items.iter() {
                    match item {
                        AstItem::Struct(s) => {
                            println!(
                                "struct {:?} ({:?})\n",
                                s.name.name,
                                s.fields
                                    .iter()
                                    .map(|f| format!("{}: {:?}", f.name.name, f.ty))
                                    .collect::<String>()
                            );
                        }
                        AstItem::Import(i) => {
                            println!("Import {:?} as {:?}\n", i.path, i.alias);
                        }
                        AstItem::ExternFunction(e) => {
                            println!(
                                "extern {:?} ({:?}) -> {:?}\n",
                                e.name.name,
                                e.args_name
                                    .iter()
                                    .zip(e.args_ty.iter())
                                    .map(|(name, ty)| format!("{:?}: {:?}", name, ty))
                                    .collect::<String>(),
                                e.ret
                            );
                        }
                        AstItem::Func(f) => {
                            println!(
                                "func {:?} ({:?}) -> {:?}\n {{\n {:?} \n}}\n",
                                f.name.name,
                                f.args
                                    .iter()
                                    .map(|a| format!("{:?}", a))
                                    .collect::<String>(),
                                f.ret,
                                f.body
                            );
                        }
                        AstItem::Class(c) => {
                            println!(
                                "class {:?} ({:?})\n {{\n {:?} \n}}\n",
                                c.name.name,
                                c.fields
                                    .iter()
                                    .map(|f| format!("{:?}", f))
                                    .collect::<String>(),
                                c.methods
                                    .iter()
                                    .map(|m| format!("{:?}", m))
                                    .collect::<String>()
                            );
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    }
}
