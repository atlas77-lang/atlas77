//todo: Add Parser::sync() to recover from errors
pub mod arena;
pub mod ast;
pub mod error;

use std::path::PathBuf;

use miette::{SourceOffset, SourceSpan};

use crate::atlas_c::atlas_frontend::parser::error::{
    NoFieldInStructError, OnlyOneConstructorAllowedError, ParseResult, SyntaxError,
    UnexpectedTokenError,
};
use ast::{
    AstAssignExpr, AstBinaryOp, AstBinaryOpExpr, AstBlock, AstBooleanLiteral, AstBooleanType,
    AstCallExpr, AstConst, AstExpr, AstExternFunction, AstFieldAccessExpr, AstFloatLiteral,
    AstFloatType, AstFunction, AstFunctionType, AstIdentifier, AstIfElseExpr, AstImport,
    AstIntegerLiteral, AstIntegerType, AstItem, AstLet, AstLiteral, AstNamedType, AstObjField,
    AstPointerType, AstProgram, AstReturnStmt, AstStatement, AstStringLiteral, AstStringType,
    AstType, AstUnaryOp, AstUnaryOpExpr, AstUnitType, AstUnsignedIntegerLiteral,
    AstUnsignedIntegerType, AstWhileExpr,
};

use crate::atlas_c::atlas_frontend::lexer::{
    token::{Token, TokenKind}, Spanned,
    TokenVec,
};
use crate::atlas_c::atlas_frontend::parser::ast::{
    AstCastingExpr, AstCharLiteral, AstCharType, AstConstructor, AstConstructorExpr,
    AstDeleteObjExpr, AstDestructor, AstFieldInit, AstGeneric, AstGenericConstraint,
    AstGenericType, AstIndexingExpr, AstListLiteral, AstListType, AstMethod, AstMethodModifier,
    AstNewArrayExpr, AstNewObjExpr, AstNoneLiteral, AstNullType, AstNullableType,
    AstOperatorOverload, AstReadOnlyType, AstSelfLiteral, AstStaticAccessExpr, AstStruct,
    AstThisType, AstUnitLiteral, AstVisibility,
};
use crate::atlas_c::utils::Span;
use arena::AstArena;

pub(crate) struct Parser<'ast> {
    arena: &'ast AstArena<'ast>,
    tokens: Vec<Token>,
    //for error reporting
    _file_path: PathBuf,
    pos: usize,
    src: String,
}

pub fn remove_comments(tokens: Vec<Token>) -> Vec<Token> {
    tokens
        .into_iter()
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
            Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                token: tok.clone(),
                expected: TokenVec(vec![kind]),
                span: SourceSpan::new(SourceOffset::from(tok.start()), tok.end() - tok.start()),
                src: self.src.clone(),
            }))
        }
    }

    fn eat_while<F, T>(&mut self, kind: TokenKind, f: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser<'ast>) -> ParseResult<T>,
    {
        let mut items = Vec::new();
        while self.current().kind() == kind {
            let _ = self.advance();
            items.push(f(self)?);
        }
        Ok(items)
    }

    fn eat_until<F, T>(&mut self, kind: TokenKind, f: F) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Parser<'ast>) -> ParseResult<T>,
    {
        let mut items = Vec::new();
        while self.current().kind() != kind {
            items.push(f(self)?);
        }
        Ok(items)
    }

    fn eat_if<F, T>(&mut self, kind: TokenKind, f: F, or: T) -> ParseResult<T>
    where
        F: Fn(&mut Parser<'ast>) -> ParseResult<T>,
    {
        if self.current().kind() == kind {
            let _ = self.advance();
            f(self)
        } else {
            Ok(or)
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
            //TokenKind::KwStruct => Ok(AstItem::Struct(self.parse_struct()?)),
            TokenKind::KwImport => Ok(AstItem::Import(self.parse_import()?)),
            TokenKind::KwExtern => Ok(AstItem::ExternFunction(self.parse_extern_function()?)),
            TokenKind::KwFunc => Ok(AstItem::Function(self.parse_func()?)),
            TokenKind::KwStruct => Ok(AstItem::Struct(self.parse_struct()?)),
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
            _ => Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                token: self.current().clone(),
                expected: TokenVec(vec![TokenKind::Identifier("Item".to_string())]),
                span: SourceSpan::new(SourceOffset::from(start), self.current().end() - start),
                src: self.src.clone(),
            })),
        }
    }

    fn parse_constructor(&mut self, class_name: String) -> ParseResult<AstConstructor<'ast>> {
        self.expect(TokenKind::Identifier(class_name))?;
        self.expect(TokenKind::LParen)?;
        let mut params = vec![];
        while self.current().kind() != TokenKind::RParen {
            params.push(self.parse_obj_field()?);
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;
        let body = self.parse_block()?;
        let node = AstConstructor {
            span: Span::union_span(&params.first().unwrap().span, &body.span),
            args: self.arena.alloc_vec(params),
            body: self.arena.alloc(body),
        };
        Ok(node)
    }
    fn parse_destructor(&mut self, class_name: String) -> ParseResult<AstDestructor<'ast>> {
        self.expect(TokenKind::Tilde)?;
        self.expect(TokenKind::Identifier(class_name))?;
        self.expect(TokenKind::LParen)?;
        let mut params = vec![];
        while self.current().kind() != TokenKind::RParen {
            params.push(self.parse_obj_field()?);
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;
        let body = self.parse_block()?;
        let node = AstDestructor {
            span: Span::union_span(&body.span, &body.span),
            args: self.arena.alloc_vec(params),
            body: self.arena.alloc(body),
        };
        Ok(node)
    }

    fn parse_struct(&mut self) -> ParseResult<AstStruct<'ast>> {
        self.expect(TokenKind::KwStruct)?;
        let struct_identifier = self.parse_identifier()?;

        let generics = self.eat_if(
            TokenKind::LAngle,
            |p| {
                let value = p.eat_until(TokenKind::RAngle, |parser| {
                    parser.eat_if(TokenKind::Comma, |_| Ok(()), ())?;
                    parser.parse_generic()
                });
                p.expect(TokenKind::RAngle)?;
                value
            },
            vec![],
        )?;

        self.expect(TokenKind::LBrace)?;
        let mut fields = vec![];
        let mut constructor: Option<&'ast AstConstructor<'ast>> = None;
        let mut destructor: Option<&'ast AstDestructor<'ast>> = None;
        let mut methods = vec![];
        let mut operators = vec![];
        let mut constants = vec![];
        let mut curr_vis = self.parse_current_vis(AstVisibility::Private)?;
        while self.current().kind() != TokenKind::RBrace {
            curr_vis = self.parse_current_vis(curr_vis)?;
            match self.current().kind() {
                TokenKind::KwConst => {
                    //TODO: Add const functions (i.e. `const func foo() { ... }`)
                    constants.push(self.parse_const()?);
                    self.expect(TokenKind::Semicolon)?;
                }
                TokenKind::KwOperator => {
                    operators.push(self.parse_operator()?);
                }
                TokenKind::KwFunc => {
                    let mut method = self.parse_method()?;
                    method.vis = curr_vis;
                    methods.push(method);
                }
                TokenKind::Identifier(s) => {
                    if s == struct_identifier.name {
                        if constructor.is_none() {
                            constructor =
                                Some(self.arena.alloc(
                                    self.parse_constructor(struct_identifier.name.to_owned())?,
                                ));
                        } else {
                            //We still parse it so we can give a better error message and recover later
                            let bad_constructor =
                                self.parse_constructor(struct_identifier.name.to_owned())?;
                            return Err(SyntaxError::OnlyOneConstructorAllowed(
                                OnlyOneConstructorAllowedError {
                                    span: SourceSpan::new(
                                        SourceOffset::from(bad_constructor.span.start),
                                        bad_constructor.span.end - bad_constructor.span.start,
                                    ),
                                    src: self.src.clone(),
                                },
                            ));
                        }
                    } else {
                        let mut obj_field = self.parse_obj_field()?;
                        obj_field.vis = curr_vis;
                        fields.push(obj_field);
                        self.expect(TokenKind::Semicolon)?;
                    }
                }
                TokenKind::Tilde => {
                    if destructor.is_none() {
                        destructor = Some(
                            self.arena
                                .alloc(self.parse_destructor(struct_identifier.name.to_owned())?),
                        );
                    } else {
                        //We still parse it so we can give a better error message and recover later
                        let bad_destructor =
                            self.parse_destructor(struct_identifier.name.to_owned())?;
                        return Err(SyntaxError::OnlyOneConstructorAllowed(
                            OnlyOneConstructorAllowedError {
                                span: SourceSpan::new(
                                    SourceOffset::from(bad_destructor.span.start),
                                    bad_destructor.span.end - bad_destructor.span.start,
                                ),
                                src: self.src.clone(),
                            },
                        ));
                    }
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                        token: self.current().clone(),
                        expected: TokenVec(vec![TokenKind::Identifier(
                            "Field/Methods/Constant/Operator".to_string(),
                        )]),
                        span: SourceSpan::new(
                            SourceOffset::from(self.current().start()),
                            self.current().end() - self.current().start(),
                        ),
                        src: self.src.clone(),
                    }));
                }
            }
        }

        let end = self.expect(TokenKind::RBrace)?.span();

        if fields.is_empty() {
            return Err(SyntaxError::NoFieldInStruct(NoFieldInStructError {
                span: SourceSpan::new(
                    SourceOffset::from(struct_identifier.span.start),
                    end.end - struct_identifier.span.start,
                ),
                src: self.src.clone(),
            }));
        }

        let node = AstStruct {
            span: Span::union_span(&struct_identifier.span, &self.current().span()),
            name_span: struct_identifier.span.clone(),
            name: self.arena.alloc(struct_identifier),
            field_span: Span::union_span(
                &fields.first().unwrap().span,
                &fields.last().unwrap().span,
            ),
            fields: self.arena.alloc_vec(fields),
            constructor,
            destructor,
            generics: self.arena.alloc_vec(generics),
            methods: self.arena.alloc_vec(methods),
            operators: self.arena.alloc_vec(operators),
            constants: self.arena.alloc_vec(constants),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_method(&mut self) -> ParseResult<AstMethod<'ast>> {
        let _ = self.advance();
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LParen)?;
        let mut params = vec![];

        let mut modifier = AstMethodModifier::Static;
        if self.current().kind() != TokenKind::RParen {
            let obj_field = self.parse_obj_field()?;
            if let AstType::ThisTy(_) = obj_field.ty {
                modifier = AstMethodModifier::None;
            } else {
                params.push(obj_field);
            }
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }

        while self.current().kind() != TokenKind::RParen {
            let obj_field = self.parse_obj_field()?;
            if let AstType::ThisTy(_) = obj_field.ty {
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![TokenKind::Identifier("Field".to_string())]),
                    span: SourceSpan::new(
                        SourceOffset::from(self.current().start()),
                        self.current().end() - self.current().start(),
                    ),
                    src: self.src.clone(),
                }));
            } else {
                params.push(obj_field);
            }
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
        let node = AstMethod {
            modifier,
            span: Span::union_span(&name.span, &body.span),
            name: self.arena.alloc(name),
            args: self.arena.alloc_vec(params),
            ret: self.arena.alloc(ret_ty),
            body: self.arena.alloc(body),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_generic(&mut self) -> ParseResult<AstGeneric<'ast>> {
        let name = self.parse_identifier()?;
        let mut constraints = vec![];
        if self.current().kind() == TokenKind::Colon {
            let _ = self.advance();
            // example: `T: Foo + Bar + Baz, G: Foo + Display`
            while self.current().kind() != TokenKind::Comma {
                let constraint = match self.current().kind() {
                    TokenKind::KwOperator => {
                        let _ = self.advance();
                        self.expect(TokenKind::DoubleColon)?;
                        self.expect(TokenKind::LParen)?;
                        let op = match self.current().kind().try_into() {
                            Ok(op) => op,
                            Err(_) => {
                                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                                    token: self.current().clone(),
                                    expected: TokenVec(vec![TokenKind::Identifier(
                                        "Operator".to_string(),
                                    )]),
                                    span: SourceSpan::new(
                                        SourceOffset::from(self.current().start()),
                                        self.current().end() - self.current().start(),
                                    ),
                                    src: self.src.clone(),
                                }));
                            }
                        };
                        let _ = self.advance();
                        self.expect(TokenKind::RParen)?;
                        AstGenericConstraint::Operator(op)
                    }
                    TokenKind::Identifier(_) => {
                        let ast_ty = match self.parse_type()? {
                            AstType::Named(ast_ty) => ast_ty,
                            _ => {
                                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                                    token: self.current().clone(),
                                    expected: TokenVec(vec![TokenKind::Identifier(
                                        "Named Type".to_string(),
                                    )]),
                                    span: SourceSpan::new(
                                        SourceOffset::from(self.current().start()),
                                        self.current().end() - self.current().start(),
                                    ),
                                    src: self.src.clone(),
                                }));
                            }
                        };

                        AstGenericConstraint::NamedType(ast_ty)
                    }
                    _ => {
                        return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                            token: self.current().clone(),
                            expected: TokenVec(vec![TokenKind::Identifier(
                                "Constraint".to_string(),
                            )]),
                            span: SourceSpan::new(
                                SourceOffset::from(self.current().start()),
                                self.current().end() - self.current().start(),
                            ),
                            src: self.src.clone(),
                        }));
                    }
                };
                constraints.push(constraint);
                if self.current().kind() == TokenKind::Plus {
                    let _ = self.advance();
                } else {
                    break;
                }
            }
        }

        Ok(AstGeneric {
            span: name.span.clone(),
            name: self.arena.alloc(name),
            constraints: self.arena.alloc_vec(constraints),
        })
    }

    fn parse_operator(&mut self) -> ParseResult<AstOperatorOverload<'ast>> {
        self.expect(TokenKind::KwOperator)?;
        let tok_op = self.current().clone();
        let op = match tok_op.kind().try_into() {
            Ok(op) => op,
            Err(_) => {
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: tok_op.clone(),
                    expected: TokenVec(vec![TokenKind::Identifier("Operator".to_string())]),
                    span: SourceSpan::new(
                        SourceOffset::from(tok_op.start()),
                        tok_op.end() - tok_op.start(),
                    ),
                    src: self.src.clone(),
                }));
            }
        };
        let _ = self.advance();
        self.expect(TokenKind::LParen)?;
        let mut params = vec![];
        while self.current().kind() != TokenKind::RParen {
            params.push(self.parse_obj_field()?);
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::RArrow)?;
        let ret_ty = self.parse_type()?;
        let body = self.parse_block()?;
        let node = AstOperatorOverload {
            span: Span::union_span(&tok_op.span(), &body.span),
            op,
            args: self.arena.alloc_vec(params),
            body: self.arena.alloc(body),
            ret: self.arena.alloc(ret_ty),
        };
        Ok(node)
    }

    fn parse_current_vis(&mut self, previous_vis: AstVisibility) -> ParseResult<AstVisibility> {
        match self.current().kind() {
            TokenKind::KwPublic => {
                self.expect(TokenKind::KwPublic)?;
                self.expect(TokenKind::Colon)?;
                Ok(AstVisibility::Public)
            }
            TokenKind::KwPrivate => {
                self.expect(TokenKind::KwPrivate)?;
                self.expect(TokenKind::Colon)?;
                Ok(AstVisibility::Private)
            }
            _ => Ok(previous_vis),
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
        let start = self.expect(TokenKind::LBrace)?.span;
        let mut stmts = vec![];
        while self.current().kind() != TokenKind::RBrace {
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(TokenKind::RBrace)?.span;
        let span = if !stmts.is_empty() {
            Span::union_span(
                &stmts.first().unwrap().span(),
                &stmts.last().unwrap().span(),
            )
        } else {
            Span::union_span(&start, &end)
        };

        let node = AstBlock {
            span,
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
            ty: self.arena.alloc(ty),
            value: self.arena.alloc(value),
        };
        Ok(node)
    }

    fn parse_binary(&mut self) -> ParseResult<AstExpr<'ast>> {
        let left = self.parse_factor()?;
        match self.current().kind() {
            TokenKind::Plus | TokenKind::Minus => {
                let op = match self.current().kind() {
                    TokenKind::Plus => AstBinaryOp::Add,
                    TokenKind::Minus => AstBinaryOp::Sub,
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
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                let op = match self.current().kind() {
                    TokenKind::Star => AstBinaryOp::Mul,
                    TokenKind::Slash => AstBinaryOp::Div,
                    TokenKind::Percent => AstBinaryOp::Mod,
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
            TokenKind::EqEq
            | TokenKind::NEq
            | TokenKind::RAngle
            | TokenKind::OpGreaterThanEq
            | TokenKind::LAngle
            | TokenKind::LFatArrow => {
                let op = match self.current().kind() {
                    TokenKind::EqEq => AstBinaryOp::Eq,
                    TokenKind::NEq => AstBinaryOp::NEq,
                    TokenKind::RAngle => AstBinaryOp::Gt,
                    TokenKind::OpGreaterThanEq => AstBinaryOp::Gte,
                    TokenKind::LAngle => AstBinaryOp::Lt,
                    TokenKind::LFatArrow => AstBinaryOp::Lte,
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
            TokenKind::Minus => {
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

        let none = String::from("none");

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
            TokenKind::Char(c) => {
                let node = AstExpr::Literal(AstLiteral::Char(AstCharLiteral {
                    span: tok.span(),
                    value: c,
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
            TokenKind::KwNew => self.parse_new_obj()?,
            TokenKind::KwDelete => self.parse_delete_obj()?,
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
            TokenKind::KwThis => {
                let node =
                    AstExpr::Literal(AstLiteral::SelfLiteral(AstSelfLiteral { span: tok.span() }));
                let _ = self.expect(TokenKind::KwThis)?;
                self.parse_ident_access(node)?
            }
            TokenKind::KwNull => {
                let node = AstExpr::Literal(AstLiteral::None(AstNoneLiteral { span: tok.span() }));
                let _ = self.advance();
                node
            }
            TokenKind::Identifier(i) => {
                let node = AstExpr::Identifier(self.parse_identifier()?);

                self.parse_ident_access(node)?
            }
            TokenKind::KwIf => AstExpr::IfElse(self.parse_if_expr()?),
            _ => {
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: tok.clone(),
                    expected: TokenVec(vec![TokenKind::Identifier(
                        "Primary expression".to_string(),
                    )]),
                    span: SourceSpan::new(SourceOffset::from(tok.start()), tok.end() - tok.start()),
                    src: self.src.clone(),
                }));
            }
        };
        Ok(node)
    }

    fn parse_delete_obj(&mut self) -> ParseResult<AstExpr<'ast>> {
        let start = self.advance();
        let expr = self.parse_expr()?;
        let node: AstExpr<'_> = AstExpr::Delete(AstDeleteObjExpr {
            span: Span::union_span(&start.span(), &expr.span()),
            target: self.arena.alloc(expr),
        });
        Ok(node)
    }

    fn parse_constructor_expr(
        &mut self,
        struct_name: AstIdentifier<'ast>,
    ) -> ParseResult<AstConstructorExpr<'ast>> {
        self.expect(TokenKind::LBrace)?;
        let mut fields = vec![];
        while self.current().kind() != TokenKind::RBrace {
            let field_name = self.parse_identifier()?;
            self.expect(TokenKind::OpAssign)?;
            let value = self.parse_expr()?;
            fields.push(AstFieldInit {
                span: Span::union_span(&field_name.span, &value.span()),
                name: self.arena.alloc(field_name),
                value: self.arena.alloc(value),
            });
            if self.current().kind() == TokenKind::Comma {
                let _ = self.advance();
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span;
        let node = AstConstructorExpr {
            span: Span::union_span(&struct_name.span, &end),
            ty: self.arena.alloc(struct_name),
            fields: self.arena.alloc_vec(fields),
        };
        Ok(node)
    }

    fn parse_ident_access(&mut self, origin: AstExpr<'ast>) -> ParseResult<AstExpr<'ast>> {
        let mut node = origin;
        while self.peek().is_some() {
            match self.current().kind() {
                TokenKind::LParen => {
                    node = AstExpr::Call(self.parse_fn_call(node)?);
                }
                TokenKind::LBracket => {
                    node = AstExpr::Indexing(self.parse_indexing(node)?);
                }
                //Struct base constructor, i.e.: `Foo { bar = 42, baz = 69 }`
                TokenKind::LBrace => {
                    if let AstExpr::Identifier(ident) = node.clone() {
                        //Check if the identifier starts with an uppercase letter, if not, it's not a constructor
                        //Not a good fix tbf, but it works for now
                        if !ident.name.chars().next().unwrap().is_uppercase() {
                            break;
                        }
                        node = AstExpr::Constructor(self.parse_constructor_expr(ident)?);
                    } else {
                        //Just a break because it's not a parse error, just not a constructor
                        break;
                    }
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
        Ok(node)
    }

    fn parse_static_access(
        &mut self,
        node: AstExpr<'ast>,
    ) -> ParseResult<AstStaticAccessExpr<'ast>> {
        self.expect(TokenKind::DoubleColon)?;
        let field = self.parse_identifier()?;
        if let AstExpr::Identifier(i) = node.clone() {
            let node = AstStaticAccessExpr {
                span: Span::union_span(&node.span(), &field.span),
                target: self.arena.alloc(i),
                field: self.arena.alloc(field),
            };
            Ok(node)
        } else {
            Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                token: self.current().clone(),
                expected: TokenVec(vec![TokenKind::Identifier("Identifier".to_string())]),
                span: SourceSpan::new(
                    SourceOffset::from(self.current().start()),
                    self.current().end() - self.current().start(),
                ),
                src: self.src.clone(),
            }))
        }
    }

    fn parse_new_obj(&mut self) -> ParseResult<AstExpr<'ast>> {
        self.expect(TokenKind::KwNew)?;
        match self.current().kind() {
            TokenKind::Identifier(_) => {
                let ty = self.parse_type()?;

                self.expect(TokenKind::LParen)?;
                let mut args = vec![];

                while self.current().kind() != TokenKind::RParen {
                    let arg = self.parse_expr()?;
                    if self.current().kind() == TokenKind::Comma {
                        let _ = self.advance();
                    }
                    args.push(arg);
                }
                self.expect(TokenKind::RParen)?;
                let node = AstExpr::NewObj(AstNewObjExpr {
                    span: Span::union_span(&ty.span(), &self.current().span()),
                    ty: self.arena.alloc(ty),
                    args: self.arena.alloc_vec(args),
                });

                Ok(node)
            }
            TokenKind::LBracket => {
                self.expect(TokenKind::LBracket)?;
                let ty = self.parse_type()?;
                self.expect(TokenKind::Semicolon)?;
                let size = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                let node = AstExpr::NewArray(AstNewArrayExpr {
                    span: Span::union_span(&ty.span(), &size.span()),
                    ty: self.arena.alloc(AstType::List(AstListType {
                        span: ty.span(),
                        inner: self.arena.alloc(ty),
                    })),
                    size: self.arena.alloc(size),
                });
                Ok(node)
            }
            _ => Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                token: self.current().clone(),
                expected: TokenVec(vec![
                    TokenKind::Identifier("Identifier".to_string()),
                    TokenKind::LBrace,
                ]),
                span: SourceSpan::new(
                    SourceOffset::from(self.current().start()),
                    self.current().end() - self.current().start(),
                ),
                src: self.src.clone(),
            })),
        }
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
        if self.current().kind == TokenKind::Semicolon {
            let node = AstReturnStmt {
                span: self.current().span(),
                value: self
                    .arena
                    .alloc(AstExpr::Literal(AstLiteral::Unit(AstUnitLiteral {
                        span: self.current().span(),
                    }))),
            };
            self.expect(TokenKind::Semicolon)?;
            return Ok(node);
        }
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

        let mut generics = None;
        //Start of generic with `fn foo<T>() -> T`
        if self.current().kind == TokenKind::LAngle {
            self.expect(TokenKind::LAngle)?;
            let mut generic_names = vec![];
            while self.current().kind != TokenKind::RAngle {
                let generic_name = self.parse_identifier()?;
                generic_names.push(AstNamedType {
                    span: self.current().span(),
                    name: self.arena.alloc(generic_name),
                });
                if self.current().kind == TokenKind::Comma {
                    let _ = self.advance();
                }
            }
            self.expect(TokenKind::RAngle)?;
            generics = Some(self.arena.alloc_vec(generic_names));
        }

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
        let ret_ty;
        if self.current().kind() == TokenKind::RArrow {
            let _ = self.expect(TokenKind::RArrow)?;
            ret_ty = self.parse_type()?;
        } else {
            ret_ty = AstType::Unit(AstUnitType {
                span: self.current().span(),
            })
        }
        self.expect(TokenKind::Semicolon)?;
        let node = AstExternFunction {
            span: Span::union_span(&name.span, &ret_ty.span()),
            name: self.arena.alloc(name),
            generics,
            args_name: self.arena.alloc_vec(args_name),
            args_ty: self.arena.alloc_vec(args_ty),
            ret_ty: self.arena.alloc(ret_ty),
            vis: AstVisibility::default(),
        };
        Ok(node)
    }

    fn parse_import(&mut self) -> ParseResult<AstImport<'ast>> {
        let start = self.advance();

        let path = match self.current().kind() {
            TokenKind::StringLiteral(s) => s,
            _ => {
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![TokenKind::StringLiteral(
                        "String Literal".to_string(),
                    )]),
                    span: SourceSpan::new(
                        SourceOffset::from(self.current().start()),
                        self.current().end() - self.current().start(),
                    ),
                    src: self.src.clone(),
                }));
            }
        };
        let end = self.advance();

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
                span: Span::union_span(&start.span(), &end.span()),
                path: self.arena.alloc(path),
                alias: None,
            };
            Ok(node)
        }
    }

    fn parse_obj_field(&mut self) -> ParseResult<AstObjField<'ast>> {
        if self.current().kind == TokenKind::KwThis {
            self.expect(TokenKind::KwThis)?;
            let name = AstIdentifier {
                span: self.current().span.clone(),
                name: self.arena.alloc("this"),
            };
            let node = AstObjField {
                vis: AstVisibility::Public,
                span: self.current().span.clone(),
                name: self.arena.alloc(name.clone()),
                ty: self.arena.alloc(AstType::ThisTy(AstThisType {
                    span: name.span.clone(),
                })),
            };
            return Ok(node);
        }
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
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![TokenKind::Identifier("Identifier".to_string())]),
                    span: SourceSpan::new(
                        SourceOffset::from(self.current().start()),
                        self.current().end() - self.current().start(),
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
        let mut generic_types = vec![];
        //Turbo fish operator (i.e. `Vector::<i32>()`)
        if self.current().kind == TokenKind::DoubleColon {
            self.expect(TokenKind::DoubleColon)?;
            if self.current().kind == TokenKind::LAngle {
                let _ = self.advance();
                while self.current().kind != TokenKind::RAngle {
                    generic_types.push(self.parse_type()?);
                    if self.current().kind == TokenKind::Comma {
                        let _ = self.advance();
                    }
                }
                self.expect(TokenKind::RAngle)?;
            }
        }
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
            generics: self.arena.alloc_vec(generic_types),
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
    //TODO: THIS DOESN'T SUPPORT GENERIC TYPES WTF?
    //It crashes on Array[T] for example... fix it
    fn parse_type(&mut self) -> ParseResult<AstType<'ast>> {
        let token = self.current();
        let start = self.current().span();
        let ty = match token.kind() {
            TokenKind::KwConst => {
                let _ = self.advance();
                let inner = self.parse_type()?;
                AstType::ReadOnly(AstReadOnlyType {
                    span: Span::union_span(&start, &self.current().span()),
                    inner: self.arena.alloc(inner),
                })
            }
            TokenKind::Int64Ty => {
                let _ = self.advance();
                AstType::Integer(AstIntegerType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::Float64Ty => {
                let _ = self.advance();
                AstType::Float(AstFloatType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::UInt64Ty => {
                let _ = self.advance();
                AstType::UnsignedInteger(AstUnsignedIntegerType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::CharTy => {
                let _ = self.advance();
                AstType::Char(AstCharType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::BoolTy => {
                let _ = self.advance();
                AstType::Boolean(AstBooleanType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::StrTy => {
                let _ = self.advance();
                AstType::String(AstStringType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::UnitTy => {
                let _ = self.advance();
                AstType::Unit(AstUnitType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::ThisTy => {
                let _ = self.advance();
                AstType::ThisTy(AstThisType {
                    span: Span::union_span(&start, &self.current().span()),
                })
            }
            TokenKind::KwNull => {
                let _ = self.advance();
                AstType::Null(AstNullType {
                    span: self.current().span(),
                })
            }
            TokenKind::Ampersand => {
                let _ = self.advance();
                let ty = self.parse_type()?;
                let node = AstType::Pointer(AstPointerType {
                    span: Span::union_span(&start, &ty.span()),
                    inner: self.arena.alloc(ty),
                });
                node
            }
            TokenKind::Identifier(_) => {
                let name = self.parse_identifier()?;
                let node = if self.current().kind == TokenKind::LAngle {
                    //Manage generics i.e. `Foo[T, E, Array[B, T], ...]`
                    self.expect(TokenKind::LAngle)?;
                    let mut inner_types = vec![];
                    while self.current().kind() != TokenKind::RAngle {
                        inner_types.push(self.parse_type()?);
                        if self.current().kind() == TokenKind::Comma {
                            let _ = self.advance();
                        }
                    }
                    let _ = self.advance();
                    let end = self.current().span();
                    AstType::Generic(AstGenericType {
                        span: Span::union_span(&start, &end),
                        name: self.arena.alloc(name),
                        inner_types: self.arena.alloc(inner_types),
                    })
                } else {
                    AstType::Named(AstNamedType {
                        span: Span::union_span(&start, &self.current().span()),
                        name: self.arena.alloc(name),
                    })
                };
                node
            }
            TokenKind::LBracket => {
                let _ = self.advance();
                let ty = self.parse_type()?;
                self.expect(TokenKind::RBracket)?;
                let node = AstType::List(AstListType {
                    span: Span::union_span(&start, &self.current().span()),
                    inner: self.arena.alloc(ty),
                });
                node
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
                node
            }
            _ => {
                return Err(SyntaxError::UnexpectedToken(UnexpectedTokenError {
                    token: self.current().clone(),
                    expected: TokenVec(vec![
                        TokenKind::Identifier(String::from(
                            "Int64, Float64, UInt64, Char, T?, Str & (T) -> T",
                        )),
                        token.kind(),
                    ]),
                    span: SourceSpan::new(
                        SourceOffset::from(start.start),
                        self.current().end() - start.start,
                    ),
                    src: self.src.clone(),
                }));
            }
        };
        let node = if self.current().kind == TokenKind::Interrogation {
            let _ = self.advance();

            AstType::Nullable(AstNullableType {
                span: Span::union_span(&start, &self.current().span()),
                inner: self.arena.alloc(ty),
            })
        } else {
            ty
        };
        Ok(node)
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use miette::Result;

    use super::*;
    use crate::atlas_c::atlas_frontend::lexer::AtlasLexer;

    #[test]
    fn test_parse_struct() -> Result<()> {
        let input = r#"
import "std/io"

public struct Vector<T> {
    private:
        data: [T];
    public:
        length: uint64;
        capacity: uint64;

    Vector(data: [T]) {
        this.length = len(data);
        this.capacity = self.length;
        this.data = data;
    }
    ~Vector() {
        this.data = 0;
        this.length = 0;
        delete this.data;
    }

    /// A function that doesn't have a "self" parameter is a static function
    /// static functions are called like this: "Vector::<T>::function_name()"
    fun with_capacity(capacity: uint64) -> Vector<T> {
        return new Vector<T>(new [T; capacity]);
    }

    /// A function that has a "self" parameter is a method of the struct
    /// and is called like this: "vector_instance.function_name()"
    fun push(this, val: T) {
        if this.capacity <= self.length {
            let new_capacity = this.capacity * 2;

            let new_data = new [T; new_capacity];
            let i = 0;
            while i < this.length {
                new_data[i] = this.data[i];
                i = i + 1;
            }
            let data_to_delete = this.data;
            this.data = new_data;
            this.capacity = new_capacity;

            delete data_to_delete;
        }
        this.data[self.length] = val;
        this.length = this.length + 1;
    }

    fun pop(this) -> T {
        if this.length == 0 {
            std::panic("Cannot pop from an empty vector");
        }
        this.length = this.length - 1;
        return this.data[this.length];
    }
}
fun main() {
    let vec = new Vector<int64>([1, 2, 3, 4, 5]);
    vec.push(6);
    let val = vec.pop();
    print("Popped value: " + val);
}
"#
            .to_string();
        let mut lexer = AtlasLexer::new("<stdin>".into(), input.clone());
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
                                "struct {} ({:?}) {{\n {:?} \n}}\n",
                                s.name.name,
                                s.fields
                                    .iter()
                                    .map(|f| format!("{}: {:?}", f.name.name, f.ty))
                                    .collect::<String>(),
                                s.methods
                                    .iter()
                                    .map(|m| format!(
                                        "fun {}({:?}): {:?}",
                                        m.name.name,
                                        m.args
                                            .iter()
                                            .map(|func| format!(
                                                "{:?}: {:?}",
                                                func.name.name, func.ty
                                            ))
                                            .collect::<String>(),
                                        m.ret
                                    ))
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
                                e.ret_ty
                            );
                        }
                        AstItem::Function(f) => {
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
                        _ => {}
                    }
                }
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    }
}
