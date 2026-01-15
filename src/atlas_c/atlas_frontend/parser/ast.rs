use crate::atlas_c::atlas_frontend::lexer::token::TokenKind;
use crate::atlas_c::atlas_frontend::parser::arena::AstArena;
use crate::atlas_c::utils::Span;

/// An `AstProgram` is the top-level node of the AST and contains all the items.
#[derive(Debug, Clone, Copy)]
pub struct AstProgram<'ast> {
    pub items: &'ast [&'ast AstItem<'ast>],
}

/// An `Item` is anything that can be declared at the top-level scope of a program.
/// This currently means functions, classes & structs declarations
///
/// Enums & unions are also top-level items, but they are not yet supported
#[derive(Debug, Clone)]
//todo: Add classes and a trait-ish stuff
pub enum AstItem<'ast> {
    Import(AstImport<'ast>),
    Struct(AstStruct<'ast>),
    ExternFunction(AstExternFunction<'ast>),
    Function(AstFunction<'ast>),
    Enum(AstEnum<'ast>),
    Union(AstUnion<'ast>),
    Constant(AstGlobalConst<'ast>),
}

impl AstItem<'_> {
    pub fn set_vis(&mut self, vis: AstVisibility) {
        match self {
            AstItem::Import(_) => {}
            AstItem::Struct(v) => v.vis = vis,
            AstItem::ExternFunction(v) => v.vis = vis,
            AstItem::Function(v) => v.vis = vis,
            AstItem::Enum(v) => v.vis = vis,
            AstItem::Union(v) => v.vis = vis,
            AstItem::Constant(v) => v.vis = vis,
        }
    }
    pub fn set_flag(&mut self, flag: AstFlag) {
        match self {
            AstItem::Struct(v) => v.flag = flag,
            _ => {}
        }
    }
    pub fn span(&self) -> Span {
        match self {
            AstItem::Import(v) => v.span,
            AstItem::Struct(v) => v.span,
            AstItem::ExternFunction(v) => v.span,
            AstItem::Function(v) => v.span,
            AstItem::Enum(v) => v.span,
            AstItem::Union(v) => v.span,
            AstItem::Constant(v) => v.span,
        }
    }
}

impl<'ast> AstItem<'ast> {
    // If there is already a docstring, we need to push the new one before it
    pub fn set_docstring(&mut self, docstring: &'ast str, arena: &'ast AstArena<'ast>) {
        match self {
            AstItem::Struct(v) => match v.docstring {
                Some(existing) => {
                    let combined = format!("{}\n{}", docstring, existing);
                    v.docstring = Some(arena.alloc(combined));
                }
                None => {
                    v.docstring = Some(docstring);
                }
            },
            AstItem::Function(v) => match v.docstring {
                Some(existing) => {
                    let combined = format!("{}\n{}", docstring, existing);
                    v.docstring = Some(arena.alloc(combined));
                }
                None => {
                    v.docstring = Some(docstring);
                }
            },
            AstItem::ExternFunction(v) => match v.docstring {
                Some(existing) => {
                    let combined = format!("{}\n{}", docstring, existing);
                    v.docstring = Some(arena.alloc(combined));
                }
                None => {
                    v.docstring = Some(docstring);
                }
            },
            AstItem::Enum(e) => match e.docstring {
                Some(existing) => {
                    let combined = format!("{}\n{}", docstring, existing);
                    e.docstring = Some(arena.alloc(combined));
                }
                None => {
                    e.docstring = Some(docstring);
                }
            },
            AstItem::Union(u) => match u.docstring {
                Some(existing) => {
                    let combined = format!("{}\n{}", docstring, existing);
                    u.docstring = Some(arena.alloc(combined));
                }
                None => {
                    u.docstring = Some(docstring);
                }
            },
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
/// Flags that can be applied to AST nodes
/// Currently used for marking structs as copyable or non-copyable
/// e.g.:
/// ```
/// #[std::copyable]
/// struct Foo {
///   x: int64;
/// }
/// ```
/// or
/// ```
/// #[std::non_copyable]
/// struct Bar {
///  x: int64;
/// }
/// ```
pub enum AstFlag {
    Copyable(Span),
    NonCopyable(Span),
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct AstGlobalConst<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub ty: &'ast AstType<'ast>,
    pub value: &'ast AstExpr<'ast>,
    pub vis: AstVisibility,
}

#[derive(Debug, Clone)]
pub struct AstUnion<'ast> {
    pub span: Span,
    pub generics: &'ast [&'ast AstGeneric<'ast>],
    pub name: &'ast AstIdentifier<'ast>,
    pub name_span: Span,
    pub vis: AstVisibility,
    pub variants: &'ast [&'ast AstObjField<'ast>],
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstEnum<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub name_span: Span,
    pub vis: AstVisibility,
    pub variants: &'ast [&'ast AstEnumVariant<'ast>],
    pub docstring: Option<&'ast str>,
}
#[derive(Debug, Clone)]
pub struct AstEnumVariant<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    /// Currently, enum variants can only have a single unsigned integer value
    pub value: u64,
    pub docstring: Option<&'ast str>,
}

/// And ASTGeneric carries the name of the generic type as well as the constraints
///
/// Example:
/// ```
/// struct Foo<T: Display + Debug> {
///     x: T;
///     fun print(self) {
///         println(x);
///     }
/// }
/// ```
#[derive(Debug, Clone)]
pub struct AstGeneric<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    //TODO: Constraints should be somewhere else (e.g. "where" keywords like in Rust).
    pub constraints: &'ast [&'ast AstGenericConstraint<'ast>],
}

#[derive(Debug, Clone)]
pub enum AstGenericConstraint<'ast> {
    Concept(AstNamedType<'ast>),
    Operator { op: AstBinaryOp, span: Span },
    Std(AstStdGenericConstraint<'ast>),
}

#[derive(Debug, Clone)]
pub struct AstStdGenericConstraint<'ast> {
    pub span: Span,
    /// Name of the standard generic constraint (e.g., "std::copyable", it's the only one for now)
    pub name: &'ast str,
}

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub enum AstVisibility {
    Public,
    #[default]
    Private,
}
#[derive(Debug, Clone)]
pub struct AstStruct<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub name_span: Span,
    pub vis: AstVisibility,
    pub fields: &'ast [&'ast AstObjField<'ast>],
    pub field_span: Span,
    pub constructor: Option<&'ast AstConstructor<'ast>>,
    pub copy_constructor: Option<&'ast AstConstructor<'ast>>,
    pub destructor: Option<&'ast AstDestructor<'ast>>,
    pub generics: &'ast [&'ast AstGeneric<'ast>],
    pub operators: &'ast [&'ast AstOperatorOverload<'ast>],
    pub constants: &'ast [&'ast AstConst<'ast>],
    pub methods: &'ast [&'ast AstMethod<'ast>],
    // Currently only one flag supported: copyable or non-copyable
    pub flag: AstFlag,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone, Default, Copy)]
pub enum AstMethodModifier {
    /// Static method - no `this` parameter
    Static,
    /// Method that takes immutable reference to `this`
    ///
    /// e.g.: `fun get(&const this) -> T { ... }`
    Const,
    /// Method that takes mutable reference to `this`
    ///
    /// e.g.: `fun push(&this, val: T) { ... }`
    Mutable,
    /// Method that consumes ownership of `this`
    ///
    /// e.g.: `fun into_iter(this) -> Iter<T> { ... }`
    #[default]
    Consuming,
}

#[derive(Debug, Clone)]
pub struct AstOperatorOverload<'ast> {
    pub span: Span,
    //TODO: Replace AstBinaryOp with AstOverloadableOperator
    pub op: AstBinaryOp,
    pub args: &'ast [&'ast AstObjField<'ast>],
    pub body: &'ast AstBlock<'ast>,
    pub ret: &'ast AstType<'ast>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AstOverloadableOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    /// Copy constructor like in C++
    /// ```
    /// struct Foo {
    ///   x: int64;
    ///   operator Copy(&const Foo) -> Foo {
    ///     let other = &const Foo;
    ///     return new Foo(other.x);
    ///   }
    /// }
    Copy,
}

#[derive(Debug, Clone)]
pub enum ConstructorKind {
    Regular,
    Copy,
}

#[derive(Debug, Clone)]
pub struct AstConstructor<'ast> {
    pub span: Span,
    pub args: &'ast [&'ast AstObjField<'ast>],
    pub body: &'ast AstBlock<'ast>,
    pub vis: AstVisibility,
    /// Allows adding constraints on struct generics in the copy constructor
    /// TODO: Maybe it's time to separate the copy constructor from the regular one
    pub where_clause: Option<&'ast [&'ast AstGeneric<'ast>]>,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstDestructor<'ast> {
    pub span: Span,
    pub args: &'ast [&'ast AstObjField<'ast>],
    pub body: &'ast AstBlock<'ast>,
    pub vis: AstVisibility,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstMethod<'ast> {
    pub modifier: AstMethodModifier,
    pub vis: AstVisibility,
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub generics: Option<&'ast [&'ast AstGeneric<'ast>]>,
    pub args: &'ast [&'ast AstObjField<'ast>],
    pub ret: &'ast AstType<'ast>,
    pub body: &'ast AstBlock<'ast>,
    /// Optional where clause containing constraints on struct and method generics.
    /// During syntax lowering, method-level generic constraints are moved into the `generics` field as bounds.
    pub where_clause: Option<&'ast [&'ast AstGeneric<'ast>]>,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstFunction<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub generics: &'ast [&'ast AstGeneric<'ast>],
    pub args: &'ast [&'ast AstObjField<'ast>],
    pub ret: &'ast AstType<'ast>,
    pub body: &'ast AstBlock<'ast>,
    pub vis: AstVisibility,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
///todo: Rename because it's also used by functions
pub struct AstObjField<'ast> {
    pub span: Span,
    /// In a function or a struct the visibility is always public
    pub name: &'ast AstIdentifier<'ast>,
    pub ty: &'ast AstType<'ast>,
    pub vis: AstVisibility,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstExternFunction<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub generics: &'ast [&'ast AstGeneric<'ast>],
    pub args_name: &'ast [&'ast AstIdentifier<'ast>],
    pub args_ty: &'ast [&'ast AstType<'ast>],
    pub ret_ty: &'ast AstType<'ast>,
    pub vis: AstVisibility,
    pub docstring: Option<&'ast str>,
}

#[derive(Debug, Clone)]
pub struct AstImport<'ast> {
    pub span: Span,
    pub path: &'ast str,
    pub alias: Option<&'ast AstIdentifier<'ast>>,
}

#[derive(Debug, Clone)]
pub enum AstStatement<'ast> {
    Let(AstLet<'ast>),
    Const(AstConst<'ast>),
    IfElse(AstIfElseExpr<'ast>),
    Block(AstBlock<'ast>),
    While(AstWhileExpr<'ast>),
    Expr(AstExpr<'ast>),
    Return(AstReturnStmt<'ast>),
    Assign(AstAssignStmt<'ast>),
}

impl AstStatement<'_> {
    pub fn span(&self) -> Span {
        match self {
            AstStatement::Let(e) => e.span,
            AstStatement::Const(e) => e.span,
            AstStatement::IfElse(e) => e.span,
            AstStatement::Block(e) => e.span,
            AstStatement::While(e) => e.span,
            AstStatement::Expr(e) => e.span(),
            AstStatement::Return(e) => e.span,
            AstStatement::Assign(e) => e.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstConst<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub ty: &'ast AstType<'ast>,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstWhileExpr<'ast> {
    pub span: Span,
    pub condition: &'ast AstExpr<'ast>,
    pub body: &'ast AstBlock<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstAssignStmt<'ast> {
    pub span: Span,
    pub target: &'ast AstExpr<'ast>,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub enum AstExpr<'ast> {
    Lambda(AstLambdaExpr<'ast>),
    ConstExpr(AstConstExpr<'ast>),
    IfElse(AstIfElseExpr<'ast>),
    BinaryOp(AstBinaryOpExpr<'ast>),
    UnaryOp(AstUnaryOpExpr<'ast>),
    Call(AstCallExpr<'ast>),
    Literal(AstLiteral<'ast>),
    Identifier(AstIdentifier<'ast>),
    Indexing(AstIndexingExpr<'ast>),
    FieldAccess(AstFieldAccessExpr<'ast>),
    StaticAccess(AstStaticAccessExpr<'ast>),
    NewObj(AstNewObjExpr<'ast>),
    ObjLiteral(AstObjLiteralExpr<'ast>),
    Delete(AstDeleteObjExpr<'ast>),
    NewArray(AstNewArrayExpr<'ast>),
    Block(AstBlock<'ast>),
    Assign(AstAssignStmt<'ast>),
    Casting(AstCastingExpr<'ast>),
}

impl AstExpr<'_> {
    pub(crate) fn span(&self) -> Span {
        match self {
            AstExpr::Lambda(e) => e.span,
            AstExpr::ConstExpr(e) => e.span,
            AstExpr::IfElse(e) => e.span,
            AstExpr::BinaryOp(e) => e.span,
            AstExpr::UnaryOp(e) => e.span,
            AstExpr::Call(e) => e.span,
            AstExpr::Literal(e) => e.span(),
            AstExpr::Identifier(e) => e.span,
            AstExpr::Indexing(e) => e.span,
            AstExpr::FieldAccess(e) => e.span,
            AstExpr::StaticAccess(e) => e.span,
            AstExpr::NewObj(e) => e.span,
            AstExpr::ObjLiteral(e) => e.span,
            AstExpr::Delete(e) => e.span,
            AstExpr::NewArray(e) => e.span,
            AstExpr::Block(e) => e.span,
            AstExpr::Assign(e) => e.span,
            AstExpr::Casting(e) => e.span,
        }
    }
    pub(crate) fn kind(&self) -> &'static str {
        match self {
            AstExpr::Lambda(_) => "Lambda",
            AstExpr::ConstExpr(_) => "ConstExpr",
            AstExpr::IfElse(_) => "IfElse",
            AstExpr::BinaryOp(_) => "BinaryOp",
            AstExpr::UnaryOp(_) => "UnaryOp",
            AstExpr::Call(_) => "Call",
            AstExpr::Literal(_) => "Literal",
            AstExpr::Identifier(_) => "Identifier",
            AstExpr::Indexing(_) => "Indexing",
            AstExpr::FieldAccess(_) => "FieldAccess",
            AstExpr::StaticAccess(_) => "StaticAccess",
            AstExpr::NewObj(_) => "NewObj",
            AstExpr::ObjLiteral(_) => "ObjLiteral",
            AstExpr::Delete(_) => "Delete",
            AstExpr::NewArray(_) => "NewArray",
            AstExpr::Block(_) => "Block",
            AstExpr::Assign(_) => "Assign",
            AstExpr::Casting(_) => "Casting",
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstObjLiteralExpr<'ast> {
    pub span: Span,
    pub target: &'ast AstExpr<'ast>,
    pub fields: &'ast [&'ast AstObjLiteralField<'ast>],
    pub generics: &'ast [&'ast AstType<'ast>],
}

#[derive(Debug, Clone)]
pub struct AstObjLiteralField<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstDeleteObjExpr<'ast> {
    pub span: Span,
    pub target: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
/// i.e. ``5 as Float64``
pub struct AstCastingExpr<'ast> {
    pub span: Span,
    pub ty: &'ast AstType<'ast>,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstReturnStmt<'ast> {
    pub span: Span,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstBlock<'ast> {
    pub span: Span,
    pub stmts: &'ast [&'ast AstStatement<'ast>],
}

#[derive(Debug, Clone)]
pub struct AstNewObjExpr<'ast> {
    pub span: Span,
    pub ty: &'ast AstType<'ast>,
    pub args: &'ast [&'ast AstExpr<'ast>],
}

#[derive(Debug, Clone)]
pub struct AstNewArrayExpr<'ast> {
    pub span: Span,
    pub ty: &'ast AstType<'ast>,
    pub size: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstStaticAccessExpr<'ast> {
    pub span: Span,
    pub target: &'ast AstType<'ast>,
    pub field: &'ast AstIdentifier<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstFieldAccessExpr<'ast> {
    pub span: Span,
    pub target: &'ast AstExpr<'ast>,
    pub field: &'ast AstIdentifier<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstIndexingExpr<'ast> {
    pub span: Span,
    pub target: &'ast AstExpr<'ast>,
    pub index: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstCallExpr<'ast> {
    pub span: Span,
    pub callee: &'ast AstExpr<'ast>,
    pub args: &'ast [&'ast AstExpr<'ast>],
    pub generics: &'ast [&'ast AstType<'ast>],
}

#[derive(Debug, Clone)]
pub struct AstUnaryOpExpr<'ast> {
    pub span: Span,
    pub expr: &'ast AstExpr<'ast>,
    pub op: Option<AstUnaryOp>,
}

#[derive(Debug, Clone)]
pub enum AstUnaryOp {
    Neg,
    Not,
    Deref,
    AsRef,
}

#[derive(Debug, Clone)]
pub struct AstBinaryOpExpr<'ast> {
    pub span: Span,
    pub lhs: &'ast AstExpr<'ast>,
    pub rhs: &'ast AstExpr<'ast>,
    pub op: AstBinaryOp,
}

#[derive(Debug, Clone)]
pub enum AstBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

impl TryFrom<TokenKind> for AstBinaryOp {
    type Error = String;
    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(AstBinaryOp::Add),
            TokenKind::Minus => Ok(AstBinaryOp::Sub),
            TokenKind::Star => Ok(AstBinaryOp::Mul),
            TokenKind::Slash => Ok(AstBinaryOp::Div),
            TokenKind::Percent => Ok(AstBinaryOp::Mod),
            TokenKind::EqEq => Ok(AstBinaryOp::Eq),
            TokenKind::NEq => Ok(AstBinaryOp::NEq),
            TokenKind::LAngle => Ok(AstBinaryOp::Lt),
            TokenKind::LFatArrow => Ok(AstBinaryOp::Lte),
            TokenKind::RAngle => Ok(AstBinaryOp::Gt),
            TokenKind::OpGreaterThanEq => Ok(AstBinaryOp::Gte),
            TokenKind::OpAnd => Ok(AstBinaryOp::And),
            TokenKind::OpOr => Ok(AstBinaryOp::Or),
            _ => Err(format!("{:?}", value)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstIfElseExpr<'ast> {
    pub span: Span,
    pub condition: &'ast AstExpr<'ast>,
    pub body: &'ast AstBlock<'ast>,
    pub else_body: Option<&'ast AstBlock<'ast>>,
}

#[derive(Debug, Clone)]
pub struct AstLet<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub ty: Option<&'ast AstType<'ast>>,
    pub value: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstLambdaExpr<'ast> {
    pub span: Span,
    pub args: &'ast [&'ast AstIdentifier<'ast>],
    pub body: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstConstExpr<'ast> {
    pub span: Span,
    pub expr: &'ast AstExpr<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstIdentifier<'ast> {
    pub span: Span,
    pub name: &'ast str,
}

#[derive(Debug, Clone)]
pub enum AstLiteral<'ast> {
    Integer(AstIntegerLiteral),
    UnsignedInteger(AstUnsignedIntegerLiteral),
    Float(AstFloatLiteral),
    Char(AstCharLiteral),
    Unit(AstUnitLiteral),
    ThisLiteral(AstThisLiteral),
    String(AstStringLiteral<'ast>),
    Boolean(AstBooleanLiteral),
    List(AstListLiteral<'ast>),
}

impl AstLiteral<'_> {
    pub(crate) fn span(&self) -> Span {
        match self {
            AstLiteral::Integer(l) => l.span,
            AstLiteral::UnsignedInteger(l) => l.span,
            AstLiteral::Float(l) => l.span,
            AstLiteral::Char(l) => l.span,
            AstLiteral::Unit(l) => l.span,
            AstLiteral::ThisLiteral(l) => l.span,
            AstLiteral::String(l) => l.span,
            AstLiteral::Boolean(l) => l.span,
            AstLiteral::List(l) => l.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AstThisLiteral {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstCharLiteral {
    pub span: Span,
    pub value: char,
}

#[derive(Debug, Clone)]
pub struct AstUnitLiteral {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstListLiteral<'ast> {
    pub span: Span,
    pub items: &'ast [&'ast AstExpr<'ast>],
}

#[derive(Debug, Clone)]
pub struct AstBooleanLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct AstStringLiteral<'ast> {
    pub span: Span,
    pub value: &'ast str,
}

#[derive(Debug, Clone)]
pub struct AstFloatLiteral {
    pub span: Span,
    pub value: f64,
}

#[derive(Debug, Clone)]
pub struct AstUnsignedIntegerLiteral {
    pub span: Span,
    pub value: u64,
}

#[derive(Debug, Clone)]
pub struct AstIntegerLiteral {
    pub span: Span,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub enum AstType<'ast> {
    Unit(AstUnitType),
    Boolean(AstBooleanType),
    Integer(AstIntegerType),
    Float(AstFloatType),
    UnsignedInteger(AstUnsignedIntegerType),
    Char(AstCharType),
    ThisTy(AstThisType),
    String(AstStringType),
    Named(AstNamedType<'ast>),
    MutableRef(AstMutableRefType<'ast>),
    ReadOnlyRef(AstReadOnlyRefType<'ast>),
    Function(AstFunctionType<'ast>),
    Nullable(AstNullableType<'ast>),
    List(AstListType<'ast>),
    Generic(AstGenericType<'ast>),
    ExternTy(AstExternType<'ast>),
}

impl AstType<'_> {
    pub(crate) fn span(&self) -> Span {
        match self {
            AstType::Unit(t) => t.span,
            AstType::Boolean(t) => t.span,
            AstType::Integer(t) => t.span,
            AstType::Float(t) => t.span,
            AstType::UnsignedInteger(t) => t.span,
            AstType::Char(t) => t.span,
            AstType::ThisTy(t) => t.span,
            AstType::String(t) => t.span,
            AstType::Named(t) => t.span,
            AstType::MutableRef(t) => t.span,
            AstType::Function(t) => t.span,
            AstType::Nullable(t) => t.span,
            AstType::ReadOnlyRef(t) => t.span,
            AstType::List(t) => t.span,
            AstType::Generic(t) => t.span,
            AstType::ExternTy(t) => t.span,
        }
    }
}

impl<'ast> AstType<'ast> {
    pub fn name(&self) -> String {
        match self {
            AstType::Unit(_) => "unit".to_owned(),
            AstType::Boolean(_) => "bool".to_owned(),
            AstType::Integer(_) => "int64".to_owned(),
            AstType::Float(_) => "float64".to_owned(),
            AstType::UnsignedInteger(_) => "uint64".to_owned(),
            AstType::Char(_) => "char".to_owned(),
            AstType::ThisTy(_) => "This".to_owned(),
            AstType::String(_) => "string".to_owned(),
            AstType::Named(t) => t.name.name.to_owned(),
            AstType::MutableRef(t) => format!("&{}", t.inner.name()),
            AstType::Nullable(t) => format!("{}?", t.inner.name()),
            AstType::ReadOnlyRef(t) => format!("&const {}", t.inner.name()),
            AstType::List(t) => format!("[{}]", t.inner.name()),
            AstType::Generic(t) => {
                if t.inner_types.is_empty() {
                    t.name.name.to_owned()
                } else {
                    let params = t
                        .inner_types
                        .iter()
                        .map(|p| p.name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", t.name.name, params)
                }
            }
            //AstType::Function(_) => "fn".to_owned(),
            AstType::ExternTy(t) => match t.type_hint {
                Some(ty) => format!("extern_ptr<{}>", ty.name()),
                None => "extern_ptr".to_owned(),
            },
            _ => {
                panic!("Type does not have a name yet")
            }
        }
    }
}

#[derive(Debug, Clone)]

pub struct AstExternType<'ast> {
    pub span: Span,
    pub type_hint: Option<&'ast AstType<'ast>>,
}

#[derive(Debug, Clone)]
/// A nullable type in atlas has the form of `T?`
pub struct AstNullableType<'ast> {
    pub span: Span,
    pub inner: &'ast AstType<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstCharType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstThisType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstGenericType<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
    pub inner_types: &'ast [AstType<'ast>],
}

#[derive(Debug, Clone)]
///A List type in atlas as the form of `[T]`
pub struct AstListType<'ast> {
    pub span: Span,
    pub inner: &'ast AstType<'ast>,
}

#[derive(Debug, Clone)]
///todo: Add support for generic types and constraints (i.e. `T: Display`)
///
/// A function type in atlas as the form of `fn(T) -> U`
pub struct AstFunctionType<'ast> {
    pub span: Span,
    pub args: &'ast [&'ast AstType<'ast>],
    pub ret: &'ast AstType<'ast>,
}

#[derive(Debug, Clone)]
///A pointer type in atlas as the form of `&T`
pub struct AstMutableRefType<'ast> {
    pub span: Span,
    pub inner: &'ast AstType<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstReadOnlyRefType<'ast> {
    pub span: Span,
    pub inner: &'ast AstType<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstStringType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstNamedType<'ast> {
    pub span: Span,
    pub name: &'ast AstIdentifier<'ast>,
}

#[derive(Debug, Clone)]
pub struct AstIntegerType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstFloatType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstUnsignedIntegerType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstBooleanType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AstUnitType {
    pub span: Span,
}
