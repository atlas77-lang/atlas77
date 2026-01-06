use crate::atlas_c::atlas_hir::{item::HirUnion, signature::HirStructMethodModifier};

use super::{
    HirModule, HirModuleBody,
    expr::*,
    item::{HirEnum, HirFunction, HirImport, HirStruct, HirStructConstructor, HirStructMethod},
    signature::{HirFunctionSignature, HirModuleSignature, HirStructFieldSignature, HirVisibility},
    stmt::*,
    ty::HirTy,
};

pub struct HirPrettyPrinter {
    output: String,
    indent: usize,
}

impl HirPrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    pub fn print_module(&mut self, module: &HirModule) -> String {
        self.writeln("// HIR Module");
        self.writeln("// Generated after ownership pass");
        self.writeln("");

        for (name, extern_fn) in &module.signature.functions {
            if extern_fn.is_external {
                self.print_external_function(*name, extern_fn);
            }
        }

        self.print_body(&module.body);

        self.output.clone()
    }

    fn print_external_function(&mut self, name: &str, extern_fn: &HirFunctionSignature) {
        self.write("extern fun ");
        self.write(name);
        self.print_function_signature(extern_fn);
        self.writeln(";");
    }

    fn print_body(&mut self, body: &HirModuleBody) {
        self.writeln("// Module Body");

        // Print imports
        for import in &body.imports {
            self.print_import(import);
            self.writeln("");
        }

        // Print structs
        for (_, struct_def) in &body.structs {
            self.print_struct(struct_def);
            self.writeln("");
        }
        // Print unions
        for (_, union_def) in &body.unions {
            self.print_union(union_def);
            self.writeln("");
        }
        // Print enums
        for (_, enum_def) in &body.enums {
            self.print_enum(enum_def);
            self.writeln("");
        }

        // Print functions
        for (_, function) in &body.functions {
            self.print_function(function);
            self.writeln("");
        }
    }

    fn print_import(&mut self, import: &HirImport) {
        let alias_part = if let Some(alias) = import.alias {
            format!(" as {}", alias)
        } else {
            String::new()
        };
        self.writeln(&format!("import \"{}\"{};", import.path, alias_part));
    }

    fn print_struct(&mut self, struct_def: &HirStruct) {
        self.write(&format!(
            "{} struct {} ",
            self.visibility_str(struct_def.vis),
            struct_def.name
        ));

        // Type parameters (from generics in signature)
        if !struct_def.signature.generics.is_empty() {
            self.write("<");
            for (i, generic) in struct_def.signature.generics.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(generic.generic_name);
            }
            self.write(">");
        }

        self.writeln(" {");
        self.indent();

        // Fields
        if !struct_def.fields.is_empty() {
            self.writeln("// Fields");
            for field in &struct_def.fields {
                self.print_field(field);
            }
            self.writeln("");
        }

        // Constructor
        if !struct_def.constructor.body.statements.is_empty() {
            self.writeln("// Constructor");
            self.print_constructor(&format!("{}", struct_def.name), &struct_def.constructor);
            self.writeln("");
        }

        // Destructor
        if !struct_def.destructor.body.statements.is_empty() {
            self.writeln("// Destructor");
            self.print_constructor(&format!("~{}", struct_def.name), &struct_def.destructor);
            self.writeln("");
        }

        // Methods
        if !struct_def.methods.is_empty() {
            self.writeln("// Methods");
            for method in &struct_def.methods {
                self.print_method(method);
                self.writeln("");
            }
        }

        self.dedent();
        self.writeln("}");
    }

    fn print_field(&mut self, field: &HirStructFieldSignature) {
        self.writeln(&format!("{}: {};", field.name, self.type_str(field.ty)));
    }

    fn print_constructor(&mut self, name: &str, constructor: &HirStructConstructor) {
        self.write_indent();
        self.write(&format!(
            "{} {}(",
            self.visibility_str(constructor.vis),
            name
        ));

        for (i, param) in constructor.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&format!("{}: {}", param.name, self.type_str(param.ty)));
        }

        self.write(") {\n");
        self.indent();
        self.print_block(&constructor.body);
        self.dedent();
        self.writeln("}");
    }

    fn print_method(&mut self, method: &HirStructMethod) {
        self.write_indent();
        self.write(&format!("fun {}(", method.name));
        match &method.signature.modifier {
            HirStructMethodModifier::Const => self.write("&const this"),
            HirStructMethodModifier::Mutable => self.write("&this"),
            HirStructMethodModifier::None => self.write("this"),
            HirStructMethodModifier::Static => {}
        }
        for (i, param) in method.signature.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&format!("{}: {}", param.name, self.type_str(param.ty)));
        }

        self.write(")");

        if let HirTy::Unit(_) = &method.signature.return_ty {
            // Skip return type for unit
        } else {
            self.write(&format!(
                " -> {}",
                self.type_str(&method.signature.return_ty)
            ));
        }

        self.write(" {\n");
        self.indent();
        self.print_block(&method.body);
        self.dedent();
        self.writeln("}");
    }

    fn print_union(&mut self, union_def: &HirUnion) {
        self.writeln(&format!(
            "{} union {} {{",
            self.visibility_str(union_def.vis),
            union_def.name
        ));
        self.indent();

        for field in &union_def.variants {
            self.writeln(&format!("{}: {};", field.name, self.type_str(field.ty)));
        }

        self.dedent();
        self.writeln("}");
    }

    fn print_enum(&mut self, enum_def: &HirEnum) {
        self.writeln(&format!(
            "{} enum {} {{",
            self.visibility_str(enum_def.vis),
            enum_def.name
        ));
        self.indent();

        for variant in &enum_def.variants {
            self.writeln(&format!("{} = {},", variant.name, variant.value));
        }

        self.dedent();
        self.writeln("}");
    }

    fn print_function(&mut self, function: &HirFunction) {
        self.write("fun ");
        self.write(function.name);
        self.print_function_signature(function.signature);
        self.writeln(" {");
        self.indent();
        self.print_block(&function.body);
        self.dedent();
        self.writeln("}");
    }

    fn print_function_signature(&mut self, sig: &HirFunctionSignature) {
        // Type parameters
        if !sig.type_params.is_empty() {
            self.write("<");
            for (i, param) in sig.type_params.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(param.name);
            }
            self.write(">");
        }

        self.write("(");
        for (i, param) in sig.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write(&format!("{}: {}", param.name, self.type_str(param.ty)));
        }
        self.write(")");

        if let HirTy::Unit(_) = &sig.return_ty {
            // Skip return type for unit
        } else {
            self.write(&format!(" -> {}", self.type_str(&sig.return_ty)));
        }
    }

    fn print_block(&mut self, block: &HirBlock) {
        for stmt in &block.statements {
            self.print_statement(stmt);
        }
    }

    fn print_statement(&mut self, stmt: &HirStatement) {
        match stmt {
            HirStatement::_Block(block) => {
                self.writeln("{");
                self.indent();
                self.print_block(block);
                self.dedent();
                self.writeln("}");
            }
            HirStatement::Return(ret) => {
                self.write_indent();
                self.write("return ");
                self.print_expr(&ret.value);
                self.write(";\n");
            }
            HirStatement::Expr(expr_stmt) => {
                self.write_indent();
                self.print_expr(&expr_stmt.expr);
                self.write(";\n");
            }
            HirStatement::Let(var) => {
                self.write_indent();
                self.write(&format!("let {}: {} = ", var.name, self.type_str(var.ty)));
                self.print_expr(&var.value);
                self.write(";\n");
            }
            HirStatement::Const(var) => {
                self.write_indent();
                self.write(&format!("const {}: {} = ", var.name, self.type_str(var.ty)));
                self.print_expr(&var.value);
                self.write(";\n");
            }
            HirStatement::IfElse(if_else) => {
                self.write_indent();
                self.write("if ");
                self.print_expr(&if_else.condition);
                self.write(" {\n");
                self.indent();
                self.print_block(&if_else.then_branch);
                self.dedent();
                if let Some(else_branch) = &if_else.else_branch {
                    self.writeln("} else {");
                    self.indent();
                    self.print_block(else_branch);
                    self.dedent();
                }
                self.writeln("}");
            }
            HirStatement::While(while_stmt) => {
                self.write_indent();
                self.write("while ");
                self.print_expr(&while_stmt.condition);
                self.write(" {\n");
                self.indent();
                self.print_block(&while_stmt.body);
                self.dedent();
                self.writeln("}");
            }
            HirStatement::Break(_) => {
                self.writeln("break;");
            }
            HirStatement::Continue(_) => {
                self.writeln("continue;");
            }
        }
    }

    fn print_expr(&mut self, expr: &HirExpr) {
        match expr {
            HirExpr::Ident(ident) => {
                self.write(ident.name);
            }
            HirExpr::IntegerLiteral(lit) => {
                self.write(&lit.value.to_string());
            }
            HirExpr::UnsignedIntegerLiteral(lit) => {
                self.write(&format!("{}u", lit.value));
            }
            HirExpr::FloatLiteral(lit) => {
                self.write(&lit.value.to_string());
            }
            HirExpr::BooleanLiteral(lit) => {
                self.write(&lit.value.to_string());
            }
            HirExpr::CharLiteral(lit) => {
                self.write(&format!("'{}'", lit.value));
            }
            HirExpr::StringLiteral(lit) => {
                self.write(&format!("\"{}\"", lit.value));
            }
            HirExpr::UnitLiteral(_) => {
                self.write("()");
            }
            HirExpr::ThisLiteral(_) => {
                self.write("this");
            }
            HirExpr::HirBinaryOperation(bin_op) => {
                self.write("(");
                self.print_expr(&bin_op.lhs);
                self.write(&format!(" {} ", bin_op.op));
                self.print_expr(&bin_op.rhs);
                self.write(")");
            }
            HirExpr::Unary(unary) => {
                if let Some(op) = &unary.op {
                    self.write(&format!("{}", op));
                }
                self.print_expr(&unary.expr);
            }
            HirExpr::Casting(cast) => {
                self.print_expr(&cast.expr);
                self.write(&format!(" as {}", self.type_str(cast.ty)));
            }
            HirExpr::Call(call) => {
                self.print_expr(&call.callee);
                self.write("(");
                for (i, arg) in call.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expr(arg);
                }
                self.write(")");
            }
            HirExpr::Assign(assign) => {
                self.print_expr(&assign.lhs);
                self.write(" = ");
                self.print_expr(&assign.rhs);
            }
            HirExpr::FieldAccess(field) => {
                self.print_expr(&field.target);
                self.write(&format!(".{}", field.field.name));
            }
            HirExpr::Indexing(index) => {
                self.print_expr(&index.target);
                self.write("[");
                self.print_expr(&index.index);
                self.write("]");
            }
            HirExpr::ListLiteral(list) => {
                self.write("[");
                for (i, elem) in list.items.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expr(elem);
                }
                self.write("]");
            }
            HirExpr::NewArray(new_array) => {
                let ty = match new_array.ty {
                    HirTy::List(l) => l.inner,
                    _ => panic!("NewArray must have List type"),
                };
                self.write(&format!("new [{}; ", self.type_str(ty)));
                self.print_expr(&new_array.size);
                self.write("]");
            }
            HirExpr::NewObj(new_obj) => {
                self.write(&format!("new {}(", self.type_str(new_obj.ty)));
                for (i, arg) in new_obj.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_expr(arg);
                }
                self.write(")");
            }
            HirExpr::ObjLiteral(obj_lit) => {
                self.write(&format!("{} {{ ", self.type_str(obj_lit.ty)));
                for (i, field_init) in obj_lit.fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!("{}: ", field_init.name));
                    self.print_expr(&field_init.value);
                }
                self.write(" }");
            }
            HirExpr::Delete(delete) => {
                self.write("delete ");
                self.print_expr(&delete.expr);
            }
            HirExpr::StaticAccess(static_access) => {
                self.write(&format!(
                    "{}::{}",
                    self.type_str(static_access.target),
                    static_access.field.name
                ));
            }
            HirExpr::Move(move_expr) => {
                self.write("move<>(");
                self.print_expr(&move_expr.expr);
                self.write(")");
            }
            HirExpr::Copy(copy_expr) => {
                self.write("copy<>(");
                self.print_expr(&copy_expr.expr);
                self.write(")");
            }
        }
    }

    fn type_str(&self, ty: &HirTy) -> String {
        match ty {
            HirTy::Int64(_) => "int64".to_string(),
            HirTy::Float64(_) => "float64".to_string(),
            HirTy::UInt64(_) => "uint64".to_string(),
            HirTy::Boolean(_) => "bool".to_string(),
            HirTy::Char(_) => "char".to_string(),
            HirTy::String(_) => "string".to_string(),
            HirTy::Unit(_) => "unit".to_string(),
            HirTy::Named(n) => n.name.to_string(),
            HirTy::List(l) => format!("[{}]", self.type_str(l.inner)),
            HirTy::ReadOnlyReference(r) => format!("&const {}", self.type_str(r.inner)),
            HirTy::MutableReference(r) => format!("&{}", self.type_str(r.inner)),
            HirTy::Generic(g) => format!(
                "{}<{}>",
                g.name.to_string(),
                g.inner
                    .iter()
                    .map(|arg| self.type_str(arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HirTy::Uninitialized(_) => "<uninit>".to_string(),
            HirTy::Nullable(n) => format!("{}?", self.type_str(n.inner)),
            HirTy::ExternTy(e) => format!("extern {:?}", e.type_hint),
            HirTy::Function(f) => {
                let params = f
                    .params
                    .iter()
                    .map(|p| self.type_str(p))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fun({}) -> {}", params, self.type_str(f.ret_ty))
            }
        }
    }

    fn visibility_str(&self, vis: HirVisibility) -> &'static str {
        match vis {
            HirVisibility::Public => "public",
            HirVisibility::Private => "",
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }
}
