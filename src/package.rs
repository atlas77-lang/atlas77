use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lang_c::ast::{
    Declaration, DeclarationSpecifier, Declarator, DeclaratorKind, DerivedDeclarator, Ellipsis,
    EnumType, ExternalDeclaration, InitDeclarator, ParameterDeclaration, SpecifierQualifier,
    StorageClassSpecifier, StructDeclaration, StructField, StructType, TypeSpecifier,
};
use lang_c::driver::{Config, parse};
use lang_c::loc::get_location_for_offset;
use lang_c::span::Span;
use lang_c::visit::{Visit, visit_type_specifier};

#[derive(Debug, Clone)]
struct TypeAlias {
    name: String,
    source_decl: String,
    alias_name_span_rel: (usize, usize),
    typedef_ref_spans_rel: Vec<(usize, usize, String)>,
    tag_name_spans_rel: Vec<(usize, usize, String)>,
    enum_constant_spans_rel: Vec<(usize, usize, String)>,
}

#[derive(Debug, Clone)]
struct FunctionDecl {
    name: String,
    namespaced_name: String,
    atlas_signature: String,
    source_decl: String,
    function_name_span_rel: (usize, usize),
    is_void_return: bool,
    typedef_ref_spans_rel: Vec<(usize, usize, String)>,
    param_conversions: Vec<ParamConversion>,
    return_conversion: ReturnConversion,
}

#[derive(Debug, Clone)]
struct ParamConversion {
    arg_name: String,
    vendor_type: Option<String>,
    pointer_depth: usize,
}

#[derive(Debug, Clone)]
struct ReturnConversion {
    vendor_type: Option<String>,
    pointer_depth: usize,
}

#[derive(Debug, Clone)]
pub struct SkipInfo {
    pub name: String,
    pub reason: String,
}

#[derive(Debug, Clone)]
pub struct PackageResult {
    pub shim_header: PathBuf,
    pub shim_c: PathBuf,
    pub atlas_module: PathBuf,
    pub skipped: Vec<SkipInfo>,
}

#[derive(Debug, Clone)]
enum CompositeKind {
    Struct,
    Union,
}

#[derive(Debug, Clone)]
struct CompositeField {
    name: String,
    ty: String,
}

#[derive(Debug, Clone)]
struct CompositeDecl {
    kind: CompositeKind,
    name: String,
    fields: Vec<CompositeField>,
}

#[derive(Debug, Clone)]
struct EnumVariant {
    name: String,
    value: Option<String>,
}

#[derive(Debug, Clone)]
struct EnumDecl {
    name: String,
    variants: Vec<EnumVariant>,
}

pub fn package_c_header(
    header: &str,
    namespace: Option<&str>,
    output_dir: Option<&str>,
) -> miette::Result<PackageResult> {
    let header_path = canonicalize_existing(Path::new(header))?;
    let out_dir = if let Some(dir) = output_dir {
        PathBuf::from(dir)
    } else {
        header_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."))
    };
    std::fs::create_dir_all(&out_dir).map_err(|err| {
        miette::miette!("failed to create output dir {}: {}", out_dir.display(), err)
    })?;

    let stem = header_path
        .file_stem()
        .and_then(|v| v.to_str())
        .unwrap_or("header")
        .to_owned();
    let namespace = namespace.unwrap_or(&stem).to_owned();

    let mut cfg = Config::default();
    cfg.cpp_options.push("-D__TINYC__=1".to_owned());
    cfg.cpp_options.push("-Dbool=_Bool".to_owned());
    cfg.cpp_options.push("-Dtrue=1".to_owned());
    cfg.cpp_options.push("-Dfalse=0".to_owned());
    // Normalize compiler-specific attributes/calling conventions so lang-c can
    // parse system headers (notably MinGW/UCRT headers pulled by vendor APIs).
    cfg.cpp_options.push("-D__cdecl=".to_owned());
    cfg.cpp_options.push("-D__stdcall=".to_owned());
    cfg.cpp_options.push("-D__fastcall=".to_owned());
    cfg.cpp_options.push("-D__thiscall=".to_owned());
    cfg.cpp_options.push("-D__vectorcall=".to_owned());
    cfg.cpp_options.push("-D__declspec(...)= ".to_owned());
    cfg.cpp_options.push("-D__attribute__(...)= ".to_owned());
    cfg.cpp_options
        .push("-D__MINGW_ATTRIB_NORETURN=".to_owned());
    cfg.cpp_options.push("-D__MINGW_EXTENSION=".to_owned());
    cfg.cpp_options.push(format!(
        "-I{}",
        header_path.parent().unwrap_or(Path::new(".")).display()
    ));

    let parsed = parse(&cfg, &header_path).map_err(|err| {
        miette::miette!(
            "failed to parse C header via lang-c for {}: {}",
            header_path.display(),
            err
        )
    })?;

    let mut type_aliases: Vec<TypeAlias> = Vec::new();
    let mut functions: Vec<FunctionDecl> = Vec::new();
    let mut composites: Vec<CompositeDecl> = Vec::new();
    let mut enums: Vec<EnumDecl> = Vec::new();
    let mut skipped: Vec<SkipInfo> = Vec::new();

    for ext in &parsed.unit.0 {
        let ExternalDeclaration::Declaration(decl_node) = &ext.node else {
            continue;
        };
        if !belongs_to_header(&parsed.source, ext.span.start, &header_path) {
            continue;
        }

        let decl = &decl_node.node;
        let decl_start = decl_node.span.start;
        let source_decl = slice_source(&parsed.source, decl_node.span.start, decl_node.span.end);

        collect_composites_and_enums(
            decl,
            &parsed.source,
            &namespace,
            &mut composites,
            &mut enums,
            &mut skipped,
        );

        let typedef_ref_spans_rel = collect_typedef_ref_spans_decl(decl, decl_start);
        let tag_name_spans_rel = collect_tag_name_spans_decl(decl, decl_start);
        let enum_constant_spans_rel = collect_enum_constant_spans_decl(decl, decl_start);

        if is_typedef_decl(decl) {
            for init_decl in &decl.declarators {
                let Some((name, abs_span)) =
                    declarator_identifier_node(&init_decl.node.declarator.node)
                else {
                    continue;
                };
                type_aliases.push(TypeAlias {
                    name: name.clone(),
                    source_decl: source_decl.clone(),
                    alias_name_span_rel: (
                        abs_span.0.saturating_sub(decl_start),
                        abs_span.1.saturating_sub(decl_start),
                    ),
                    typedef_ref_spans_rel: typedef_ref_spans_rel
                        .iter()
                        .filter(|(s, e, _)| {
                            !(*s == abs_span.0.saturating_sub(decl_start)
                                && *e == abs_span.1.saturating_sub(decl_start))
                        })
                        .cloned()
                        .collect(),
                    tag_name_spans_rel: tag_name_spans_rel.clone(),
                    enum_constant_spans_rel: enum_constant_spans_rel.clone(),
                });
            }
            continue;
        }

        for init_decl in &decl.declarators {
            let Some((
                name,
                fn_span_abs,
                has_varargs,
                arg_names,
                atlas_signature,
                is_void_return,
                param_conversions,
                return_conversion,
            )) = extract_function_decl(decl, &init_decl.node, &namespace)
            else {
                continue;
            };

            if has_varargs {
                skipped.push(SkipInfo {
                    name,
                    reason: "variadic function (unsafe forwarding)".to_owned(),
                });
                continue;
            }

            if arg_names.iter().any(String::is_empty) {
                skipped.push(SkipInfo {
                    name,
                    reason: "unnamed function parameter not supported for wrapper generation"
                        .to_owned(),
                });
                continue;
            }

            functions.push(FunctionDecl {
                namespaced_name: namespaced(&namespace, &name),
                name,
                atlas_signature,
                source_decl: source_decl.clone(),
                function_name_span_rel: (
                    fn_span_abs.0.saturating_sub(decl_start),
                    fn_span_abs.1.saturating_sub(decl_start),
                ),
                is_void_return,
                typedef_ref_spans_rel: typedef_ref_spans_rel.clone(),
                param_conversions,
                return_conversion,
            });
        }
    }

    let known_type_names: HashSet<String> = type_aliases.iter().map(|v| v.name.clone()).collect();
    let alias_chain_map = resolve_alias_chains(&type_aliases);

    let shim_header_name = format!("atlas77-{}.h", stem);
    let shim_c_name = format!("atlas77-{}.c", stem);
    let atlas_module_name = format!("{}.atlas", stem);
    let shim_header_path = out_dir.join(&shim_header_name);
    let shim_c_path = out_dir.join(&shim_c_name);
    let atlas_module_path = out_dir.join(&atlas_module_name);

    let mut header_out = String::new();
    header_out.push_str("#pragma once\n\n");
    header_out.push_str("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");

    for alias in &type_aliases {
        let mut replacements: Vec<(usize, usize, String)> = Vec::new();
        replacements.push((
            alias.alias_name_span_rel.0,
            alias.alias_name_span_rel.1,
            namespaced(&namespace, &alias.name),
        ));
        for (start, end, ref_name) in &alias.typedef_ref_spans_rel {
            if known_type_names.contains(ref_name) {
                replacements.push((*start, *end, namespaced(&namespace, ref_name)));
            }
        }
        for (start, end, tag_name) in &alias.tag_name_spans_rel {
            replacements.push((*start, *end, namespaced(&namespace, tag_name)));
        }
        for (start, end, enum_name) in &alias.enum_constant_spans_rel {
            replacements.push((*start, *end, namespaced(&namespace, enum_name)));
        }
        let rewritten = apply_replacements(&alias.source_decl, replacements);
        header_out.push_str(&rewritten);
        if !rewritten.ends_with('\n') {
            header_out.push('\n');
        }
    }

    if !type_aliases.is_empty() {
        header_out.push('\n');
    }

    for func in &functions {
        let rewritten =
            rewrite_function_decl(func, &known_type_names, &alias_chain_map, &namespace);
        header_out.push_str(&rewritten);
        if !rewritten.ends_with('\n') {
            header_out.push('\n');
        }
    }

    header_out.push_str("\n#ifdef __cplusplus\n}\n#endif\n");

    let mut c_out = String::new();
    let vendor_header_name = header_path
        .file_name()
        .and_then(|v| v.to_str())
        .unwrap_or("vendor.h");
    c_out.push_str(&format!("#include \"{}\"\n", vendor_header_name));
    c_out.push_str(&format!("#include \"{}\"\n\n", shim_header_name));

    for func in &functions {
        let rewritten =
            rewrite_function_decl(func, &known_type_names, &alias_chain_map, &namespace);
        let signature = rewritten.trim_end().trim_end_matches(';').trim_end();
        let forwarded_args = func
            .param_conversions
            .iter()
            .map(|param| build_forward_arg_expr(param, &known_type_names, &namespace))
            .collect::<Vec<_>>()
            .join(", ");
        let call_expr = format!("{}({})", func.name, forwarded_args);

        c_out.push_str(signature);
        c_out.push_str(" {\n");
        if func.is_void_return {
            c_out.push_str(&format!("    {};\n", call_expr));
            c_out.push_str("}\n\n");
        } else {
            let return_expr = build_return_expr(
                &call_expr,
                &func.return_conversion,
                &known_type_names,
                &namespace,
            );
            c_out.push_str(&format!("    return {};\n", return_expr));
            c_out.push_str("}\n\n");
        }
    }

    std::fs::write(&shim_header_path, header_out).map_err(|err| {
        miette::miette!(
            "failed to write shim header {}: {}",
            shim_header_path.display(),
            err
        )
    })?;
    std::fs::write(&shim_c_path, c_out).map_err(|err| {
        miette::miette!("failed to write shim C {}: {}", shim_c_path.display(), err)
    })?;

    let atlas_out = generate_atlas_module(&namespace, &functions, &composites, &enums, &skipped);
    std::fs::write(&atlas_module_path, atlas_out).map_err(|err| {
        miette::miette!(
            "failed to write atlas module {}: {}",
            atlas_module_path.display(),
            err
        )
    })?;

    Ok(PackageResult {
        shim_header: shim_header_path,
        shim_c: shim_c_path,
        atlas_module: atlas_module_path,
        skipped,
    })
}

fn generate_atlas_module(
    namespace: &str,
    functions: &[FunctionDecl],
    composites: &[CompositeDecl],
    enums: &[EnumDecl],
    skipped: &[SkipInfo],
) -> String {
    let mut out = String::new();
    out.push_str(&format!("namespace {} {{\n", namespace));

    let mut seen_composites: HashSet<(String, &'static str)> = HashSet::new();
    for comp in composites {
        let kind_name = match comp.kind {
            CompositeKind::Struct => "struct",
            CompositeKind::Union => "union",
        };
        if !seen_composites.insert((comp.name.clone(), kind_name)) {
            continue;
        }

        out.push_str(&format!("    public extern {} {}\n", kind_name, comp.name));
        out.push_str("    {\n");
        out.push_str("    public:\n");
        for field in &comp.fields {
            out.push_str(&format!("        {}: {};\n", field.name, field.ty));
        }
        out.push_str("    }\n\n");
    }

    let mut seen_enums: HashSet<String> = HashSet::new();
    for en in enums {
        if !seen_enums.insert(en.name.clone()) {
            continue;
        }

        out.push_str(&format!("    public enum {} {{\n", en.name));
        for variant in &en.variants {
            match &variant.value {
                Some(v) => out.push_str(&format!("        {} = {};\n", variant.name, v.trim())),
                None => out.push_str(&format!("        {};\n", variant.name)),
            }
        }
        out.push_str("    }\n\n");
    }

    for func in functions {
        out.push_str("    public extern ");
        out.push_str(&func.atlas_signature);
        out.push('\n');
    }

    if !skipped.is_empty() {
        out.push_str("\n    // Skipped by package command:\n");
        for item in skipped {
            out.push_str(&format!("    // - {}: {}\n", item.name, item.reason));
        }
    }

    out.push_str("}\n");
    out
}

fn atlas_function_signature(
    name: &str,
    params: &[lang_c::span::Node<ParameterDeclaration>],
    return_specifiers: &[lang_c::span::Node<DeclarationSpecifier>],
    return_pointer_depth: usize,
    namespace: &str,
) -> String {
    let mut sig = String::new();
    sig.push_str("fun ");
    sig.push_str(name);
    sig.push('(');
    sig.push_str(&atlas_params(params, namespace).join(", "));
    sig.push(')');
    let ret = atlas_return_type(return_specifiers, return_pointer_depth, namespace);
    if ret != "unit" {
        sig.push_str(" -> ");
        sig.push_str(&ret);
    }
    sig.push(';');
    sig
}

fn atlas_params(
    params: &[lang_c::span::Node<ParameterDeclaration>],
    namespace: &str,
) -> Vec<String> {
    let mut out = Vec::new();
    for (idx, parameter) in params.iter().enumerate() {
        if parameter.node.declarator.is_none() && is_void_parameter(&parameter.node) {
            continue;
        }

        let base = atlas_type_from_specifiers(&parameter.node.specifiers, namespace);
        let pointer_depth = parameter
            .node
            .declarator
            .as_ref()
            .map(|decl| declarator_pointer_depth(&decl.node))
            .unwrap_or(0);
        let ty = pointer_wrap(base, pointer_depth);

        let name = parameter
            .node
            .declarator
            .as_ref()
            .and_then(|decl| declarator_identifier_node(&decl.node).map(|(n, _)| n))
            .unwrap_or_else(|| format!("arg_{}", idx));

        out.push(format!("{}: {}", name, ty));
    }
    out
}

fn atlas_return_type(
    return_specifiers: &[lang_c::span::Node<DeclarationSpecifier>],
    pointer_depth: usize,
    namespace: &str,
) -> String {
    let base = atlas_type_from_specifiers(return_specifiers, namespace);
    pointer_wrap(base, pointer_depth)
}

fn c_type_string_to_atlas(c_type: &str, namespace: &str) -> String {
    let raw = c_type.trim();
    if raw.is_empty() {
        return "unit".to_owned();
    }

    let mut ty = raw.replace("const ", "");
    ty = ty.replace("volatile ", "");
    ty = ty.replace("restrict ", "");
    ty = ty.trim().to_owned();

    let pointer_depth = ty.chars().filter(|ch| *ch == '*').count();
    let mut base = ty.replace('*', "");
    base = base.replace("  ", " ");
    base = base.trim().to_owned();

    let mapped = match base.as_str() {
        "void" => "unit".to_owned(),
        "char" => "uint8".to_owned(),
        "unsigned char" => "uint8".to_owned(),
        "signed char" => "int8".to_owned(),
        "short" | "short int" | "signed short" | "signed short int" => "int16".to_owned(),
        "unsigned short" | "unsigned short int" => "uint16".to_owned(),
        "int" | "signed" | "signed int" => "int32".to_owned(),
        "unsigned" | "unsigned int" => "uint32".to_owned(),
        "long" | "long int" | "signed long" | "signed long int" => "int64".to_owned(),
        "unsigned long" | "unsigned long int" => "uint64".to_owned(),
        "long long" | "long long int" | "signed long long" | "signed long long int" => {
            "int64".to_owned()
        }
        "unsigned long long" | "unsigned long long int" => "uint64".to_owned(),
        "float" => "float32".to_owned(),
        "double" => "float64".to_owned(),
        "_Bool" | "bool" => "bool".to_owned(),
        other => format!("{}::{}", namespace, other),
    };

    if pointer_depth == 0 {
        return mapped;
    }

    let mut out = mapped;
    for _ in 0..pointer_depth {
        out = format!("*{}", out);
    }
    out
}

fn collect_composites_and_enums(
    decl: &Declaration,
    source: &str,
    namespace: &str,
    composites: &mut Vec<CompositeDecl>,
    enums: &mut Vec<EnumDecl>,
    skipped: &mut Vec<SkipInfo>,
) {
    let typedef_alias_name = if is_typedef_decl(decl) {
        decl.declarators
            .iter()
            .find_map(|v| declarator_identifier_node(&v.node.declarator.node).map(|(n, _)| n))
    } else {
        None
    };

    for spec in &decl.specifiers {
        let DeclarationSpecifier::TypeSpecifier(ty) = &spec.node else {
            continue;
        };

        match &ty.node {
            TypeSpecifier::Struct(st) => {
                if let Some(comp) =
                    build_composite_from_struct(st, typedef_alias_name.as_deref(), namespace)
                {
                    composites.push(comp);
                }
            }
            TypeSpecifier::Enum(en) => {
                if let Some(e) =
                    build_enum_from_enum_type(en, typedef_alias_name.as_deref(), source)
                {
                    enums.push(e);
                }
            }
            _ => {}
        }
    }

    for init in &decl.declarators {
        let decl_node = &init.node.declarator.node;
        if contains_function_pointer(decl_node)
            && let Some((name, _)) = declarator_identifier_node(decl_node)
        {
            skipped.push(SkipInfo {
                name,
                reason: "function-pointer declaration emits as *uint8 in .atlas".to_owned(),
            });
        }
    }
}

fn build_composite_from_struct(
    st: &lang_c::span::Node<StructType>,
    fallback_name: Option<&str>,
    namespace: &str,
) -> Option<CompositeDecl> {
    let kind = match st.node.kind.node {
        lang_c::ast::StructKind::Struct => CompositeKind::Struct,
        lang_c::ast::StructKind::Union => CompositeKind::Union,
    };
    let name = st
        .node
        .identifier
        .as_ref()
        .map(|v| v.node.name.clone())
        .or_else(|| fallback_name.map(str::to_owned))?;

    let mut fields: Vec<CompositeField> = Vec::new();
    if let Some(decls) = &st.node.declarations {
        for field_decl in decls {
            collect_fields_from_struct_decl(&field_decl.node, namespace, &mut fields);
        }
    }

    Some(CompositeDecl { kind, name, fields })
}

fn collect_fields_from_struct_decl(
    field_decl: &StructDeclaration,
    namespace: &str,
    out: &mut Vec<CompositeField>,
) {
    let StructDeclaration::Field(field_node) = field_decl else {
        return;
    };
    collect_fields_from_struct_field(&field_node.node, namespace, out);
}

fn collect_fields_from_struct_field(
    field: &StructField,
    namespace: &str,
    out: &mut Vec<CompositeField>,
) {
    let base = atlas_type_from_specifier_qualifiers(&field.specifiers, namespace);
    for declarator in &field.declarators {
        let Some(named_decl) = &declarator.node.declarator else {
            continue;
        };
        let Some((name, _)) = declarator_identifier_node(&named_decl.node) else {
            continue;
        };
        let pointer_depth = declarator_pointer_depth(&named_decl.node);
        out.push(CompositeField {
            name,
            ty: pointer_wrap(base.clone(), pointer_depth),
        });
    }
}

fn build_enum_from_enum_type(
    en: &lang_c::span::Node<EnumType>,
    fallback_name: Option<&str>,
    source: &str,
) -> Option<EnumDecl> {
    let name = en
        .node
        .identifier
        .as_ref()
        .map(|v| v.node.name.clone())
        .or_else(|| fallback_name.map(str::to_owned))?;

    let variants = en
        .node
        .enumerators
        .iter()
        .map(|entry| EnumVariant {
            name: entry.node.identifier.node.name.clone(),
            value: entry
                .node
                .expression
                .as_ref()
                .map(|expr| slice_source(source, expr.span.start, expr.span.end)),
        })
        .collect();

    Some(EnumDecl { name, variants })
}

fn atlas_type_from_specifiers(
    specifiers: &[lang_c::span::Node<DeclarationSpecifier>],
    namespace: &str,
) -> String {
    let mut words: Vec<String> = Vec::new();

    for spec in specifiers {
        match &spec.node {
            DeclarationSpecifier::TypeSpecifier(ty) => match &ty.node {
                TypeSpecifier::Void => words.push("void".to_owned()),
                TypeSpecifier::Char => words.push("char".to_owned()),
                TypeSpecifier::Short => words.push("short".to_owned()),
                TypeSpecifier::Int => words.push("int".to_owned()),
                TypeSpecifier::Long => words.push("long".to_owned()),
                TypeSpecifier::Float => words.push("float".to_owned()),
                TypeSpecifier::Double => words.push("double".to_owned()),
                TypeSpecifier::Signed => words.push("signed".to_owned()),
                TypeSpecifier::Unsigned => words.push("unsigned".to_owned()),
                TypeSpecifier::Bool => words.push("bool".to_owned()),
                TypeSpecifier::TypedefName(id) => words.push(id.node.name.clone()),
                TypeSpecifier::Struct(st) => {
                    if let Some(id) = &st.node.identifier {
                        words.push(id.node.name.clone());
                    }
                }
                TypeSpecifier::Enum(en) => {
                    if let Some(id) = &en.node.identifier {
                        words.push(id.node.name.clone());
                    }
                }
                _ => {}
            },
            DeclarationSpecifier::TypeQualifier(_)
            | DeclarationSpecifier::Function(_)
            | DeclarationSpecifier::Alignment(_)
            | DeclarationSpecifier::Extension(_) => {}
            DeclarationSpecifier::StorageClass(_) => {}
        }
    }

    c_type_string_to_atlas(&words.join(" "), namespace)
}

fn atlas_type_from_specifier_qualifiers(
    specifiers: &[lang_c::span::Node<SpecifierQualifier>],
    namespace: &str,
) -> String {
    let mut words: Vec<String> = Vec::new();
    for spec in specifiers {
        match &spec.node {
            SpecifierQualifier::TypeSpecifier(ty) => match &ty.node {
                TypeSpecifier::Void => words.push("void".to_owned()),
                TypeSpecifier::Char => words.push("char".to_owned()),
                TypeSpecifier::Short => words.push("short".to_owned()),
                TypeSpecifier::Int => words.push("int".to_owned()),
                TypeSpecifier::Long => words.push("long".to_owned()),
                TypeSpecifier::Float => words.push("float".to_owned()),
                TypeSpecifier::Double => words.push("double".to_owned()),
                TypeSpecifier::Signed => words.push("signed".to_owned()),
                TypeSpecifier::Unsigned => words.push("unsigned".to_owned()),
                TypeSpecifier::Bool => words.push("bool".to_owned()),
                TypeSpecifier::TypedefName(id) => words.push(id.node.name.clone()),
                TypeSpecifier::Struct(st) => {
                    if let Some(id) = &st.node.identifier {
                        words.push(id.node.name.clone());
                    }
                }
                TypeSpecifier::Enum(en) => {
                    if let Some(id) = &en.node.identifier {
                        words.push(id.node.name.clone());
                    }
                }
                _ => {}
            },
            SpecifierQualifier::TypeQualifier(_) | SpecifierQualifier::Extension(_) => {}
        }
    }
    c_type_string_to_atlas(&words.join(" "), namespace)
}

fn pointer_wrap(base: String, pointer_depth: usize) -> String {
    if pointer_depth == 0 {
        return base;
    }

    let mut out = base;
    for _ in 0..pointer_depth {
        out = format!("*{}", out);
    }
    out
}

fn contains_function_pointer(declarator: &Declarator) -> bool {
    if declarator
        .derived
        .iter()
        .any(|d| matches!(d.node, DerivedDeclarator::Function(_)))
        && declarator
            .derived
            .iter()
            .any(|d| matches!(d.node, DerivedDeclarator::Pointer(_)))
    {
        return true;
    }

    if let DeclaratorKind::Declarator(inner) = &declarator.kind.node {
        return contains_function_pointer(&inner.node);
    }
    false
}

fn rewrite_function_decl(
    func: &FunctionDecl,
    known_type_names: &HashSet<String>,
    alias_chain_map: &HashMap<String, String>,
    namespace: &str,
) -> String {
    let mut replacements: Vec<(usize, usize, String)> = Vec::new();
    replacements.push((
        func.function_name_span_rel.0,
        func.function_name_span_rel.1,
        func.namespaced_name.clone(),
    ));
    for (start, end, ref_name) in &func.typedef_ref_spans_rel {
        if known_type_names.contains(ref_name) {
            let resolved_name = alias_chain_map
                .get(ref_name)
                .cloned()
                .unwrap_or_else(|| ref_name.clone());
            replacements.push((*start, *end, namespaced(namespace, &resolved_name)));
        }
    }
    apply_replacements(&func.source_decl, replacements)
}

fn canonicalize_existing(path: &Path) -> miette::Result<PathBuf> {
    let canonical = path
        .canonicalize()
        .map_err(|err| miette::miette!("failed to resolve {}: {}", path.display(), err))?;
    Ok(normalize_windows_verbatim_path(&canonical))
}

fn belongs_to_header(source: &str, start: usize, header: &Path) -> bool {
    let (loc, _) = get_location_for_offset(source, start);
    if loc.file.is_empty() {
        return false;
    }

    let file_path = normalize_windows_verbatim_path(&PathBuf::from(loc.file));
    if let Ok(canonical_file) = file_path.canonicalize()
        && canonical_file == header
    {
        return true;
    }

    file_path.file_name() == header.file_name()
}

fn slice_source(source: &str, start: usize, end: usize) -> String {
    source.get(start..end).unwrap_or("").to_owned()
}

fn is_typedef_decl(decl: &Declaration) -> bool {
    decl.specifiers.iter().any(|s| {
        matches!(
            s.node,
            DeclarationSpecifier::StorageClass(ref storage)
                if storage.node == StorageClassSpecifier::Typedef
        )
    })
}

fn declarator_identifier_node(declarator: &Declarator) -> Option<(String, (usize, usize))> {
    match &declarator.kind.node {
        DeclaratorKind::Identifier(ident) => {
            Some((ident.node.name.clone(), (ident.span.start, ident.span.end)))
        }
        DeclaratorKind::Declarator(inner) => declarator_identifier_node(&inner.node),
        DeclaratorKind::Abstract => None,
    }
}

type FunDecl = (
    String,
    (usize, usize),
    bool,
    Vec<String>,
    String,
    bool,
    Vec<ParamConversion>,
    ReturnConversion,
);

fn extract_function_decl(
    decl: &Declaration,
    init_decl: &InitDeclarator,
    namespace: &str,
) -> Option<FunDecl> {
    if init_decl.initializer.is_some() {
        return None;
    }

    let declarator = &init_decl.declarator.node;
    let (func_decl, func_name, func_span, has_ptr_in_signature) =
        function_declarator_info(declarator)?;
    let has_varargs = func_decl.ellipsis == Ellipsis::Some;

    let mut arg_names: Vec<String> = Vec::new();
    let mut param_conversions: Vec<ParamConversion> = Vec::new();
    for (idx, parameter) in func_decl.parameters.iter().enumerate() {
        let vendor_type = first_typedef_name_in_specifiers(&parameter.node.specifiers);
        let Some(param_decl) = &parameter.node.declarator else {
            if is_void_parameter(&parameter.node) {
                continue;
            }
            let generated = format!("arg_{}", idx);
            arg_names.push(generated.clone());
            param_conversions.push(ParamConversion {
                arg_name: generated,
                vendor_type,
                pointer_depth: 0,
            });
            continue;
        };
        let Some((name, _)) = declarator_identifier_node(&param_decl.node) else {
            arg_names.push(String::new());
            continue;
        };
        arg_names.push(name);
        param_conversions.push(ParamConversion {
            arg_name: arg_names.last().cloned().unwrap_or_default(),
            vendor_type,
            pointer_depth: declarator_pointer_depth(&param_decl.node),
        });
    }

    let return_pointer_depth = function_return_pointer_depth(declarator);

    let is_void_return = decl.specifiers.iter().any(|spec| {
        matches!(
            spec.node,
            DeclarationSpecifier::TypeSpecifier(ref ty) if ty.node == TypeSpecifier::Void
        )
    }) && !has_ptr_in_signature
        && return_pointer_depth == 0;

    let return_conversion = ReturnConversion {
        vendor_type: first_typedef_name_in_specifiers(&decl.specifiers),
        pointer_depth: return_pointer_depth,
    };

    let atlas_signature = atlas_function_signature(
        &func_name,
        &func_decl.parameters,
        &decl.specifiers,
        return_pointer_depth,
        namespace,
    );

    Some((
        func_name,
        func_span,
        has_varargs,
        arg_names,
        atlas_signature,
        is_void_return,
        param_conversions,
        return_conversion,
    ))
}

fn function_declarator_info(
    declarator: &Declarator,
) -> Option<(
    lang_c::ast::FunctionDeclarator,
    String,
    (usize, usize),
    bool,
)> {
    let (identifier, ident_span) = declarator_identifier_node(declarator)?;
    let mut has_pointer = false;
    let mut function_part: Option<lang_c::ast::FunctionDeclarator> = None;

    for derived in &declarator.derived {
        match &derived.node {
            DerivedDeclarator::Pointer(_) => {
                has_pointer = true;
            }
            DerivedDeclarator::Function(function) => {
                function_part = Some(function.node.clone());
                break;
            }
            DerivedDeclarator::Array(_)
            | DerivedDeclarator::KRFunction(_)
            | DerivedDeclarator::Block(_) => {}
        }
    }

    function_part.map(|func| (func, identifier, ident_span, has_pointer))
}

fn is_void_parameter(param: &ParameterDeclaration) -> bool {
    param.specifiers.iter().any(|spec| {
        matches!(spec.node, DeclarationSpecifier::TypeSpecifier(ref ty) if ty.node == TypeSpecifier::Void)
    })
}

fn first_typedef_name_in_specifiers(
    specifiers: &[lang_c::span::Node<DeclarationSpecifier>],
) -> Option<String> {
    specifiers.iter().find_map(|spec| {
        if let DeclarationSpecifier::TypeSpecifier(ref ty_spec) = spec.node
            && let TypeSpecifier::TypedefName(ref ident) = ty_spec.node
        {
            return Some(ident.node.name.clone());
        }
        None
    })
}

fn declarator_pointer_depth(declarator: &Declarator) -> usize {
    let mut depth = declarator
        .derived
        .iter()
        .filter(|entry| matches!(entry.node, DerivedDeclarator::Pointer(_)))
        .count();
    if let DeclaratorKind::Declarator(ref inner) = declarator.kind.node {
        depth += declarator_pointer_depth(&inner.node);
    }
    depth
}

fn function_return_pointer_depth(declarator: &Declarator) -> usize {
    let mut depth = 0usize;
    for derived in &declarator.derived {
        match derived.node {
            DerivedDeclarator::Pointer(_) => depth += 1,
            DerivedDeclarator::Function(_) => break,
            DerivedDeclarator::Array(_)
            | DerivedDeclarator::KRFunction(_)
            | DerivedDeclarator::Block(_) => {}
        }
    }
    if let DeclaratorKind::Declarator(ref inner) = declarator.kind.node {
        depth += function_return_pointer_depth(&inner.node);
    }
    depth
}

fn collect_typedef_ref_spans_decl(
    decl: &Declaration,
    decl_start: usize,
) -> Vec<(usize, usize, String)> {
    let mut collector = TypedefRefCollector { refs: Vec::new() };
    collector.visit_declaration(decl, &Span::none());

    collector
        .refs
        .into_iter()
        .map(|(s, e, name)| {
            (
                s.saturating_sub(decl_start),
                e.saturating_sub(decl_start),
                name,
            )
        })
        .collect()
}

fn collect_tag_name_spans_decl(
    decl: &Declaration,
    decl_start: usize,
) -> Vec<(usize, usize, String)> {
    let mut collector = TagNameCollector { refs: Vec::new() };
    collector.visit_declaration(decl, &Span::none());
    collector
        .refs
        .into_iter()
        .map(|(s, e, name)| {
            (
                s.saturating_sub(decl_start),
                e.saturating_sub(decl_start),
                name,
            )
        })
        .collect()
}

fn collect_enum_constant_spans_decl(
    decl: &Declaration,
    decl_start: usize,
) -> Vec<(usize, usize, String)> {
    let mut collector = EnumConstantCollector { refs: Vec::new() };
    collector.visit_declaration(decl, &Span::none());
    collector
        .refs
        .into_iter()
        .map(|(s, e, name)| {
            (
                s.saturating_sub(decl_start),
                e.saturating_sub(decl_start),
                name,
            )
        })
        .collect()
}

#[derive(Default)]
struct TypedefRefCollector {
    refs: Vec<(usize, usize, String)>,
}

impl<'ast> Visit<'ast> for TypedefRefCollector {
    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        if let TypeSpecifier::TypedefName(ident) = type_specifier {
            self.refs
                .push((ident.span.start, ident.span.end, ident.node.name.clone()));
        }
        visit_type_specifier(self, type_specifier, span);
    }
}

#[derive(Default)]
struct TagNameCollector {
    refs: Vec<(usize, usize, String)>,
}

impl<'ast> Visit<'ast> for TagNameCollector {
    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        match type_specifier {
            TypeSpecifier::Struct(st) => {
                if let Some(ident) = &st.node.identifier {
                    self.refs
                        .push((ident.span.start, ident.span.end, ident.node.name.clone()));
                }
            }
            TypeSpecifier::Enum(en) => {
                if let Some(ident) = &en.node.identifier {
                    self.refs
                        .push((ident.span.start, ident.span.end, ident.node.name.clone()));
                }
            }
            _ => {}
        }
        visit_type_specifier(self, type_specifier, span);
    }
}

#[derive(Default)]
struct EnumConstantCollector {
    refs: Vec<(usize, usize, String)>,
}

impl<'ast> Visit<'ast> for EnumConstantCollector {
    fn visit_type_specifier(&mut self, type_specifier: &'ast TypeSpecifier, span: &'ast Span) {
        if let TypeSpecifier::Enum(en) = type_specifier {
            for enumerator in &en.node.enumerators {
                self.refs.push((
                    enumerator.node.identifier.span.start,
                    enumerator.node.identifier.span.end,
                    enumerator.node.identifier.node.name.clone(),
                ));
            }
        }
        visit_type_specifier(self, type_specifier, span);
    }
}

fn resolve_alias_chains(type_aliases: &[TypeAlias]) -> HashMap<String, String> {
    type_aliases
        .iter()
        .map(|alias| (alias.name.clone(), alias.name.clone()))
        .collect()
}

fn apply_replacements(source: &str, mut replacements: Vec<(usize, usize, String)>) -> String {
    if replacements.is_empty() {
        return source.to_owned();
    }

    replacements.sort_by_key(|entry| entry.0);
    let mut output = String::with_capacity(source.len());
    let mut cursor = 0usize;

    for (start, end, replacement) in replacements {
        if start < cursor || start > source.len() || end > source.len() || start > end {
            continue;
        }
        output.push_str(&source[cursor..start]);
        output.push_str(&replacement);
        cursor = end;
    }

    output.push_str(&source[cursor..]);
    output
}

fn namespaced(namespace: &str, name: &str) -> String {
    let prefix = format!("{}_", namespace);
    if name.starts_with(&prefix) {
        name.to_owned()
    } else {
        format!("{}{}", prefix, name)
    }
}

fn normalize_windows_verbatim_path(path: &Path) -> PathBuf {
    #[cfg(windows)]
    {
        let raw = path.to_string_lossy();
        if let Some(stripped) = raw.strip_prefix(r"\\?\") {
            return PathBuf::from(stripped);
        }
        path.to_path_buf()
    }

    #[cfg(not(windows))]
    {
        path.to_path_buf()
    }
}

fn build_forward_arg_expr(
    param: &ParamConversion,
    known_type_names: &HashSet<String>,
    namespace: &str,
) -> String {
    let Some(vendor_type) = &param.vendor_type else {
        return param.arg_name.clone();
    };
    if !known_type_names.contains(vendor_type) {
        return param.arg_name.clone();
    }
    if param.pointer_depth > 0 {
        let stars = "*".repeat(param.pointer_depth);
        return format!("({}{} {}){}", vendor_type, stars, "", param.arg_name).replace("  ", " ");
    }

    let ns_type = namespaced(namespace, vendor_type);
    format!(
        "((union {{ {} ns; {} vd; }}){{ .ns = {} }}).vd",
        ns_type, vendor_type, param.arg_name
    )
}

fn build_return_expr(
    call_expr: &str,
    ret: &ReturnConversion,
    known_type_names: &HashSet<String>,
    namespace: &str,
) -> String {
    let Some(vendor_type) = &ret.vendor_type else {
        return call_expr.to_owned();
    };
    if !known_type_names.contains(vendor_type) {
        return call_expr.to_owned();
    }

    let ns_type = namespaced(namespace, vendor_type);
    if ret.pointer_depth > 0 {
        let stars = "*".repeat(ret.pointer_depth);
        return format!("({}{})({})", ns_type, stars, call_expr);
    }

    format!(
        "((union {{ {} ns; {} vd; }}){{ .vd = {} }}).ns",
        ns_type, vendor_type, call_expr
    )
}
