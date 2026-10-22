// HIR docs generator module

#[cfg(not(feature = "docs"))]
pub fn generate_docs<T, P>(_hir: T, _out_dir: &P) -> Result<(), Box<dyn std::error::Error>>
where
    P: AsRef<std::path::Path>,
{
    // Stub when docs feature is disabled — no-op to keep compilation without optional deps
    Ok(())
}
#[cfg(feature = "docs")]
pub use inner::generate_docs;

#[cfg(feature = "docs")]
pub mod inner {
    use crate::atlas_c::atlas_hir::HirModule;
    use crate::atlas_c::atlas_hir::item::HirExtendBlock;
    use crate::atlas_c::atlas_hir::pretty_print::HirPrettyPrinter;
    use crate::atlas_c::atlas_hir::signature::{
        HirConceptSignature, HirStructMethodSignature, HirVisibility,
    };
    use pulldown_cmark::{Options, Parser as MdParser, html};
    use std::collections::BTreeMap;
    use std::error::Error;
    use std::path::Path;

    fn escape_html(value: &str) -> String {
        let mut out = String::with_capacity(value.len());
        for ch in value.chars() {
            match ch {
                '&' => out.push_str("&amp;"),
                '<' => out.push_str("&lt;"),
                '>' => out.push_str("&gt;"),
                '"' => out.push_str("&quot;"),
                '\'' => out.push_str("&#39;"),
                _ => out.push(ch),
            }
        }
        out
    }

    const STYLE: &str = include_str!("templates/style.css");
    const ROOT: &str = "{{ROOT}}";

    fn md_to_html(md: &str) -> String {
        let parser = MdParser::new_ext(md, Options::all());
        let mut html_out = String::new();
        html::push_html(&mut html_out, parser);
        html_out
    }

    fn docs_html(docs: Option<&str>) -> String {
        docs.map(md_to_html).unwrap_or_default()
    }

    fn summary(docs: Option<&str>) -> String {
        let Some(docs) = docs else {
            return String::new();
        };
        let line = docs.lines().find(|l| !l.trim().is_empty()).unwrap_or("");
        escape_html(line.trim())
    }

    fn split_qualified(qualified: &str) -> (Vec<String>, String) {
        let mut parts: Vec<String> = qualified.split("::").map(str::to_owned).collect();
        let name = parts.pop().unwrap_or_else(|| qualified.to_owned());
        (parts, name)
    }

    fn slug(value: &str) -> String {
        let mut out: String = value
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_' {
                    c
                } else {
                    '-'
                }
            })
            .collect();
        while out.contains("--") {
            out = out.replace("--", "-");
        }
        out.trim_matches('-').to_lowercase()
    }

    fn forward_slashes(path: &str) -> String {
        let path = path.replace('\\', "/");
        path.strip_prefix("//?/").unwrap_or(&path).to_string()
    }

    fn project_root() -> Option<String> {
        let cwd = std::env::current_dir().ok()?;
        let canonical = std::fs::canonicalize(cwd).ok()?;
        Some(forward_slashes(&canonical.to_string_lossy()))
    }

    fn display_path(raw: &str, root: Option<&str>) -> String {
        let normalized = forward_slashes(raw);
        if let Some(root) = root
            && let Some(rest) = normalized.strip_prefix(root)
        {
            return rest.trim_start_matches('/').to_string();
        }
        normalized
    }

    fn is_dependency_file(path: &str) -> bool {
        let normalized = path.replace('\\', "/");
        let Some(first) = normalized.split('/').next() else {
            return false;
        };
        if first.is_empty() || first == "." || first == ".." || first.contains(':') {
            return false;
        }
        Path::new("build/libs").join(first).is_dir()
    }

    fn is_synthetic_file(path: &str) -> bool {
        path.is_empty() || path == "<stdin>"
    }

    fn base_type_name(displayed: &str) -> String {
        displayed
            .split('<')
            .next()
            .unwrap_or(displayed)
            .trim()
            .trim_start_matches('*')
            .trim()
            .to_owned()
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Kind {
        Struct,
        Concept,
        Enum,
        Union,
        Function,
        Constant,
    }

    impl Kind {
        fn prefix(self) -> &'static str {
            match self {
                Kind::Struct => "struct",
                Kind::Concept => "concept",
                Kind::Enum => "enum",
                Kind::Union => "union",
                Kind::Function => "fn",
                Kind::Constant => "const",
            }
        }

        fn label(self) -> &'static str {
            match self {
                Kind::Struct => "Struct",
                Kind::Concept => "Concept",
                Kind::Enum => "Enum",
                Kind::Union => "Union",
                Kind::Function => "Function",
                Kind::Constant => "Constant",
            }
        }

        fn section(self) -> &'static str {
            match self {
                Kind::Struct => "Structs",
                Kind::Concept => "Concepts",
                Kind::Enum => "Enums",
                Kind::Union => "Unions",
                Kind::Function => "Functions",
                Kind::Constant => "Constants",
            }
        }

        fn all() -> [Kind; 6] {
            [
                Kind::Struct,
                Kind::Concept,
                Kind::Enum,
                Kind::Union,
                Kind::Function,
                Kind::Constant,
            ]
        }
    }

    struct Entry {
        kind: Kind,
        name: String,
        qualified: String,
        namespace: Vec<String>,
        file: String,
        docs: Option<String>,
        /// Declaration rendered as code, shown at the top of the item's page.
        signature: String,
        /// Everything below the signature: fields, variants, methods, conformances...
        detail: String,
    }

    impl Entry {
        fn file_name(&self) -> String {
            format!("{}.{}.html", self.kind.prefix(), slug(&self.name))
        }

        fn href(&self) -> String {
            let mut segments: Vec<String> = self.namespace.iter().map(|s| slug(s)).collect();
            segments.push(self.file_name());
            segments.join("/")
        }
    }

    fn code_block(code: &str) -> String {
        format!("<pre class=\"code\">{}</pre>\n", escape_html(code))
    }

    fn section(title: &str, body: &str) -> String {
        let trimmed = body.trim();
        if trimmed.is_empty() || !trimmed.contains("<li") && trimmed.starts_with("<ul") {
            return String::new();
        }
        format!("<section>\n<h2>{}</h2>\n{}</section>\n", title, body)
    }

    fn method_signature(name: &str, sig: &HirStructMethodSignature<'_>) -> String {
        let mut printer = HirPrettyPrinter::new();
        printer.print_method_signature(name, sig);
        printer.get_output()
    }

    fn member(anchor: &str, signature: &str, own: Option<&str>, inherited: Option<&str>) -> String {
        let mut out = String::new();
        out.push_str(&format!(
            "<div class=\"member\" id=\"{}\">\n",
            escape_html(anchor)
        ));
        out.push_str(&code_block(signature));
        match (own, inherited) {
            (Some(docs), _) => out.push_str(&md_to_html(docs)),
            (None, Some(docs)) => {
                out.push_str(
                    "<p class=\"inherited\">Documentation inherited from the concept:</p>\n",
                );
                out.push_str(&md_to_html(docs));
            }
            (None, None) => {}
        }
        out.push_str("</div>\n");
        out
    }

    pub fn generate_docs(
        module: &HirModule,
        out_dir: &Path,
        package: Option<&str>,
    ) -> Result<(), Box<dyn Error>> {
        std::fs::create_dir_all(out_dir)?;
        let signature = &module.signature;
        let root = project_root();
        let root = root.as_deref();
        let file_docs: BTreeMap<String, &str> = signature
            .file_docs
            .iter()
            .map(|(path, docs)| (display_path(path, root), *docs))
            .collect();

        let mut extends_by_type: BTreeMap<String, Vec<&HirExtendBlock<'_>>> = BTreeMap::new();
        for blocks in module.body.extends.values() {
            for block in blocks {
                extends_by_type
                    .entry(base_type_name(&format!("{}", block.ty)))
                    .or_default()
                    .push(block);
            }
        }

        let mut entries: Vec<Entry> = Vec::new();

        for (qualified, strukt) in signature.structs.iter() {
            if strukt.vis == HirVisibility::Private
                || strukt.pre_mangled_ty.is_some()
                || is_dependency_file(strukt.declaration_span.path)
                || is_synthetic_file(strukt.declaration_span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            let generics = render_generics(&strukt.generics);
            let mut detail = String::new();

            let fields = strukt
                .fields
                .values()
                .filter(|f| f.vis != HirVisibility::Private)
                .map(|f| {
                    member(
                        &format!("field.{}", slug(f.name)),
                        &format!("{}: {}", f.name, f.ty),
                        f.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Fields", &fields));

            let constants = strukt
                .constants
                .iter()
                .map(|(cname, c)| {
                    member(
                        &format!("const.{}", slug(cname)),
                        &format!("const {}: {}", cname, c.ty),
                        c.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Constants", &constants));

            let methods = strukt
                .methods
                .iter()
                .filter(|(_, m)| m.vis != HirVisibility::Private)
                .map(|(mname, m)| {
                    member(
                        &format!("method.{}", slug(mname)),
                        &method_signature(mname, m),
                        m.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Methods", &methods));

            if let Some(destructor) = &strukt.destructor
                && strukt.had_user_defined_destructor
            {
                detail.push_str(&section(
                    "Destructor",
                    &member(
                        "destructor",
                        &format!("~{}()", name),
                        destructor.docstring,
                        None,
                    ),
                ));
            }

            detail.push_str(&render_conformances(
                qualified,
                &extends_by_type,
                &signature.concepts,
            ));

            entries.push(Entry {
                kind: Kind::Struct,
                signature: format!("struct {}{}", qualified, generics),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(strukt.declaration_span.path, root),
                docs: strukt.docstring.map(str::to_owned),
                detail,
            });
        }

        for (qualified, concept) in signature.concepts.iter() {
            if concept.vis == HirVisibility::Private
                || is_synthetic_file(concept.declaration_span.path)
                || is_dependency_file(concept.declaration_span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            let generics = render_generics(&concept.generics);
            let mut detail = String::new();

            let associated = concept
                .associated_types
                .values()
                .map(|assoc| {
                    let rendered = match assoc.ty {
                        Some(ty) => format!("type {} = {}", assoc.name, ty),
                        None => format!("type {}", assoc.name),
                    };
                    member(
                        &format!("type.{}", slug(assoc.name)),
                        &rendered,
                        assoc.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Associated types", &associated));

            let required = concept
                .required_method_names
                .iter()
                .zip(concept.required_methods.iter())
                .map(|(mname, m)| {
                    member(
                        &format!("method.{}", slug(mname)),
                        &method_signature(mname, m),
                        m.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Required methods", &required));

            let operators = concept
                .required_operators
                .iter()
                .map(|(kind, m)| {
                    let label = format!("operator {:?}", kind).to_lowercase();
                    member(
                        &slug(&label),
                        &method_signature(&label, m),
                        m.docstring,
                        None,
                    )
                })
                .collect::<String>();
            detail.push_str(&section("Required operators", &operators));

            detail.push_str(&render_implementors(qualified, &extends_by_type));

            entries.push(Entry {
                kind: Kind::Concept,
                signature: format!("concept {}{}", qualified, generics),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(concept.declaration_span.path, root),
                docs: concept.docstring.map(str::to_owned),
                detail,
            });
        }

        for (qualified, hir_enum) in signature.enums.iter() {
            if hir_enum.vis == HirVisibility::Private
                || is_synthetic_file(hir_enum.span.path)
                || is_dependency_file(hir_enum.span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            let variants = hir_enum
                .variants
                .iter()
                .map(|v| {
                    member(
                        &format!("variant.{}", slug(v.name)),
                        &format!("{} = {}", v.name, v.value),
                        v.docstring,
                        None,
                    )
                })
                .collect::<String>();

            entries.push(Entry {
                kind: Kind::Enum,
                signature: format!("enum {}", qualified),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(hir_enum.span.path, root),
                docs: hir_enum.docstring.map(str::to_owned),
                detail: section("Variants", &variants),
            });
        }

        for (qualified, union) in signature.unions.iter() {
            if union.vis == HirVisibility::Private
                || is_synthetic_file(union.declaration_span.path)
                || is_dependency_file(union.declaration_span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            entries.push(Entry {
                kind: Kind::Union,
                signature: format!("union {}", qualified),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(union.declaration_span.path, root),
                docs: union.docstring.map(str::to_owned),
                detail: String::new(),
            });
        }

        for (qualified, function) in signature.functions.iter() {
            if function.vis == HirVisibility::Private
                || function.pre_mangled_ty.is_some()
                || function.is_intrinsic
                || is_synthetic_file(function.span.path)
                || is_dependency_file(function.span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            let params = function
                .params
                .iter()
                .map(|p| format!("{}: {}", p.name, p.ty))
                .collect::<Vec<_>>()
                .join(", ");
            entries.push(Entry {
                kind: Kind::Function,
                signature: format!(
                    "fun {}{}({}) -> {}",
                    qualified,
                    render_generics(&function.generics),
                    params,
                    function.return_ty
                ),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(function.span.path, root),
                docs: function.docstring.map(str::to_owned),
                detail: String::new(),
            });
        }

        for (qualified, constant) in signature.global_consts.iter() {
            if constant.vis == HirVisibility::Private
                || is_synthetic_file(constant.span.path)
                || is_dependency_file(constant.span.path)
            {
                continue;
            }
            let (namespace, name) = split_qualified(qualified);
            entries.push(Entry {
                kind: Kind::Constant,
                signature: format!("const {}: {}", qualified, constant.ty),
                name,
                qualified: qualified.to_string(),
                namespace,
                file: display_path(constant.span.path, root),
                docs: constant.docstring.map(str::to_owned),
                detail: String::new(),
            });
        }

        entries.sort_by(|a, b| a.qualified.cmp(&b.qualified));

        let mut namespaces: BTreeMap<Vec<String>, Option<String>> = BTreeMap::new();
        for entry in &entries {
            for depth in 0..=entry.namespace.len() {
                namespaces
                    .entry(entry.namespace[..depth].to_vec())
                    .or_insert(None);
            }
        }
        for (qualified, namespace) in signature.namespaces.iter() {
            let path: Vec<String> = qualified.split("::").map(str::to_owned).collect();
            if let Some(slot) = namespaces.get_mut(&path) {
                *slot = namespace.docstring.map(str::to_owned);
            }
        }

        let nav = render_nav(&namespaces, &entries);

        for entry in &entries {
            let depth = entry.namespace.len();
            let mut body = String::new();
            body.push_str(&format!(
                "<h1><span class=\"kind\">{}</span> {}</h1>\n",
                entry.kind.label(),
                escape_html(&entry.qualified)
            ));
            body.push_str(&code_block(&entry.signature));
            body.push_str(&format!(
                "<p class=\"source\">Declared in <a href=\"{ROOT}files/{}\">{}</a></p>\n",
                file_page_name(&entry.file),
                escape_html(&entry.file)
            ));
            body.push_str(&docs_html(entry.docs.as_deref()));
            body.push_str(&entry.detail);
            write_page(out_dir, &entry.href(), &entry.qualified, &nav, &body, depth)?;
        }

        for (path, docs) in &namespaces {
            let title = if path.is_empty() {
                package_title(package.unwrap_or(signature.module_name))
            } else {
                path.join("::")
            };
            let mut body = String::new();
            body.push_str(&format!("<h1>{}</h1>\n", escape_html(&title)));
            if path.is_empty() {
                body.push_str("<p class=\"lead\">API documentation.</p>\n");
            }
            body.push_str(&docs_html(docs.as_deref()));

            let children: Vec<&Vec<String>> = namespaces
                .keys()
                .filter(|candidate| {
                    candidate.len() == path.len() + 1 && candidate.starts_with(path.as_slice())
                })
                .collect();
            if !children.is_empty() {
                let list = children
                    .iter()
                    .map(|child| {
                        let href = format!(
                            "{}/index.html",
                            child.iter().map(|s| slug(s)).collect::<Vec<_>>().join("/")
                        );
                        format!(
                            "<li><a href=\"{ROOT}{}\">{}</a></li>\n",
                            href,
                            escape_html(child.join("::").as_str())
                        )
                    })
                    .collect::<String>();
                body.push_str(&section(
                    "Namespaces",
                    &format!("<ul class=\"listing\">{list}</ul>"),
                ));
            }

            for kind in Kind::all() {
                let list = entries
                    .iter()
                    .filter(|entry| entry.kind == kind && &entry.namespace == path)
                    .map(|entry| {
                        format!(
                            "<li><a href=\"{ROOT}{}\"><code>{}</code></a> <span class=\"blurb\">{}</span></li>\n",
                            entry.href(),
                            escape_html(&entry.name),
                            summary(entry.docs.as_deref())
                        )
                    })
                    .collect::<String>();
                body.push_str(&section(
                    kind.section(),
                    &format!("<ul class=\"listing\">{list}</ul>"),
                ));
            }

            if path.is_empty() {
                let files = file_docs
                    .keys()
                    .cloned()
                    .chain(entries.iter().map(|entry| entry.file.clone()))
                    .filter(|path| !is_dependency_file(path) && !is_synthetic_file(path))
                    .collect::<std::collections::BTreeSet<_>>();
                let list = files
                    .iter()
                    .map(|file| {
                        format!(
                            "<li><a href=\"{ROOT}files/{}\"><code>{}</code></a> <span class=\"blurb\">{}</span></li>\n",
                            file_page_name(file),
                            escape_html(file),
                            summary(file_docs.get(file.as_str()).copied())
                        )
                    })
                    .collect::<String>();
                body.push_str(&section(
                    "Source files",
                    &format!("<ul class=\"listing\">{list}</ul>"),
                ));
            }

            let href = if path.is_empty() {
                "index.html".to_string()
            } else {
                format!(
                    "{}/index.html",
                    path.iter().map(|s| slug(s)).collect::<Vec<_>>().join("/")
                )
            };
            write_page(out_dir, &href, &title, &nav, &body, path.len())?;
        }

        let files = file_docs
            .keys()
            .cloned()
            .chain(entries.iter().map(|entry| entry.file.clone()))
            .filter(|path| !is_dependency_file(path) && !is_synthetic_file(path))
            .collect::<std::collections::BTreeSet<_>>();
        for file in &files {
            let mut body = String::new();
            body.push_str(&format!("<h1>{}</h1>\n", escape_html(file)));
            body.push_str(&docs_html(file_docs.get(file.as_str()).copied()));
            for kind in Kind::all() {
                let list = entries
                    .iter()
                    .filter(|entry| entry.kind == kind && &entry.file == file)
                    .map(|entry| {
                        format!(
                            "<li><a href=\"{ROOT}{}\"><code>{}</code></a> <span class=\"blurb\">{}</span></li>\n",
                            entry.href(),
                            escape_html(&entry.qualified),
                            summary(entry.docs.as_deref())
                        )
                    })
                    .collect::<String>();
                body.push_str(&section(
                    kind.section(),
                    &format!("<ul class=\"listing\">{list}</ul>"),
                ));
            }
            write_page(
                out_dir,
                &format!("files/{}", file_page_name(file)),
                file,
                &nav,
                &body,
                1,
            )?;
        }

        std::fs::write(out_dir.join("style.css"), STYLE)?;
        println!(
            "[atlas_docs] wrote {} item pages, {} namespace pages and {} file pages to {}",
            entries.len(),
            namespaces.len(),
            files.len(),
            out_dir.display()
        );
        Ok(())
    }

    fn package_title(module_name: &str) -> String {
        if module_name.is_empty() {
            "Documentation".to_string()
        } else {
            module_name.to_string()
        }
    }

    fn file_page_name(path: &str) -> String {
        format!("{}.html", slug(&path.replace('\\', "/")))
    }

    fn render_generics(
        generics: &[&crate::atlas_c::atlas_hir::signature::HirGenericConstraint<'_>],
    ) -> String {
        if generics.is_empty() {
            return String::new();
        }
        let names = generics
            .iter()
            .map(|g| g.generic_name.to_string())
            .collect::<Vec<_>>();
        format!("<{}>", names.join(", "))
    }

    fn render_conformances(
        type_name: &str,
        extends_by_type: &BTreeMap<String, Vec<&HirExtendBlock<'_>>>,
        concepts: &BTreeMap<&str, &HirConceptSignature<'_>>,
    ) -> String {
        let Some(blocks) = extends_by_type.get(type_name) else {
            return String::new();
        };

        let mut out = String::new();
        for block in blocks {
            let concept_name = base_type_name(&format!("{}", block.concept));
            let concept = concepts.get(concept_name.as_str()).copied();
            out.push_str(&format!(
                "<div class=\"member\" id=\"conformance.{}\">\n",
                slug(&concept_name)
            ));
            out.push_str(&code_block(&format!(
                "extend {} with {}",
                block.ty, block.concept
            )));
            match (block.docstring, concept.and_then(|c| c.docstring)) {
                (Some(docs), _) => out.push_str(&md_to_html(docs)),
                (None, Some(docs)) => {
                    out.push_str(
                        "<p class=\"inherited\">Documentation inherited from the concept:</p>\n",
                    );
                    out.push_str(&md_to_html(docs));
                }
                (None, None) => {}
            }

            for associated in &block.associated_types {
                let rendered = match associated.ty {
                    Some(ty) => format!("type {} = {}", associated.name, ty),
                    None => format!("type {}", associated.name),
                };
                let inherited = concept
                    .and_then(|c| c.associated_types.get(associated.name))
                    .and_then(|assoc| assoc.docstring);
                out.push_str(&member(
                    &format!(
                        "conformance.{}.type.{}",
                        slug(&concept_name),
                        slug(associated.name)
                    ),
                    &rendered,
                    associated.docstring,
                    inherited,
                ));
            }

            for method in &block.methods {
                let inherited = concept.and_then(|c| {
                    c.required_method_names
                        .iter()
                        .position(|name| *name == method.name)
                        .and_then(|index| c.required_methods.get(index))
                        .and_then(|required| required.docstring)
                });
                out.push_str(&member(
                    &format!(
                        "conformance.{}.method.{}",
                        slug(&concept_name),
                        slug(method.name)
                    ),
                    &method_signature(method.name, method.signature),
                    method.signature.docstring,
                    inherited,
                ));
            }

            out.push_str("</div>\n");
        }
        section("Conformances", &out)
    }

    fn render_implementors(
        concept_name: &str,
        extends_by_type: &BTreeMap<String, Vec<&HirExtendBlock<'_>>>,
    ) -> String {
        let mut out = String::new();
        for blocks in extends_by_type.values() {
            for block in blocks {
                if base_type_name(&format!("{}", block.concept)) != concept_name {
                    continue;
                }
                out.push_str("<div class=\"member\">\n");
                out.push_str(&code_block(&format!(
                    "extend {} with {}",
                    block.ty, block.concept
                )));
                if let Some(docs) = block.docstring {
                    out.push_str(&md_to_html(docs));
                }
                out.push_str("</div>\n");
            }
        }
        section("Implementors", &out)
    }

    fn render_nav(namespaces: &BTreeMap<Vec<String>, Option<String>>, entries: &[Entry]) -> String {
        let mut nav = String::new();
        nav.push_str(&format!(
            "<a class=\"home\" href=\"{ROOT}index.html\">Index</a>\n"
        ));
        for path in namespaces.keys() {
            if path.is_empty() {
                continue;
            }
            let href = format!(
                "{}/index.html",
                path.iter().map(|s| slug(s)).collect::<Vec<_>>().join("/")
            );
            nav.push_str(&format!(
                "<div class=\"ns\" style=\"padding-left:{}px\"><a href=\"{ROOT}{}\">{}</a></div>\n",
                (path.len() - 1) * 12,
                href,
                escape_html(path.last().map(String::as_str).unwrap_or_default())
            ));
            let mut listed = entries
                .iter()
                .filter(|entry| &entry.namespace == path)
                .collect::<Vec<_>>();
            listed.sort_by(|a, b| a.name.cmp(&b.name));
            for entry in listed {
                nav.push_str(&format!(
                    "<div class=\"item\" style=\"padding-left:{}px\"><a href=\"{ROOT}{}\">{}</a></div>\n",
                    path.len() * 12 + 4,
                    entry.href(),
                    escape_html(&entry.name)
                ));
            }
        }
        for entry in entries.iter().filter(|entry| entry.namespace.is_empty()) {
            nav.push_str(&format!(
                "<div class=\"item\"><a href=\"{ROOT}{}\">{}</a></div>\n",
                entry.href(),
                escape_html(&entry.name)
            ));
        }
        nav
    }

    fn write_page(
        out_dir: &Path,
        href: &str,
        title: &str,
        nav: &str,
        body: &str,
        depth: usize,
    ) -> Result<(), Box<dyn Error>> {
        let root_rel = if depth == 0 {
            String::new()
        } else {
            "../".repeat(depth)
        };
        let page = format!(
            "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n\
             <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
             <title>{title}</title>\n\
             <link rel=\"stylesheet\" href=\"{root}style.css\">\n\
             </head>\n<body>\n\
             <nav id=\"sidebar\">\n{nav}</nav>\n\
             <main id=\"content\">\n{body}</main>\n\
             <footer>Generated by <code>atlas77 docs</code></footer>\n\
             </body>\n</html>\n",
            title = escape_html(title),
            root = root_rel,
            nav = nav.replace(ROOT, &root_rel),
            body = body.replace(ROOT, &root_rel),
        );

        let target = out_dir.join(href);
        if let Some(parent) = target.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(target, page)?;
        Ok(())
    }
}
