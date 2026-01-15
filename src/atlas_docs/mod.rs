use crate::atlas_c::atlas_frontend::parser::ast::AstProgram;

//#[cfg(feature = "docs")]
mod inner {
    use super::AstProgram;
    use pulldown_cmark::html;
    use pulldown_cmark::{CodeBlockKind, Event, Options, Parser as MdParser, Tag};
    use std::path::{Path, PathBuf};
    use tera::{Context, Tera};

    use syntect::{highlighting::ThemeSet, html::highlighted_html_for_string, parsing::SyntaxSet};

    fn md_to_html_with_highlight(md: &str, ss: &SyntaxSet, ts: &ThemeSet) -> String {
        // We'll convert markdown to events and replace fenced code blocks with highlighted HTML
        let mut out = String::new();
        let mut events = Vec::new();
        let parser = MdParser::new_ext(md, Options::all());
        let mut in_code = false;
        let mut code_lang = None::<String>;
        let mut code_acc = String::new();

        for ev in parser {
            match ev {
                Event::Start(Tag::CodeBlock(kind)) => {
                    in_code = true;
                    code_acc.clear();
                    match kind {
                        CodeBlockKind::Fenced(lang) => code_lang = Some(lang.to_string()),
                        CodeBlockKind::Indented => code_lang = None,
                    }
                }
                Event::End(_) => {
                    if in_code {
                        // highlight
                        let lang = code_lang.clone().unwrap_or_default();
                        let highlighted = if !lang.is_empty() {
                            // try to find syntax reference
                            let syntax = ss
                                .find_syntax_by_token(&lang)
                                .unwrap_or_else(|| ss.find_syntax_plain_text());
                            let theme = ts
                                .themes
                                .get("InspiredGitHub")
                                .unwrap_or_else(|| ts.themes.values().next().unwrap());
                            match highlighted_html_for_string(&code_acc, &ss, syntax, theme) {
                                Ok(h) => h,
                                Err(_) => {
                                    format!("<pre><code>{}</code></pre>", escape_html(&code_acc))
                                }
                            }
                        } else {
                            format!("<pre><code>{}</code></pre>", escape_html(&code_acc))
                        };
                        events.push(Event::Html(highlighted.into()));
                        in_code = false;
                        code_lang = None;
                    } else {
                        events.push(Event::End(Tag::CodeBlock(CodeBlockKind::Indented).into()));
                    }
                }
                Event::Text(t) => {
                    if in_code {
                        code_acc.push_str(&t);
                    } else {
                        events.push(Event::Text(t));
                    }
                }
                other => events.push(other),
            }
        }

        html::push_html(&mut out, events.into_iter());
        out
    }

    fn escape_html(s: &str) -> String {
        s.replace('&', "&amp;")
            .replace('<', "&lt;")
            .replace('>', "&gt;")
            .replace('"', "&quot;")
    }

    fn load_templates() -> Result<Tera, tera::Error> {
        // Prefer templates on-disk so they are easier to edit
        let glob = "src/atlas_docs/templates/**/*";
        match Tera::new(glob) {
            Ok(t) => Ok(t),
            Err(e) => {
                // Fallback to empty tera and return error
                Err(e)
            }
        }
    }

    pub fn generate_docs_for(
        ast: AstProgram,
        out_dir: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        std::fs::create_dir_all(out_dir)?;
        println!("[atlas_docs] output dir: {}", out_dir.display());
        println!("[atlas_docs] AST items: {}", ast.items.len());

        // Load syntect resources
        let ss: SyntaxSet = SyntaxSet::load_defaults_newlines();
        let ts: ThemeSet = ThemeSet::load_defaults();

        // Load templates from disk (created under src/atlas_docs/templates)
        let mut tera = match load_templates() {
            Ok(t) => t,
            Err(_) => {
                // If templates missing, create a minimal in-memory Tera with single templates
                let mut t = Tera::default();
                t.add_raw_template("index.html", "<html><body>{% for item in items %}<a href='{{ item.file }}'>{{ item.title }}</a><br>{% endfor %}</body></html>")?;
                t.add_raw_template("item.html", "<html><body><a href='index.html'>Index</a><h1>{{ title }}</h1><div>{{ doc | safe }}</div></body></html>")?;
                t
            }
        };

        // Copy static assets (style.css) from templates dir to out_dir if present
        let tpl_css = PathBuf::from("src/atlas_docs/templates/style.css");
        if tpl_css.exists() {
            let css_dst = out_dir.join("style.css");
            std::fs::copy(tpl_css, &css_dst)?;
            println!("[atlas_docs] copied style.css -> {}", css_dst.display());
        }

        let mut items_meta: Vec<std::collections::HashMap<String, tera::Value>> = Vec::new();

        // helper to render templates and write to disk while reporting detailed errors
        fn render_and_write(
            tera: &Tera,
            template: &str,
            ctx: &Context,
            dst: &std::path::Path,
        ) -> Result<(), Box<dyn std::error::Error>> {
            match tera.render(template, ctx) {
                Ok(rendered) => {
                    std::fs::write(dst, rendered)?;
                    Ok(())
                }
                Err(e) => {
                    eprintln!(
                        "[atlas_docs] tera render error for template '{}' -> {}",
                        template, e
                    );
                    eprintln!("[atlas_docs] tera error details: {:#?}", e);
                    Err(Box::new(e))
                }
            }
        }

        // Build a markdown representation and a small TOC of (id, title)
        fn slugify(kind: &str, name: &str) -> String {
            let mut s = format!("{}-{}", kind.to_lowercase(), name.to_lowercase());
            s = s
                .chars()
                .map(|c| if c.is_ascii_alphanumeric() { c } else { '-' })
                .collect();
            while s.contains("--") {
                s = s.replace("--", "-");
            }
            s.trim_matches('-').to_string()
        }

        fn build_markdown_and_toc(ast: AstProgram) -> (String, Vec<(String, String)>) {
            let mut md = String::new();
            let mut toc: Vec<(String, String)> = Vec::new();
            md.push_str(&format!("# Documentation\n\n"));

            // Imports
            let imports: Vec<String> = ast
                .items
                .iter()
                .filter_map(|it| match it {
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Import(imp) => {
                        Some(imp.path.to_string())
                    }
                    _ => None,
                })
                .collect();
            if !imports.is_empty() {
                md.push_str("## Imports\n\n");
                for imp in &imports {
                    md.push_str(&format!("- {}\n", imp));
                }
                md.push_str("\n");
            }

            // Items
            for item in ast.items.iter() {
                match item {
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Function(f) => {
                        let kind = "Function";
                        let name = f.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        // signature
                        let args = f
                            .args
                            .iter()
                            .map(|a| format!("{}: {}", a.name.name, a.ty.name()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        md.push_str(&format!(
                            "```
fun {}({}) -> {} 
```\n\n",
                            f.name.name,
                            args,
                            f.ret.name()
                        ));
                        if let Some(d) = f.docstring {
                            md.push_str(d);
                            md.push_str("\n\n");
                        }
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Struct(s) => {
                        let kind = "Struct";
                        let name = s.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        if let Some(d) = s.docstring {
                            md.push_str(d);
                            md.push_str("\n\n");
                        }
                        // flag
                        match s.flag {
                            crate::atlas_c::atlas_frontend::parser::ast::AstFlag::Copyable(_) => {
                                md.push_str("**Flag:** std::copyable\n\n");
                            }
                            crate::atlas_c::atlas_frontend::parser::ast::AstFlag::NonCopyable(
                                _,
                            ) => {
                                md.push_str("**Flag:** std::non_copyable\n\n");
                            }
                            _ => {}
                        }
                        // fields
                        if !s.fields.is_empty() {
                            md.push_str("**Fields**\n\n");
                            for f in s.fields.iter() {
                                md.push_str(&format!("- {}: {}\n", f.name.name, f.ty.name()));
                            }
                            md.push_str("\n");
                        }
                        // constructor
                        if let Some(c) = s.constructor {
                            md.push_str("**Constructor**\n\n");
                            if let Some(d) = c.docstring {
                                md.push_str(d);
                                md.push_str("\n\n");
                            }
                        }
                        if let Some(cc) = s.copy_constructor {
                            md.push_str("**Copy Constructor**\n\n");
                            if let Some(d) = cc.docstring {
                                md.push_str(d);
                                md.push_str("\n\n");
                            }
                        }
                        if let Some(dtor) = s.destructor {
                            md.push_str("**Destructor**\n\n");
                            if let Some(d) = dtor.docstring {
                                md.push_str(d);
                                md.push_str("\n\n");
                            }
                        }
                        if !s.methods.is_empty() {
                            md.push_str("**Methods**\n\n");
                            for m in s.methods.iter() {
                                let modifier = match m.modifier {
                                    crate::atlas_c::atlas_frontend::parser::ast::AstMethodModifier::Static => "Static",
                                    crate::atlas_c::atlas_frontend::parser::ast::AstMethodModifier::Const => "Const",
                                    crate::atlas_c::atlas_frontend::parser::ast::AstMethodModifier::Mutable => "Mutable",
                                    crate::atlas_c::atlas_frontend::parser::ast::AstMethodModifier::Consuming => "Consuming",
                                };
                                md.push_str(&format!("- **{}** {}\n\n", modifier, m.name.name));
                                if let Some(d) = m.docstring {
                                    md.push_str(d);
                                    md.push_str("\n\n");
                                }
                            }
                        }
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Enum(en) => {
                        let kind = "Enum";
                        let name = en.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        if let Some(d) = en.docstring {
                            md.push_str(d);
                            md.push_str("\n\n");
                        }
                        if !en.variants.is_empty() {
                            md.push_str("**Variants**\n\n");
                            for v in en.variants.iter() {
                                md.push_str(&format!("- {} = {}\n", v.name.name, v.value));
                            }
                            md.push_str("\n");
                        }
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::ExternFunction(e) => {
                        let kind = "ExternFunction";
                        let name = e.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        let args = e
                            .args_name
                            .iter()
                            .zip(e.args_ty.iter())
                            .map(|(n, t)| format!("{}: {}", n.name, t.name()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        md.push_str(&format!(
                            "```
extern fn {}({}) -> {} 
```\n\n",
                            e.name.name,
                            args,
                            e.ret_ty.name()
                        ));
                        if let Some(d) = e.docstring {
                            md.push_str(d);
                            md.push_str("\n\n");
                        }
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Import(_) => {
                        // imports already handled
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Union(u) => {
                        let kind = "Union";
                        let name = u.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        if let Some(d) = u.docstring {
                            md.push_str(d);
                            md.push_str("\n\n");
                        }
                    }
                    crate::atlas_c::atlas_frontend::parser::ast::AstItem::Constant(c) => {
                        let kind = "Constant";
                        let name = c.name.name;
                        let id = slugify(kind, name);
                        toc.push((id.clone(), format!("{}: {}", kind, name)));
                        md.push_str(&format!("<a id=\"{}\"></a>\n", id));
                        md.push_str(&format!("## {}: {}\n\n", kind, name));
                        if !c.value.span().path.is_empty() {
                            // no-op; keep signature light
                        }
                    }
                }
            }

            (md, toc)
        }

        let (markdown, toc) = build_markdown_and_toc(ast);
        // determine output names based on first item's span path
        let file_path = if !ast.items.is_empty() {
            ast.items[0].span().path
        } else {
            "<unknown>"
        };
        let base_name = std::path::Path::new(file_path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("documentation");
        // write markdown file
        let md_file = out_dir.join(format!("{}.md", base_name));
        std::fs::write(&md_file, &markdown)?;
        println!("[atlas_docs] wrote {}", md_file.display());

        // Convert markdown to HTML with syntax highlighting
        let content_html = md_to_html_with_highlight(&markdown, &ss, &ts);

        // Build HTML with sidebar TOC
        let mut html = String::new();
        html.push_str("<!doctype html><html><head><meta charset=\"utf-8\">\n");
        html.push_str(&format!("<title>{}</title>\n", base_name));
        if Path::new("src/atlas_docs/templates/style.css").exists() {
            html.push_str("<link rel=\"stylesheet\" href=\"style.css\">\n");
        } else {
            // minimal layout styles
            html.push_str("<style>body{font-family:Arial,Helvetica,sans-serif;margin:0}#wrap{display:flex}#toc{width:260px;padding:16px;border-right:1px solid #eee;overflow:auto;height:100vh}#main{flex:1;padding:24px;overflow:auto}pre{background:#f6f8fa;padding:12px;border-radius:6px}</style>\n");
        }
        html.push_str("</head><body>\n");
        html.push_str(&format!(
            "<h1 style=\"margin:16px\">Documentation for {}</h1>\n",
            file_path
        ));
        html.push_str("<div id=\"wrap\">\n");
        // toc
        html.push_str("<nav id=\"toc\">\n<h3>Contents</h3>\n<ul>\n");
        for (id, title) in &toc {
            html.push_str(&format!(
                "<li><a href=\"#{}\">{}</a></li>\n",
                escape_html(id),
                escape_html(title)
            ));
        }
        html.push_str("</ul>\n</nav>\n");
        // main content
        html.push_str("<main id=\"main\">\n");
        html.push_str(&content_html);
        html.push_str("</main>\n");
        html.push_str("</div>\n");
        html.push_str("</body></html>");
        let out_file = out_dir.join(format!("{}.html", base_name));
        std::fs::write(&out_file, html)?;
        println!("[atlas_docs] wrote {}", out_file.display());
        Ok(())
    }
}

#[cfg(feature = "docs")]
pub use inner::generate_docs_for as generate_docs;

#[cfg(not(feature = "docs"))]
pub fn generate_docs<T, P>(_ast: T, _out_dir: &P) -> Result<(), Box<dyn std::error::Error>>
where
    P: AsRef<std::path::Path>,
{
    // Stub when docs feature is disabled — no-op to keep compilation without optional deps
    Ok(())
}
