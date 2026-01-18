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
    use crate::atlas_c::atlas_hir::pretty_print::HirPrettyPrinter;
    use crate::atlas_c::atlas_hir::signature::{HirModuleSignature, HirVisibility};
    use pulldown_cmark::{Options, Parser as MdParser, html};
    use std::error::Error;
    use std::path::Path;
    use tera::escape_html;

    fn md_to_html(md: &str) -> String {
        let parser = MdParser::new_ext(md, Options::all());
        let mut html_out = String::new();
        html::push_html(&mut html_out, parser);
        html_out
    }

    pub fn generate_docs(hir: &HirModuleSignature, out_dir: &Path) -> Result<(), Box<dyn Error>> {
        std::fs::create_dir_all(out_dir)?;

        use std::collections::HashMap;

        let mut files_md: HashMap<String, String> = HashMap::new();
        let mut files_toc: HashMap<String, Vec<(String, String)>> = HashMap::new();
        let mut all_paths: Vec<String> = Vec::new();

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

        // Functions
        for (name, f) in hir.functions.iter() {
            // Skip private functions
            if f.vis == HirVisibility::Private {
                continue;
            }
            let path = f.span.path;
            all_paths.push(path.to_string());
            let entry = files_md
                .entry(path.to_string())
                .or_insert_with(|| String::from("# Documentation\n\n"));
            let toc: &mut Vec<(String, String)> = files_toc.entry(path.to_string()).or_default();
            let kind = "Function";
            let id = slugify(kind, name);
            toc.push((id.clone(), format!("{}: {}", kind, name)));
            entry.push_str(&format!("<a id=\"{}\"></a>\n", id));
            entry.push_str(&format!("## {}: {}\n\n", kind, name));
            let args = f
                .params
                .iter()
                .map(|a| format!("{}: {}", a.name, a.ty))
                .collect::<Vec<_>>()
                .join(", ");
            entry.push_str(&format!(
                "```\nfun {}({}) -> {} \n```\n\n",
                name, args, f.return_ty
            ));
            if let Some(d) = f.docstring {
                entry.push_str(d);
                entry.push_str("\n\n");
            }
        }

        // Structs
        for (name, s) in hir.structs.iter() {
            // Skip private structs
            if s.vis == HirVisibility::Private {
                continue;
            }
            let path = s.declaration_span.path;
            all_paths.push(path.to_string());
            let entry = files_md
                .entry(path.to_string())
                .or_insert_with(|| String::from("# Documentation\n\n"));
            let toc = files_toc.entry(path.to_string()).or_default();
            let kind = "Struct";
            let id = slugify(kind, name);
            toc.push((id.clone(), format!("{}: {}", kind, name)));
            entry.push_str(&format!("<a id=\"{}\"></a>\n", id));
            entry.push_str(&format!("## {}: {}\n\n", kind, name));
            if let Some(d) = s.docstring {
                entry.push_str(d);
                entry.push_str("\n\n");
            }
            if !s.fields.is_empty() {
                entry.push_str("**Fields**\n\n");
                for (_k, f) in s.fields.iter() {
                    entry.push_str(&format!("- {}: {}\n", f.name, f.ty));
                }
                entry.push('\n');
            }

            // Methods (render signatures using HirPrettyPrinter)
            if !s.methods.is_empty() {
                entry.push_str("**Methods**\n\n");
                for (mname, msig) in s.methods.iter() {
                    if msig.vis == HirVisibility::Private {
                        continue;
                    }
                    let mut pp = HirPrettyPrinter::new();
                    pp.print_method_signature(mname, msig);
                    let sig = pp.get_output();
                    entry.push_str(&format!("```\n{}\n```\n\n", sig));
                    // Docstring
                    if let Some(d) = msig.docstring {
                        entry.push_str(d);
                        entry.push_str("\n\n");
                    }
                }
            }
        }

        // Enums
        for (name, e) in hir.enums.iter() {
            // Skip private enums
            if e.vis == HirVisibility::Private {
                continue;
            }
            let path = e.span.path;
            all_paths.push(path.to_string());
            let entry = files_md
                .entry(path.to_string())
                .or_insert_with(|| String::from("# Documentation\n\n"));
            let toc = files_toc.entry(path.to_string()).or_default();
            let kind = "Enum";
            let id = slugify(kind, name);
            toc.push((id.clone(), format!("{}: {}", kind, name)));
            entry.push_str(&format!("<a id=\"{}\"></a>\n", id));
            entry.push_str(&format!("## {}: {}\n\n", kind, name));
            if let Some(d) = e.docstring {
                entry.push_str(d);
                entry.push_str("\n\n");
            }
            if !e.variants.is_empty() {
                entry.push_str("**Variants**\n\n");
                for v in e.variants.iter() {
                    entry.push_str(&format!("- {} = {}\n", v.name, v.value));
                }
                entry.push('\n');
            }
        }

        // Unions
        for (name, u) in hir.unions.iter() {
            if u.vis == HirVisibility::Private {
                continue;
            }
            let path = u.declaration_span.path;
            all_paths.push(path.to_string());
            let entry = files_md
                .entry(path.to_string())
                .or_insert_with(|| String::from("# Documentation\n\n"));
            let toc = files_toc.entry(path.to_string()).or_default();
            let kind = "Union";
            let id = slugify(kind, name);
            toc.push((id.clone(), format!("{}: {}", kind, name)));
            entry.push_str(&format!("<a id=\"{}\"></a>\n", id));
            entry.push_str(&format!("## {}: {}\n\n", kind, name));
            if let Some(d) = u.docstring {
                entry.push_str(d);
                entry.push_str("\n\n");
            }
        }

        if files_md.is_empty() {
            let md_file = out_dir.join("documentation.md");
            std::fs::write(&md_file, "# Documentation\n\n(No items)")?;
            let content_html = md_to_html("# Documentation\n\n(No items)");
            let out_file = out_dir.join("documentation.html");
            std::fs::write(&out_file, content_html)?;
            println!("[atlas_docs] wrote {}", out_file.display());
            return Ok(());
        }

        // compute common prefix to preserve directory layout
        let norm_paths: Vec<String> = all_paths.iter().map(|p| p.replace('\\', "/")).collect();
        let comps: Vec<Vec<&str>> = norm_paths.iter().map(|p| p.split('/').collect()).collect();
        let min_len = comps.iter().map(|c| c.len()).min().unwrap_or(0);
        let mut common_idx = 0usize;
        'outer: for i in 0..min_len {
            let v = comps[0][i];
            for c in &comps[1..] {
                if c[i] != v {
                    break 'outer;
                }
            }
            common_idx += 1;
        }
        let common_prefix = if common_idx > 0 {
            comps[0][..common_idx].join("/")
        } else {
            String::new()
        };

        let mut generated_files: Vec<(String, String)> = Vec::new();
        for (src_path, md) in files_md.iter() {
            let norm = src_path.replace('\\', "/");
            let rel = if !common_prefix.is_empty() && norm.starts_with(&common_prefix) {
                norm[common_prefix.len() + 1..].to_string()
            } else {
                norm.clone()
            };
            // Use the source-relative parent directory to preserve layout, and the file stem for name.
            let rel_path = Path::new(&rel);
            let parent_dir = rel_path.parent().and_then(|p| p.to_str()).unwrap_or("");
            let base_name = rel_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("doc")
                .replace(':', "_");
            // Create destination subdirectory under the output dir mirroring source layout.
            let dst_dir = if parent_dir.is_empty() {
                out_dir.to_path_buf()
            } else {
                out_dir.join(parent_dir)
            };
            std::fs::create_dir_all(&dst_dir)?;
            let md_file = dst_dir.join(format!("{}.md", base_name));
            std::fs::write(&md_file, md)?;
            println!("[atlas_docs] wrote {}", md_file.display());

            let content_html = md_to_html(md);

            let toc = files_toc.get(src_path).cloned().unwrap_or_default();
            let mut html = String::new();
            html.push_str("<!doctype html><html><head><meta charset=\"utf-8\">\n");
            html.push_str(&format!("<title>{}</title>\n", base_name));
            if Path::new("src/atlas_docs/templates/style.css").exists() {
                html.push_str("<link rel=\"stylesheet\" href=\"style.css\">\n");
            } else {
                html.push_str("<style>body{font-family:Arial,Helvetica,sans-serif;margin:0}#wrap{display:flex}#toc{width:260px;padding:16px;border-right:1px solid #eee;overflow:auto;height:100vh}#main{flex:1;padding:24px;overflow:auto}pre{background:#f6f8fa;padding:12px;border-radius:6px}</style>\n");
            }
            html.push_str("</head><body>\n");
            html.push_str(&format!(
                "<h1 style=\"margin:16px\">Documentation for {}</h1>\n",
                src_path
            ));
            html.push_str("<div id=\"wrap\">\n");
            html.push_str("<nav id=\"toc\">\n<h3>Contents</h3>\n<ul>\n");
            for (id, title) in &toc {
                html.push_str(&format!(
                    "<li><a href=\"#{}\">{}</a></li>\n",
                    escape_html(id),
                    escape_html(title)
                ));
            }
            html.push_str("</ul>\n</nav>\n");
            html.push_str("<main id=\"main\">\n");
            html.push_str(&content_html);
            html.push_str("</main>\n");
            html.push_str("</div>\n");
            html.push_str("</body></html>");

            let out_file = dst_dir.join(format!("{}.html", base_name));
            std::fs::write(&out_file, html)?;
            println!("[atlas_docs] wrote {}", out_file.display());

            // Build a relative link from the output dir root to the generated HTML file.
            let rel_link = if parent_dir.is_empty() {
                format!("{}.html", base_name)
            } else {
                format!("{}/{}.html", parent_dir, base_name)
            };
            generated_files.push((rel_link, base_name.to_string()));
        }

        use std::collections::BTreeMap;

        // Group generated files by their parent directory so the index reflects layout.
        let mut groups: BTreeMap<String, Vec<(String, String)>> = BTreeMap::new();
        for (rel, title) in &generated_files {
            let parent = std::path::Path::new(rel)
                .parent()
                .and_then(|p| p.to_str())
                .unwrap_or("")
                .to_string();
            groups
                .entry(parent)
                .or_default()
                .push((rel.clone(), title.clone()));
        }

        let mut index_md = String::new();
        index_md.push_str("# Index\n\n");
        for (dir, items) in &groups {
            if dir.is_empty() {
                index_md.push_str("## Root\n\n");
            } else {
                index_md.push_str(&format!("## {}/\n\n", dir));
            }
            for (rel, title) in items {
                index_md.push_str(&format!("- [{}]({})\n", title, rel));
            }
            index_md.push('\n');
        }
        let index_md_file = out_dir.join("index.md");
        std::fs::write(&index_md_file, &index_md)?;
        let index_html = md_to_html(&index_md);
        let index_out = out_dir.join("index.html");
        std::fs::write(&index_out, index_html)?;
        println!("[atlas_docs] wrote {}", index_out.display());
        Ok(())
    }
}
