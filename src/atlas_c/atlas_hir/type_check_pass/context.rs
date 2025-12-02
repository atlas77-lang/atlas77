use crate::atlas_c::atlas_hir::ty::HirTy;
use crate::atlas_c::utils::Span;
use std::collections::HashMap;

pub struct ContextFunction<'hir> {
    pub scopes: Vec<ContextScope<'hir>>,
}

impl Default for ContextFunction<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> ContextFunction<'hir> {
    pub fn new() -> Self {
        Self {
            scopes: vec![ContextScope::new(None)],
        }
    }
    pub fn new_scope(&mut self) -> usize {
        let parent = self.scopes.len() - 1;
        self.scopes.push(ContextScope::new(Some(parent)));
        parent
    }
    pub fn end_scope(&mut self) -> usize {
        self.scopes.pop();
        self.scopes.len() - 1
    }

    pub fn get(&self, name: &str) -> Option<&ContextVariable<'hir>> {
        let scope = self.scopes.last().unwrap();
        match scope.get(name) {
            Some(s) => Some(s),
            None => {
                let mut parent = scope.parent;
                while parent.is_some() {
                    let parent_scope = &self.scopes[parent.unwrap()];
                    match parent_scope.get(name) {
                        Some(s) => return Some(s),
                        None => parent = parent_scope.parent,
                    }
                }
                None
            }
        }
    }

    pub fn insert(&mut self, name: &'hir str, var: ContextVariable<'hir>) {
        self.scopes.last_mut().unwrap().insert(name, var);
    }
}

#[derive(Debug)]
pub struct ContextScope<'hir> {
    ///I should stop using HashMap everywhere. A ContextVariable should be `(depth, &'hir str)`
    /// depth as in the scope depth
    pub variables: HashMap<&'hir str, ContextVariable<'hir>>,
    pub parent: Option<usize>,
}

impl<'hir> ContextScope<'hir> {
    pub fn new(parent: Option<usize>) -> Self {
        Self {
            variables: HashMap::new(),
            parent,
        }
    }
    pub fn get(&self, name: &str) -> Option<&ContextVariable<'hir>> {
        self.variables.get(name)
    }
    pub fn insert(&mut self, name: &'hir str, var: ContextVariable<'hir>) {
        self.variables.insert(name, var);
    }
}

#[derive(Debug)]
pub struct ContextVariable<'hir> {
    pub _name: &'hir str,
    pub name_span: Span,
    pub ty: &'hir HirTy<'hir>,
    pub ty_span: Span,
    pub is_mut: bool,
}
