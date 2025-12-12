use std::collections::HashMap;

use crate::atlas_c::utils::Span;

#[derive(Debug, Clone)]
/// Maps variable names to their status within scopes
///
/// We need `Clone` here because when we encounter an if/else or a loop with an early return,
/// we need to clone the current scope map so we don't change the state of the outer scope
/// for the rest of the pass.
pub struct ScopeMap<'hir> {
    // Stack of variable maps for each scope
    pub scopes: Vec<VarMap<'hir>>,
}

impl<'hir> ScopeMap<'hir> {
    pub fn new() -> Self {
        Self {
            scopes: vec![VarMap::new(None)],
        }
    }

    pub fn new_scope(&mut self) -> usize {
        let parent = self.scopes.len() - 1;
        self.scopes.push(VarMap::new(Some(parent)));
        parent
    }

    pub fn end_scope(&mut self) -> usize {
        self.scopes.pop();
        self.scopes.len() - 1
    }

    pub fn get(&self, name: &str) -> Option<&VarData<'hir>> {
        let scope = self.scopes.last().unwrap();
        match scope.var_status.get(name) {
            Some(s) => Some(s),
            None => {
                let mut parent = scope.parent;
                while parent.is_some() {
                    let parent_scope = &self.scopes[parent.unwrap()];
                    match parent_scope.var_status.get(name) {
                        Some(s) => return Some(s),
                        None => parent = parent_scope.parent,
                    }
                }
                None
            }
        }
    }

    pub fn insert(&mut self, name: &'hir str, var: VarData<'hir>) {
        self.scopes.last_mut().unwrap().var_status.insert(name, var);
    }

    pub fn mark_as_moved(&mut self, name: &str) -> Option<()> {
        let scope = self.scopes.last_mut().unwrap();
        if let Some(var) = scope.var_status.get_mut(name) {
            var.status = VarStatus::Moved;
            Some(())
        } else {
            None
        }
    }

    /// Returns all the variables that are valid in the current scope
    ///
    /// Used when exiting a scope to determine which variables need to be deleted
    pub fn get_valid_vars(&self) -> Vec<&VarData<'hir>> {
        let mut valid_vars = Vec::new();
        let scope = self.scopes.last().unwrap();
        for var in scope.var_status.values() {
            if var.status == VarStatus::Valid {
                valid_vars.push(var);
            }
        }
        valid_vars
    }

    /// Returns all the variables that are valid from the current scope and parent scopes
    ///
    /// Used when encountering a `return` statement to determine which variables need to be deleted
    ///
    /// The variables are returned in reverse order of declaration (i.e., the most recently declared variable first)
    pub fn get_all_vars(&self) -> Vec<&VarData<'hir>> {
        let mut valid_vars = Vec::new();
        let mut current_scope_index = self.scopes.len() - 1;
        loop {
            let scope = &self.scopes[current_scope_index];
            for var in scope.var_status.values() {
                if var.status == VarStatus::Valid {
                    valid_vars.push(var);
                }
            }
            if let Some(parent_index) = scope.parent {
                current_scope_index = parent_index;
            } else {
                break;
            }
        }
        valid_vars
    }
}

#[derive(Debug, Clone)]
/// Contains all variable information for a given scope
pub struct VarMap<'hir> {
    /// Map variable names to their moved/deleted status
    pub var_status: HashMap<&'hir str, VarData<'hir>>,
    /// The parent scope index, if any
    pub parent: Option<usize>,
}

impl<'hir> VarMap<'hir> {
    pub fn new(parent: Option<usize>) -> Self {
        Self {
            var_status: HashMap::new(),
            parent,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarData<'hir> {
    pub name: &'hir str,
    pub status: VarStatus,
    //We store the span for better error messages
    pub span: Span,
    pub kind: VarKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarKind {
    /// Primitive types (int, float, bool, char)
    Primitive,
    /// Reference types (&T, &const T)
    Reference,
    /// Complex types (structs, arrays, etc.)
    Object,
}

impl<'hir> VarData<'hir> {
    pub fn new(name: &'hir str, status: VarStatus, span: Span, kind: VarKind) -> Self {
        Self {
            name,
            status,
            span,
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarStatus {
    /// The variable is valid and can be used
    Valid,
    /// The variable has been moved and cannot be used
    Moved,
    /// The variable has been deleted and cannot be used
    Deleted,
}
