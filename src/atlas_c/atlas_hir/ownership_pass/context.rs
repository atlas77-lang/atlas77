use std::collections::HashMap;

use crate::atlas_c::{atlas_hir::ty::HirTy, utils::Span};

/// Tracks where a reference value originates from.
///
/// This is used to detect use-after-free bugs: when the origin of a reference
/// is deleted or moved, all references derived from it become invalid.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum ReferenceOrigin<'hir> {
    /// No origin tracking (for non-reference types)
    #[default]
    None,
    /// Reference originates from a specific variable
    Variable(&'hir str),
    /// Reference originates from multiple possible sources (e.g., from a function
    /// that takes multiple reference arguments)
    Union(Vec<ReferenceOrigin<'hir>>),
}

impl<'hir> ReferenceOrigin<'hir> {
    /// Check if this origin includes a specific variable name
    pub fn includes(&self, var_name: &str) -> bool {
        match self {
            ReferenceOrigin::None => false,
            ReferenceOrigin::Variable(name) => *name == var_name,
            ReferenceOrigin::Union(origins) => origins.iter().any(|o| o.includes(var_name)),
        }
    }

    /// Merge two origins into a union
    pub fn merge(self, other: ReferenceOrigin<'hir>) -> ReferenceOrigin<'hir> {
        match (self, other) {
            (ReferenceOrigin::None, other) => other,
            (this, ReferenceOrigin::None) => this,
            (ReferenceOrigin::Variable(a), ReferenceOrigin::Variable(b)) if a == b => {
                ReferenceOrigin::Variable(a)
            }
            (ReferenceOrigin::Variable(a), ReferenceOrigin::Variable(b)) => {
                ReferenceOrigin::Union(vec![
                    ReferenceOrigin::Variable(a),
                    ReferenceOrigin::Variable(b),
                ])
            }
            (ReferenceOrigin::Union(mut origins), ReferenceOrigin::Variable(v)) => {
                if !origins
                    .iter()
                    .any(|o| matches!(o, ReferenceOrigin::Variable(x) if *x == v))
                {
                    origins.push(ReferenceOrigin::Variable(v));
                }
                ReferenceOrigin::Union(origins)
            }
            (ReferenceOrigin::Variable(v), ReferenceOrigin::Union(mut origins)) => {
                if !origins
                    .iter()
                    .any(|o| matches!(o, ReferenceOrigin::Variable(x) if *x == v))
                {
                    origins.insert(0, ReferenceOrigin::Variable(v));
                }
                ReferenceOrigin::Union(origins)
            }
            (ReferenceOrigin::Union(mut a), ReferenceOrigin::Union(b)) => {
                for origin in b {
                    if !a.contains(&origin) {
                        a.push(origin);
                    }
                }
                ReferenceOrigin::Union(a)
            }
        }
    }
}

/// Represents how a variable is used in an expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UseKind {
    /// The variable is read but ownership is not transferred (e.g., in a binary operation, borrow)
    Read,
    /// The variable's ownership is consumed (assignment, function arg by value, return)
    OwnershipConsuming,
}

/// Represents a single use of a variable
#[derive(Debug, Clone)]
pub struct VarUse {
    pub span: Span,
    pub kind: UseKind,
    /// Index of the statement where this use occurs (for ordering)
    pub stmt_index: usize,
}

/// Maps variable names to their status within scopes
///
/// We need `Clone` here because when we encounter an if/else or a loop with an early return,
/// we need to clone the current scope map so we don't change the state of the outer scope
/// for the rest of the pass.
#[derive(Debug, Clone)]
pub struct ScopeMap<'hir> {
    /// Stack of variable maps for each scope
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

    pub fn get_mut(&mut self, name: &str) -> Option<&mut VarData<'hir>> {
        // First, find which scope contains the variable
        let scope_index = {
            let scope = self.scopes.last().unwrap();
            if scope.var_status.contains_key(name) {
                Some(self.scopes.len() - 1)
            } else {
                let mut parent = scope.parent;
                let mut found_index = None;
                while parent.is_some() {
                    let parent_scope = &self.scopes[parent.unwrap()];
                    if parent_scope.var_status.contains_key(name) {
                        found_index = parent;
                        break;
                    }
                    parent = parent_scope.parent;
                }
                found_index
            }
        };

        // Now get mutable access to that scope
        if let Some(idx) = scope_index {
            self.scopes
                .get_mut(idx)
                .and_then(|s| s.var_status.get_mut(name))
        } else {
            None
        }
    }

    pub fn insert(&mut self, name: &'hir str, var: VarData<'hir>) {
        self.scopes.last_mut().unwrap().var_status.insert(name, var);
    }

    pub fn mark_as_moved(&mut self, name: &str, move_span: Span) -> Option<()> {
        if let Some(var) = self.get_mut(name) {
            var.status = VarStatus::Moved { move_span };
            Some(())
        } else {
            None
        }
    }

    /// Invalidate all references whose origin includes the given variable name.
    /// This should be called when a variable is deleted or consumed by a method.
    ///
    /// Returns the names of variables that were invalidated.
    pub fn invalidate_references_with_origin(
        &mut self,
        origin_name: &str,
        invalidation_span: Span,
    ) -> Vec<&'hir str> {
        let mut invalidated = Vec::new();

        // Check all scopes for references that depend on this origin
        for scope in &mut self.scopes {
            for (var_name, var_data) in scope.var_status.iter_mut() {
                // Only invalidate reference types that depend on this origin
                if var_data.kind == VarKind::Reference
                    && var_data.origin.includes(origin_name)
                    && var_data.status.is_valid()
                {
                    var_data.status = VarStatus::Deleted {
                        delete_span: invalidation_span,
                    };
                    invalidated.push(*var_name);
                }
            }
        }

        invalidated
    }

    pub fn mark_as_borrowed(&mut self, name: &str) -> Option<()> {
        if let Some(var) = self.get_mut(name) {
            var.borrow_count += 1;
            Some(())
        } else {
            None
        }
    }

    pub fn record_use(&mut self, name: &str, use_info: VarUse) -> Option<()> {
        if let Some(var) = self.get_mut(name) {
            var.uses.push(use_info);
            Some(())
        } else {
            None
        }
    }

    /// Returns all the variables that are owned (valid) in the current scope only
    ///
    /// Used when exiting a scope to determine which variables need to be deleted
    /// Returns variables in reverse order of declaration (LIFO) for proper cleanup
    pub fn get_owned_vars_in_current_scope(&self) -> Vec<&VarData<'hir>> {
        let mut owned_vars = Vec::new();
        let scope = self.scopes.last().unwrap();
        for var in scope.var_status.values() {
            if var.status == VarStatus::Owned && var.kind != VarKind::Reference {
                owned_vars.push(var);
            }
        }
        // Sort by declaration order (descending), then by name (descending) for determinism
        // This ensures most recently declared variables are deleted first (LIFO order)
        owned_vars.sort_by(|a, b| {
            b.declaration_stmt_index
                .cmp(&a.declaration_stmt_index)
                .then_with(|| b.name.cmp(a.name))
        });
        owned_vars
    }

    /// Returns all the variables that are owned from the current scope and all parent scopes
    ///
    /// Used when encountering a `return` statement to determine which variables need to be deleted
    /// before returning.
    ///
    /// The variables are returned in reverse order of declaration (LIFO).
    pub fn get_all_owned_vars(&self) -> Vec<&VarData<'hir>> {
        let mut owned_vars = Vec::new();
        let mut current_scope_index = self.scopes.len() - 1;
        loop {
            let scope = &self.scopes[current_scope_index];
            for var in scope.var_status.values() {
                if var.status == VarStatus::Owned && var.kind != VarKind::Reference {
                    owned_vars.push(var);
                }
            }
            if let Some(parent_index) = scope.parent {
                current_scope_index = parent_index;
            } else {
                break;
            }
        }
        // Sort by declaration order (descending), then by name (descending) for determinism
        // This ensures most recently declared variables are deleted first (LIFO order)
        owned_vars.sort_by(|a, b| {
            b.declaration_stmt_index
                .cmp(&a.declaration_stmt_index)
                .then_with(|| b.name.cmp(a.name))
        });
        owned_vars
    }

    /// Merge ownership states after an if-else statement.
    ///
    /// This handles the case where a variable is moved in one branch but not the other.
    /// After merging:
    /// - If moved in both branches → Moved
    /// - If moved in one branch only → ConditionallyMoved  
    /// - If owned in both → Owned (unchanged)
    ///
    /// `then_state` is the state after the then branch
    /// `else_state` is the state after the else branch (or the original state if no else)
    /// The result is applied to the current scope map.
    pub fn merge_branch_states(
        &mut self,
        then_state: &ScopeMap<'hir>,
        else_state: &ScopeMap<'hir>,
    ) {
        // We need to check variables that exist in parent scopes (not the if/else inner scopes)
        // because those are the ones that could have been moved inside the branches
        let current_scope = self.scopes.last_mut().unwrap();

        for (name, var_data) in current_scope.var_status.iter_mut() {
            let then_var = then_state.get(name);
            let else_var = else_state.get(name);

            match (then_var, else_var) {
                (Some(then_v), Some(else_v)) => {
                    let then_moved = matches!(
                        then_v.status,
                        VarStatus::Moved { .. } | VarStatus::ConditionallyMoved { .. }
                    );
                    let else_moved = matches!(
                        else_v.status,
                        VarStatus::Moved { .. } | VarStatus::ConditionallyMoved { .. }
                    );

                    match (then_moved, else_moved) {
                        (true, true) => {
                            // Moved in both branches - definitely moved
                            if let VarStatus::Moved { move_span } = &then_v.status {
                                var_data.status = VarStatus::Moved {
                                    move_span: *move_span,
                                };
                            } else if let VarStatus::ConditionallyMoved { move_span } =
                                &then_v.status
                            {
                                var_data.status = VarStatus::Moved {
                                    move_span: *move_span,
                                };
                            }
                        }
                        (true, false) => {
                            // Moved only in then branch - conditionally moved
                            if let VarStatus::Moved { move_span } = &then_v.status {
                                var_data.status = VarStatus::ConditionallyMoved {
                                    move_span: *move_span,
                                };
                            }
                        }
                        (false, true) => {
                            // Moved only in else branch - conditionally moved
                            if let VarStatus::Moved { move_span } = &else_v.status {
                                var_data.status = VarStatus::ConditionallyMoved {
                                    move_span: *move_span,
                                };
                            }
                        }
                        (false, false) => {
                            // Not moved in either branch - keep as owned
                        }
                    }
                }
                _ => {
                    // Variable not found in one of the states - shouldn't happen
                    // but leave as is
                }
            }
        }

        // Also check parent scopes
        for scope_idx in (0..self.scopes.len() - 1).rev() {
            let scope = &mut self.scopes[scope_idx];
            for (name, var_data) in scope.var_status.iter_mut() {
                let then_var = then_state.get(name);
                let else_var = else_state.get(name);

                match (then_var, else_var) {
                    (Some(then_v), Some(else_v)) => {
                        let then_moved = matches!(
                            then_v.status,
                            VarStatus::Moved { .. } | VarStatus::ConditionallyMoved { .. }
                        );
                        let else_moved = matches!(
                            else_v.status,
                            VarStatus::Moved { .. } | VarStatus::ConditionallyMoved { .. }
                        );

                        match (then_moved, else_moved) {
                            (true, true) => {
                                if let VarStatus::Moved { move_span } = &then_v.status {
                                    var_data.status = VarStatus::Moved {
                                        move_span: *move_span,
                                    };
                                } else if let VarStatus::ConditionallyMoved { move_span } =
                                    &then_v.status
                                {
                                    var_data.status = VarStatus::Moved {
                                        move_span: *move_span,
                                    };
                                }
                            }
                            (true, false) => {
                                if let VarStatus::Moved { move_span } = &then_v.status {
                                    var_data.status = VarStatus::ConditionallyMoved {
                                        move_span: *move_span,
                                    };
                                }
                            }
                            (false, true) => {
                                if let VarStatus::Moved { move_span } = &else_v.status {
                                    var_data.status = VarStatus::ConditionallyMoved {
                                        move_span: *move_span,
                                    };
                                }
                            }
                            (false, false) => {}
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Get variables that were moved in one branch but still owned in another.
    /// Returns pairs of (variable_name, was_moved_in_then_branch).
    /// This is used to insert delete statements in the branch that didn't move.
    pub fn get_conditionally_moved_vars<'a>(
        &self,
        then_state: &'a ScopeMap<'hir>,
        else_state: &'a ScopeMap<'hir>,
    ) -> Vec<(&'hir str, bool, &'a VarData<'hir>)> {
        let mut result = Vec::new();

        // Check all scopes for variables that are moved in one branch but not the other
        for scope in &self.scopes {
            for name in scope.var_status.keys() {
                let then_var = then_state.get(name);
                let else_var = else_state.get(name);

                if let (Some(then_v), Some(else_v)) = (then_var, else_var) {
                    let then_moved = matches!(then_v.status, VarStatus::Moved { .. });
                    let else_moved = matches!(else_v.status, VarStatus::Moved { .. });

                    match (then_moved, else_moved) {
                        (true, false) => {
                            // Moved in then, still owned in else - need delete in else
                            result.push((*name, true, else_v));
                        }
                        (false, true) => {
                            // Moved in else, still owned in then - need delete in then
                            result.push((*name, false, then_v));
                        }
                        _ => {}
                    }
                }
            }
        }

        result
    }
}

/// Contains all variable information for a given scope
#[derive(Debug, Clone)]
pub struct VarMap<'hir> {
    /// Map variable names to their status and metadata
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

/// Complete data about a variable for ownership analysis
#[derive(Debug, Clone)]
pub struct VarData<'hir> {
    /// Variable name
    pub name: &'hir str,
    /// Current ownership status
    pub status: VarStatus,
    /// Span where the variable was declared
    pub span: Span,
    /// Kind of variable (determines copy/move semantics)
    pub kind: VarKind,
    /// The type of the variable (needed to check for copy constructor)
    pub ty: &'hir HirTy<'hir>,
    /// Whether this type has a copy constructor
    pub is_copyable: bool,
    /// All uses of this variable (for last-use analysis)
    pub uses: Vec<VarUse>,
    /// Number of active borrows
    pub borrow_count: usize,
    /// Statement index where this variable was declared
    pub declaration_stmt_index: usize,
    /// For reference types: tracks what variable(s) this reference points into.
    /// When the origin is deleted/moved, this reference becomes invalid.
    pub origin: ReferenceOrigin<'hir>,
}

impl<'hir> VarData<'hir> {
    pub fn new(
        name: &'hir str,
        span: Span,
        kind: VarKind,
        ty: &'hir HirTy<'hir>,
        is_copyable: bool,
        declaration_stmt_index: usize,
    ) -> Self {
        Self {
            name,
            status: VarStatus::Owned,
            span,
            kind,
            ty,
            is_copyable,
            uses: Vec::new(),
            borrow_count: 0,
            declaration_stmt_index,
            origin: ReferenceOrigin::None,
        }
    }

    /// Create a new VarData with a specific origin (for reference types)
    pub fn with_origin(
        name: &'hir str,
        span: Span,
        kind: VarKind,
        ty: &'hir HirTy<'hir>,
        is_copyable: bool,
        declaration_stmt_index: usize,
        origin: ReferenceOrigin<'hir>,
    ) -> Self {
        Self {
            name,
            status: VarStatus::Owned,
            span,
            kind,
            ty,
            is_copyable,
            uses: Vec::new(),
            borrow_count: 0,
            declaration_stmt_index,
            origin,
        }
    }

    /// Returns the last ownership-consuming use, if any
    pub fn last_ownership_consuming_use(&self) -> Option<&VarUse> {
        self.uses
            .iter()
            .rfind(|u| u.kind == UseKind::OwnershipConsuming)
    }

    /// Check if a given use is the last ownership-consuming use
    pub fn is_last_ownership_use(&self, stmt_index: usize) -> bool {
        self.last_ownership_consuming_use()
            .map(|u| u.stmt_index == stmt_index)
            .unwrap_or(false)
    }

    /// Check if there are any uses (read or consuming) after the given statement index
    pub fn has_any_use_after(&self, stmt_index: usize) -> bool {
        self.uses.iter().any(|u| u.stmt_index > stmt_index)
    }

    /// Check if we can safely move at this statement (no more uses after)
    pub fn can_move_at(&self, stmt_index: usize) -> bool {
        !self.has_any_use_after(stmt_index)
    }
}

/// The kind of variable (determines default copy/move behavior)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarKind {
    /// Primitive types (int, float, bool, char, uint) - always implicitly copyable
    Primitive,
    /// Reference types (&T, &const T) - never owned, just pointers
    Reference,
    /// Object types (structs, generics) - copyable only if they have a copy constructor
    Object,
    /// String type - special handling (always copyable as it's a built-in)
    String,
    /// List type - copyable if inner type is copyable
    List,
}

impl VarKind {
    /// Returns true if this kind is always implicitly copyable (no copy constructor needed)
    pub fn is_implicitly_copyable(&self) -> bool {
        matches!(
            self,
            VarKind::Primitive | VarKind::Reference | VarKind::String
        )
    }
}

/// Ownership status of a variable
#[derive(Debug, Clone)]
pub enum VarStatus {
    /// The variable owns its value and can be used
    Owned,
    /// The variable's value has been moved to another location
    Moved { move_span: Span },
    /// The variable has been explicitly deleted
    Deleted { delete_span: Span },
    /// The variable is borrowed (references only)
    Borrowed,
    /// The variable was moved in one branch of a conditional but not the other.
    /// This means the outer scope should NOT generate a delete for it - each branch
    /// is responsible for handling its own cleanup.
    ConditionallyMoved { move_span: Span },
}

// For backwards compatibility with existing code
impl VarStatus {
    pub fn is_valid(&self) -> bool {
        matches!(self, VarStatus::Owned | VarStatus::Borrowed)
    }
}

impl PartialEq for VarStatus {
    fn eq(&self, other: &VarStatus) -> bool {
        matches!(
            (self, other),
            (VarStatus::Owned, VarStatus::Owned)
                | (VarStatus::Moved { .. }, VarStatus::Moved { .. })
                | (VarStatus::Deleted { .. }, VarStatus::Deleted { .. })
                | (VarStatus::Borrowed, VarStatus::Borrowed)
                | (
                    VarStatus::ConditionallyMoved { .. },
                    VarStatus::ConditionallyMoved { .. }
                )
        )
    }
}

impl Eq for VarStatus {}
