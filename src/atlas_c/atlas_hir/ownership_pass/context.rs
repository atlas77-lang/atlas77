//! Context structures for the ownership pass.
//!
//! This module defines the data structures for tracking:
//! - Variable states (owned, borrowed, moved, deleted)
//! - Lifetimes for references
//! - Scope information

use crate::atlas_c::atlas_hir::ty::HirTy;
use crate::atlas_c::utils::Span;
use std::collections::HashMap;

/// Unique identifier for a variable in the ownership pass
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

/// Unique identifier for a scope
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ScopeId(pub usize);

/// Unique identifier for a heap allocation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AllocId(pub usize);

/// Type classification for ownership semantics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCategory {
    /// Trivial types: can be bitwise copied, no destructor needed
    /// Examples: primitives, raw pointers, arrays of trivial types
    Trivial,
    /// Resource types: own heap resources, require cleanup
    /// Examples: String, Vec<T>, File
    Resource,
    /// View types: non-owning references to data
    /// Examples: &T, &const T, slices
    View,
}

/// Status of a variable during ownership analysis
#[derive(Debug, Clone)]
pub enum VarStatus {
    /// Variable owns its value and is valid for use
    Owned,
    /// Variable is currently borrowed
    Borrowed {
        /// The VarId that holds the borrow
        by: VarId,
        /// Whether it's a mutable borrow
        is_mutable: bool,
    },
    /// Variable has been moved from (use-after-move is UB/warning)
    Moved {
        /// Where the move occurred
        move_span: Span,
    },
    /// Variable has been moved in some branches but not others
    ConditionallyMoved {
        /// Where the move occurred
        move_span: Span,
    },
    /// Variable has been explicitly deleted
    Deleted {
        /// Where the delete occurred
        delete_span: Span,
    },
}

impl Default for VarStatus {
    fn default() -> Self {
        Self::Owned
    }
}

/// Lifetime scope information for references
#[derive(Debug, Clone)]
pub struct LifetimeScope {
    pub scope_id: ScopeId,
    /// Whether this value can escape the current function (e.g., function parameters can)
    pub can_escape: bool,
}

/// Information about an active borrow
#[derive(Debug, Clone)]
pub struct BorrowInfo {
    /// The variable ID of the reference
    pub ref_var: VarId,
    /// Whether it's a mutable borrow
    pub is_mutable: bool,
    /// Where the borrow occurred
    pub span: Span,
}

/// Tracked variable in the ownership pass
#[derive(Debug, Clone)]
pub struct OwnershipVariable<'hir> {
    /// Variable name
    pub name: &'hir str,
    /// Variable ID
    pub var_id: VarId,
    /// Variable type
    pub ty: &'hir HirTy<'hir>,
    /// Type category
    pub category: TypeCategory,
    /// Current status
    pub status: VarStatus,
    /// Span of the variable declaration
    pub declaration_span: Span,
    /// Whether this is a function parameter (vs local variable)
    pub is_param: bool,
    /// Lifetime scope
    pub lifetime: LifetimeScope,
    /// Local variables that this variable references (for reference lifetime tracking)
    pub refs_locals: Vec<VarId>,
}

/// Heap allocation tracking
#[derive(Debug, Clone)]
pub struct HeapAllocation<'hir> {
    pub ty: &'hir HirTy<'hir>,
    pub span: Span,
    pub freed: bool,
}

/// Scope in the ownership pass
#[derive(Debug)]
pub struct OwnershipScope<'hir> {
    /// Scope identifier
    pub scope_id: ScopeId,
    /// Variables declared in this scope
    pub variables: HashMap<&'hir str, VarId>,
    /// Parent scope
    pub parent: Option<ScopeId>,
    /// Whether this is a loop scope (affects move semantics)
    pub is_loop: bool,
    /// Span of the scope
    pub span: Span,
}

/// Function context for ownership analysis
#[derive(Debug)]
pub struct OwnershipFunction<'hir> {
    /// All variables in this function (including nested scopes)
    pub all_variables: HashMap<VarId, OwnershipVariable<'hir>>,
    /// Scope stack
    pub scopes: Vec<OwnershipScope<'hir>>,
    /// Next variable ID
    pub next_var_id: usize,
    /// Next scope ID
    pub next_scope_id: usize,
    /// Active borrows: borrowed variable -> list of active borrows
    pub active_borrows: HashMap<VarId, Vec<BorrowInfo>>,
    /// Lifetime tracking: variable -> list of origin variables it references
    pub reference_origins: HashMap<VarId, Vec<VarId>>,
    /// Heap allocations
    pub heap_allocations: HashMap<AllocId, HeapAllocation<'hir>>,
    /// Next allocation ID
    pub next_alloc_id: usize,
}

impl Default for OwnershipFunction<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> OwnershipFunction<'hir> {
    pub fn new() -> Self {
        Self {
            all_variables: HashMap::new(),
            scopes: Vec::new(),
            next_var_id: 0,
            next_scope_id: 0,
            active_borrows: HashMap::new(),
            reference_origins: HashMap::new(),
            heap_allocations: HashMap::new(),
            next_alloc_id: 0,
        }
    }

    /// Create a new variable ID
    pub fn create_var_id(&mut self) -> VarId {
        let id = VarId(self.next_var_id);
        self.next_var_id += 1;
        id
    }

    /// Create a new scope ID
    pub fn create_scope_id(&mut self) -> ScopeId {
        let id = ScopeId(self.next_scope_id);
        self.next_scope_id += 1;
        id
    }

    /// Create a new allocation ID
    pub fn create_alloc_id(&mut self) -> AllocId {
        let id = AllocId(self.next_alloc_id);
        self.next_alloc_id += 1;
        id
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, span: Span, is_loop: bool) -> ScopeId {
        let scope_id = self.create_scope_id();
        let parent = self.scopes.last().map(|s| s.scope_id);
        self.scopes.push(OwnershipScope {
            scope_id,
            variables: HashMap::new(),
            parent,
            is_loop,
            span,
        });
        scope_id
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) -> Option<OwnershipScope<'hir>> {
        self.scopes.pop()
    }

    /// Get the current scope ID
    pub fn current_scope_id(&self) -> ScopeId {
        self.scopes.last().map(|s| s.scope_id).unwrap_or(ScopeId(0))
    }

    /// Check if we're currently inside a loop
    pub fn is_in_loop(&self) -> bool {
        self.scopes.iter().any(|s| s.is_loop)
    }

    /// Get the innermost loop span if we're in a loop
    pub fn get_innermost_loop_span(&self) -> Option<Span> {
        self.scopes.iter().rev().find(|s| s.is_loop).map(|s| s.span)
    }

    /// Declare a new variable
    pub fn declare_variable(&mut self, var: OwnershipVariable<'hir>) -> VarId {
        let var_id = var.var_id;
        let name = var.name;
        self.all_variables.insert(var_id, var);
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.insert(name, var_id);
        }
        var_id
    }

    /// Look up a variable by name in the current scope chain
    pub fn lookup_variable(&self, name: &str) -> Option<&OwnershipVariable<'hir>> {
        // Search from innermost scope outward
        for scope in self.scopes.iter().rev() {
            if let Some(var_id) = scope.variables.get(name) {
                return self.all_variables.get(var_id);
            }
        }
        None
    }

    /// Look up a variable ID by name in the current scope chain
    pub fn lookup_variable_id(&self, name: &str) -> Option<VarId> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_id) = scope.variables.get(name) {
                return Some(*var_id);
            }
        }
        None
    }

    /// Get a mutable reference to a variable
    pub fn get_variable_mut(&mut self, var_id: VarId) -> Option<&mut OwnershipVariable<'hir>> {
        self.all_variables.get_mut(&var_id)
    }

    /// Get a variable
    pub fn get_variable(&self, var_id: VarId) -> Option<&OwnershipVariable<'hir>> {
        self.all_variables.get(&var_id)
    }

    /// Mark a variable as moved
    pub fn mark_moved(&mut self, var_id: VarId, span: Span) {
        if let Some(var) = self.all_variables.get_mut(&var_id) {
            var.status = VarStatus::Moved { move_span: span };
        }
    }

    /// Mark a variable as conditionally moved
    pub fn mark_conditionally_moved(&mut self, var_id: VarId, span: Span) {
        if let Some(var) = self.all_variables.get_mut(&var_id) {
            var.status = VarStatus::ConditionallyMoved { move_span: span };
        }
    }

    /// Mark a variable as deleted
    pub fn mark_deleted(&mut self, var_id: VarId, span: Span) {
        if let Some(var) = self.all_variables.get_mut(&var_id) {
            var.status = VarStatus::Deleted { delete_span: span };
        }
    }

    /// Check if a variable is in a valid (owned) state
    pub fn is_valid(&self, var_id: VarId) -> bool {
        if let Some(var) = self.all_variables.get(&var_id) {
            matches!(var.status, VarStatus::Owned | VarStatus::Borrowed { .. })
        } else {
            false
        }
    }

    /// Get all live (owned) variables in the current scope and parent scopes
    /// Returns them in declaration order (for proper cleanup)
    pub fn get_live_variables(&self) -> Vec<VarId> {
        let mut result = Vec::new();
        for scope in &self.scopes {
            for var_id in scope.variables.values() {
                if let Some(var) = self.all_variables.get(var_id) {
                    if matches!(var.status, VarStatus::Owned)
                        && matches!(var.category, TypeCategory::Resource)
                    {
                        result.push(*var_id);
                    }
                }
            }
        }
        result
    }

    /// Get variables declared in the current scope only
    pub fn get_current_scope_variables(&self) -> Vec<VarId> {
        if let Some(scope) = self.scopes.last() {
            scope.variables.values().copied().collect()
        } else {
            Vec::new()
        }
    }

    /// Record a borrow
    pub fn record_borrow(&mut self, borrowed_var: VarId, borrow_info: BorrowInfo) {
        self.active_borrows
            .entry(borrowed_var)
            .or_default()
            .push(borrow_info);
    }

    /// Release borrows by a specific reference variable
    pub fn release_borrow(&mut self, ref_var: VarId) {
        for borrows in self.active_borrows.values_mut() {
            borrows.retain(|b| b.ref_var != ref_var);
        }
    }

    /// Check if a variable is currently borrowed
    pub fn is_borrowed(&self, var_id: VarId) -> Option<&[BorrowInfo]> {
        self.active_borrows.get(&var_id).map(|v| v.as_slice())
    }

    /// Track reference origin
    pub fn add_reference_origin(&mut self, ref_var: VarId, origin_var: VarId) {
        self.reference_origins
            .entry(ref_var)
            .or_default()
            .push(origin_var);
    }

    /// Get reference origins
    pub fn get_reference_origins(&self, ref_var: VarId) -> Option<&[VarId]> {
        self.reference_origins.get(&ref_var).map(|v| v.as_slice())
    }

    /// Clone the current variable states (for branch analysis)
    pub fn clone_var_states(&self) -> HashMap<VarId, VarStatus> {
        self.all_variables
            .iter()
            .map(|(id, var)| (*id, var.status.clone()))
            .collect()
    }

    /// Restore variable states
    pub fn restore_var_states(&mut self, states: HashMap<VarId, VarStatus>) {
        for (var_id, status) in states {
            if let Some(var) = self.all_variables.get_mut(&var_id) {
                var.status = status;
            }
        }
    }
}
