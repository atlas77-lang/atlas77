use crate::atlas_c::atlas_hir::HirModule;

/// LifetimePass inserts destructor calls for values whose lifetimes end at
/// the closing of their lexical scope.
///
/// Atlas currently allocates all constructed values on the heap. The
/// distinction between *long-lived* and *short-lived* values is therefore
/// purely a compiler concept:
///   - `new T(...)` produces a heap allocation the compiler does **not**
///     auto-destroy (user-managed lifetime).
///   - `T(...)` produces a heap allocation the compiler **will** destroy
///     at end of scope unless ownership has been moved.
///
/// The pass tracks the introduction of short-lived values, tracks ownership
/// moves, and emits `delete <var>` for every still-owned short-lived value
/// when its scope ends.
///
/// Stack allocation is not yet implemented; when added, this same pass will
/// determine whether a value is stack-allocated or heap-allocated.

pub struct LifeTimePass<'hir> {
    pub hir: &'hir mut HirModule<'hir>,
}
