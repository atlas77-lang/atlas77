use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    marker::PhantomData,
    rc::Rc,
};

use super::ty::{HirBooleanTy, HirCharTy, HirConstTy, HirFloatTy, HirGenericTy, HirIntegerTy, HirListTy, HirNamedTy, HirNullTy, HirNullableTy, HirStringTy, HirTy, HirTyId, HirUninitializedTy, HirUnitTy, HirUnsignedIntTy};
use bumpalo::Bump;
use logos::Span;

//todo: Implement my own Arenas (maybe)
pub struct HirArena<'arena> {
    allocator: Rc<Bump>,
    type_arena: TypeArena<'arena>,
    name_arena: HirNameArena<'arena>,
    phantom: PhantomData<&'arena ()>,
}

impl Default for HirArena<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'arena> HirArena<'arena> {
    pub fn new() -> Self {
        let allocator = Rc::new(Bump::new());
        Self {
            type_arena: TypeArena::new(allocator.clone()),
            name_arena: HirNameArena::new(allocator.clone()),
            allocator,
            phantom: PhantomData,
        }
    }

    pub fn intern<T>(&'arena self, v: T) -> &'arena mut T {
        self.allocator.alloc(v)
    }

    pub fn names(&'arena self) -> &'arena HirNameArena<'arena> {
        &self.name_arena
    }

    pub fn types(&'arena self) -> &'arena TypeArena<'arena> {
        &self.type_arena
    }
}

pub struct HirNameArena<'arena> {
    allocator: Rc<Bump>,
    intern: RefCell<HashSet<&'arena str>>,
}

impl<'arena> HirNameArena<'arena> {
    pub fn new(allocator: Rc<Bump>) -> Self {
        Self {
            allocator,
            intern: RefCell::new(HashSet::new()),
        }
    }

    pub fn get(&'arena self, name: &str) -> &'arena str {
        if let Some(interned) = self.intern.borrow().get(name) {
            return interned;
        }
        let id = self.allocator.alloc_str(name);
        self.intern.borrow_mut().insert(id);
        id
    }
}

pub struct TypeArena<'arena> {
    allocator: Rc<Bump>,
    intern: RefCell<HashMap<HirTyId, &'arena HirTy<'arena>>>,
}

impl<'arena> TypeArena<'arena> {
    pub fn new(allocator: Rc<Bump>) -> Self {
        Self {
            allocator,
            intern: RefCell::new(HashMap::new()),
        }
    }

    pub fn _get_type(&'arena self, id: HirTyId) -> Option<&'arena HirTy<'arena>> {
        self.intern.borrow().get(&id).copied()
    }

    pub fn get_none_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_null_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Null(HirNullTy {})))
    }

    pub fn get_integer64_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_integer64_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Int64(HirIntegerTy {})))
    }

    pub fn get_float64_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_float64_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Float64(HirFloatTy {})))
    }

    pub fn get_uint64_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_uint64_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::UInt64(HirUnsignedIntTy {})))
    }

    pub fn get_char_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_char_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Char(HirCharTy {})))
    }

    pub fn get_boolean_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_boolean_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Boolean(HirBooleanTy {})))
    }

    pub fn get_str_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_str_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::String(HirStringTy {})))
    }

    pub fn get_nullable_ty(&'arena self, inner: &'arena HirTy<'arena>) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_nullable_ty_id(&HirTyId::from(inner));
        self.intern.borrow_mut().entry(id).or_insert_with(|| {
            self.allocator
                .alloc(HirTy::Nullable(HirNullableTy { inner }))
        })
    }

    pub fn get_readonly_ty(&'arena self, inner: &'arena HirTy<'arena>) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_readonly_ty_id(&HirTyId::from(inner));
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Const(HirConstTy { inner })))
    }

    pub fn get_unit_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_unit_ty_id();
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::Unit(HirUnitTy {})))
    }

    pub fn get_uninitialized_ty(&'arena self) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_uninitialized_ty_id();
        self.intern.borrow_mut().entry(id).or_insert_with(|| {
            self.allocator
                .alloc(HirTy::Uninitialized(HirUninitializedTy {}))
        })
    }

    pub fn get_list_ty(&'arena self, ty: &'arena HirTy<'arena>) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_list_ty_id(&HirTyId::from(ty));
        self.intern
            .borrow_mut()
            .entry(id)
            .or_insert_with(|| self.allocator.alloc(HirTy::List(HirListTy { inner: ty })))
    }

    pub fn get_named_ty(&'arena self, name: &'arena str, span: Span) -> &'arena HirTy<'arena> {
        let id = HirTyId::compute_name_ty_id(name);
        self.intern.borrow_mut().entry(id).or_insert_with(|| {
            self.allocator
                .alloc(HirTy::Named(HirNamedTy { name, span }))
        })
    }

    pub fn get_generic_ty(
        &'arena self,
        name: &'arena str,
        inner: Vec<&'arena HirTy<'arena>>,
    ) -> &'arena HirTy<'arena> {
        // compute stable id from name + inner types
        let param_ids = inner.iter().map(|t| HirTyId::from(*t)).collect::<Vec<_>>();
        let id = HirTyId::compute_generic_ty_id(name, &param_ids);
        self.intern.borrow_mut().entry(id).or_insert_with(|| {
            // clone inner hir types to store inside the owned Vec
            let inner_owned = inner.iter().map(|t| (*t).clone()).collect::<Vec<_>>();
            self.allocator.alloc(HirTy::Generic(HirGenericTy {
                name,
                inner: inner_owned,
            }))
        })
    }
}
