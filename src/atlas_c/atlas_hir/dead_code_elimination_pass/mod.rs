use std::collections::HashMap;

use crate::atlas_c::atlas_hir::{HirModule, arena::HirArena, error::HirResult};

pub struct DeadCodeStruct {
    pub name: String,
    pub is_used: bool,
    pub methods: HashMap<String, DeadCodeFunction>,
}

pub struct DeadCodeFunction {
    pub name: String,
    pub is_used: bool,
}

pub struct DeadCodeEliminationPass<'hir> {
    _hir_arena: &'hir HirArena<'hir>,
    _struct_map: HashMap<&'hir str, DeadCodeStruct>,
    _function_map: HashMap<&'hir str, DeadCodeFunction>,
}

impl<'hir> DeadCodeEliminationPass<'hir> {
    pub fn new(hir_arena: &'hir HirArena<'hir>) -> Self {
        Self {
            _hir_arena: hir_arena,
            _struct_map: HashMap::new(),
            _function_map: HashMap::new(),
        }
    }

    pub fn eliminate_dead_code(
        &mut self,
        hir_module: &'hir mut HirModule<'hir>,
    ) -> HirResult<&'hir mut HirModule<'hir>> {
        //1. Build the call graph
        //2. Mark all reachable functions and structs starting from the entry points (e.g., main function)
        //3. Remove unmarked functions and structs from the HIR module

        //Note: Implementation of these steps is pending
        return Ok(hir_module);
    }
}
