use bumpalo::Bump;

pub struct RuntimeArena<'arena> {
    pub allocator: &'arena Bump,
}

impl<'arena> RuntimeArena<'arena> {
    pub const fn new(allocator: &'arena Bump) -> Self {
        RuntimeArena { allocator }
    }
    pub fn alloc<T>(&self, value: T) -> &'arena mut T {
        self.allocator.alloc(value)
    }
}
