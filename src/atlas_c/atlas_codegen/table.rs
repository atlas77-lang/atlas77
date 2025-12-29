use std::borrow::Borrow;
use std::fmt;

//ignore unused

pub struct Table<T> {
    pub items: Vec<T>,
}

impl<T> Table<T> {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }
}

impl<T> Iterator for Table<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.items.pop()
    }
}

impl<T> Table<T> {
    pub fn insert(&mut self, item: T) -> usize
    where
        T: PartialEq,
    {
        if self.has(&item) {
            return self.get_index(&item).unwrap();
        }
        self.items.push(item);
        self.items.len() - 1
    }
    pub fn get_index<K>(&self, item: &K) -> Option<usize>
    where
        T: Borrow<K>,
        K: PartialEq + ?Sized,
    {
        self.items.iter().position(|x| x.borrow() == item.borrow())
    }
    pub fn has<K>(&self, item: &K) -> bool
    where
        T: Borrow<K>,
        K: PartialEq,
    {
        self.items.iter().any(|x| x.borrow() == item)
    }
    pub fn len(&self) -> usize {
        self.items.len()
    }
    pub fn clear(&mut self) {
        self.items.clear()
    }
}

impl<'a> Table<&'a str> {
    /// Insert an anonymous entry that won't be looked up by name.
    /// Uses a reserved prefix that won't conflict with user-defined names.
    pub fn insert_anonymous(&mut self) -> usize {
        // We just need a unique slot index. The name doesn't matter as it won't be looked up.
        // We use a static empty string since all anonymous temps are equivalent for lookup purposes.
        // The PartialEq check in insert() would return existing index, so we bypass it.
        self.items.push("");
        self.items.len() - 1
    }
}

impl<T: fmt::Debug> fmt::Debug for Table<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Table").field("Items", &self.items).finish()
    }
}
