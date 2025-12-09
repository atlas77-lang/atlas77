use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::object::{Object, ObjectIndex, ObjectKind};
use std::fmt;
use std::fmt::{Display, Formatter};

pub const HEAP_DEFAULT_SIZE: usize = 8192; //In number of objects

pub struct Heap {
    memory: Vec<Object>,
    pub free: ObjectIndex,
    pub used_space: usize,
}

impl Heap {
    pub fn new(space: usize) -> Self {
        Self {
            free: ObjectIndex::new(0),
            memory: (0..space)
                .map(|x| Object {
                    kind: ObjectKind::Free {
                        next: ObjectIndex::new((x + 1) % space),
                    },
                })
                .collect(),
            used_space: 0,
        }
    }
    pub fn clear(&mut self) {
        for (idx, obj) in self.memory.iter_mut().enumerate() {
            obj.kind = ObjectKind::Free { next: self.free };
            self.free = ObjectIndex::new(idx);
        }
    }

    pub fn put(&mut self, object: ObjectKind) -> Result<ObjectIndex, RuntimeError> {
        //println!("Allocating object: {:?}", object);
        let idx = self.free;
        let v = self.memory.get_mut(usize::from(self.free)).unwrap();
        let repl = std::mem::replace(v, Object { kind: object });

        match repl {
            Object {
                kind: ObjectKind::Free { next },
                ..
            } => {
                self.free = next;
                Ok(idx)
            }
            _ => Err(RuntimeError::OutOfMemory),
        }
    }

    pub fn free(&mut self, index: ObjectIndex) -> RuntimeResult<()> {
        let next = self.free;
        let v = self.memory.get_mut(usize::from(index)).unwrap();

        let repl = std::mem::replace(
            v,
            Object {
                kind: ObjectKind::Free { next },
            },
        );
        let res = match repl {
            Object {
                kind: ObjectKind::Free { .. },
                ..
            } => Err(RuntimeError::NullReference),
            _ => Ok(()),
        };
        self.free = index;
        res
    }

    #[inline(always)]
    pub fn get(&mut self, index: ObjectIndex) -> RuntimeResult<ObjectKind> {
        let obj = self.memory[usize::from(index)].kind.clone();
        Ok(obj)
    }

    #[inline(always)]
    pub fn get_mut(&mut self, index: ObjectIndex) -> RuntimeResult<&mut ObjectKind> {
        let kind = &mut self.memory[usize::from(index)].kind;
        Ok(kind)
    }

    #[inline(always)]
    pub fn raw(&self) -> &[Object] {
        &self.memory
    }

    #[inline(always)]
    pub fn raw_mut(&mut self) -> &mut [Object] {
        &mut self.memory
    }
}

impl Display for Heap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, obj) in self.memory.iter().enumerate() {
            if let Object {
                kind: ObjectKind::Free { .. },
                ..
            } = obj
            {
                continue;
            }
            writeln!(f, "\t{}: {}", i, obj)?;
        }
        Ok(())
    }
}
