use crate::atlas_vm::RuntimeResult;
use crate::atlas_vm::errors::RuntimeError;
use crate::atlas_vm_new::object::{Object, ObjectIndex, ObjectKind, Structure};
use crate::atlas_vm_new::vm_data::VMTag;
use std::fmt;
use std::fmt::{Display, Formatter};

pub struct Heap<'heap> {
    memory: Vec<Object<'heap>>,
    pub free: ObjectIndex,
    pub used_space: usize,
}

impl<'mem> Heap<'mem> {
    pub fn new(space: usize) -> Self {
        Self {
            free: ObjectIndex::new(0),
            memory: (0..space)
                .map(|x| Object {
                    kind: ObjectKind::Free {
                        next: ObjectIndex::new(((x + 1) % space)),
                    },
                    rc: 0,
                })
                .collect(),
            used_space: 0,
        }
    }
    pub fn clear(&mut self) {
        for (idx, obj) in self.memory.iter_mut().enumerate() {
            obj.kind = ObjectKind::Free { next: self.free };
            obj.rc = 0;
            self.free = ObjectIndex::new(idx);
        }
    }

    pub fn put(&mut self, object: ObjectKind<'mem>) -> Result<ObjectIndex, RuntimeError> {
        let idx = self.free;
        let v = self.memory.get_mut(usize::from(self.free)).unwrap();
        let repl = std::mem::replace(
            v,
            Object {
                kind: object,
                rc: 1000,
            },
        );

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
        let v = &self.memory.get_mut(usize::from(index)).unwrap().kind;
        println!("Freeing: {}", v);
        let mut obj_to_dec = vec![];
        //todo: Support classes
        match v {
            ObjectKind::Structure(Structure { fields, .. }) => {
                for field in fields.ptr.iter() {
                    match field.tag {
                        VMTag::Str | VMTag::List | VMTag::Object => {
                            obj_to_dec.push(field.as_object());
                        }
                        _ => {}
                    }
                }
            }
            ObjectKind::List(list) => {
                for item in list {
                    match item.tag {
                        VMTag::Str | VMTag::List | VMTag::Object => {
                            obj_to_dec.push(item.as_object());
                        }
                        _ => {}
                    }
                }
            }
            ObjectKind::Free { .. } => {
                // Already freed
                return Ok(());
            }
            _ => {}
        }
        for obj_index in obj_to_dec {
            self.rc_dec(obj_index)?;
        }
        let v = self.memory.get_mut(usize::from(index)).unwrap();
        let repl = std::mem::replace(
            v,
            Object {
                kind: ObjectKind::Free { next },
                rc: 0,
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
    pub fn get(&mut self, index: ObjectIndex) -> RuntimeResult<ObjectKind<'mem>> {
        let obj = self.memory[usize::from(index)].kind.clone();
        self.rc_dec(index)?;
        Ok(obj)
    }

    #[inline(always)]
    pub fn get_mut(&mut self, index: ObjectIndex) -> RuntimeResult<&mut ObjectKind<'mem>> {
        //You can decrement the rc here, because if it reaches 0 and still need to return a mutable reference...
        self.rc_dec(index)?;
        let kind = &mut self.memory[usize::from(index)].kind;
        Ok(kind)
    }

    #[inline(always)]
    pub fn rc_inc(&mut self, index: ObjectIndex) {
        self.memory[usize::from(index)].rc += 1;
    }

    #[inline(always)]
    pub fn rc_dec(&mut self, index: ObjectIndex) -> RuntimeResult<()> {
        let rc = &mut self.memory[usize::from(index)].rc;
        *rc -= 1;
        if *rc == 0 {
            self.free(index)?;
        }
        Ok(())
    }

    #[inline(always)]
    pub fn raw(&self) -> &[Object<'mem>] {
        &self.memory
    }

    #[inline(always)]
    pub fn raw_mut(&mut self) -> &mut [Object<'mem>] {
        &mut self.memory
    }
}

impl Display for Heap<'_> {
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
