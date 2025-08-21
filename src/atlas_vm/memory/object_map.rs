use crate::atlas_vm::RuntimeResult;
use crate::atlas_vm::errors::RuntimeError;
use crate::atlas_vm::memory::vm_data::{VMData, VMTag};
use std::borrow::Borrow;
use std::ops::{Index, IndexMut};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Probably should be renamed lmao
///
/// Need to find a way to make the memory shrink, grows, and garbage collect unused memory (by scanning the stack & VarMap)
pub struct Memory<'mem> {
    mem: Vec<Object<'mem>>,
    pub free: ObjectIndex,
    pub used_space: usize,
}

#[repr(transparent)]
#[derive(Clone, Copy, Default, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct ObjectIndex {
    pub idx: usize,
}

impl From<ObjectIndex> for usize {
    fn from(value: ObjectIndex) -> Self {
        value.idx
    }
}

impl ObjectIndex {
    pub const fn new(i: usize) -> ObjectIndex {
        ObjectIndex { idx: i }
    }
}
impl Display for ObjectIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[@{}]", self.idx)
    }
}

impl<'mem> Memory<'mem> {
    pub fn new(space: usize) -> Self {
        Self {
            free: ObjectIndex::new(0),
            mem: (0..space)
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
        for (idx, obj) in self.mem.iter_mut().enumerate() {
            obj.kind = ObjectKind::Free { next: self.free };
            obj.rc = 0;
            self.free = ObjectIndex::new(idx);
        }
    }

    pub fn put(&mut self, object: ObjectKind<'mem>) -> Result<ObjectIndex, RuntimeError> {
        let idx = self.free;
        let v = self.mem.get_mut(usize::from(self.free)).unwrap();
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
        let v = &self.mem.get_mut(usize::from(index)).unwrap().kind;
        println!("Freeing: {}", v);
        let mut obj_to_dec = vec![];
        //todo: Support classes
        match v {
            ObjectKind::Class(Class { fields, .. }) => {
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
        for obj in obj_to_dec {
            self.rc_dec(obj)?;
        }
        let v = self.mem.get_mut(usize::from(index)).unwrap();
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
        let obj = self.mem[usize::from(index)].kind.clone();
        self.rc_dec(index)?;
        Ok(obj)
    }

    #[inline(always)]
    pub fn get_mut(&mut self, index: ObjectIndex) -> RuntimeResult<&mut ObjectKind<'mem>> {
        //You can decrement the rc here, because if it reaches 0 and still need to return a mutable reference...
        self.rc_dec(index)?;
        let kind = &mut self.mem[usize::from(index)].kind;
        Ok(kind)
    }

    #[inline(always)]
    pub fn rc_inc(&mut self, index: ObjectIndex) {
        self.mem[usize::from(index)].rc += 1;
    }

    #[inline(always)]
    pub fn rc_dec(&mut self, index: ObjectIndex) -> RuntimeResult<()> {
        let rc = &mut self.mem[usize::from(index)].rc;
        *rc -= 1;
        if *rc == 0 {
            self.free(index)?;
        }
        Ok(())
    }

    #[inline(always)]
    pub fn raw(&self) -> &[Object<'mem>] {
        &self.mem
    }

    #[inline(always)]
    pub fn raw_mut(&mut self) -> &mut [Object<'mem>] {
        &mut self.mem
    }
}

impl Display for Memory<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, obj) in self.mem.iter().enumerate() {
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

#[derive(Debug, Clone)]
pub enum ObjectKind<'mem> {
    String(String),
    Class(Class<'mem>),
    List(Vec<VMData>),
    Free { next: ObjectIndex },
}
impl Default for ObjectKind<'_> {
    fn default() -> Self {
        ObjectKind::Free {
            next: ObjectIndex::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Object<'mem> {
    pub kind: ObjectKind<'mem>,
    /// Reference count
    pub rc: usize,
}
impl<'mem> Borrow<ObjectKind<'mem>> for Object<'mem> {
    fn borrow(&self) -> &ObjectKind<'mem> {
        &self.kind
    }
}
impl Display for Object<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} (rc: {})", self.kind, self.rc)
    }
}

impl Display for ObjectKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ObjectKind::String(s) => write!(f, "`String`: \"{}\"", s),
            ObjectKind::Class(s) => write!(f, "{:?}", s),
            ObjectKind::List(l) => write!(f, "{:?}", l),
            ObjectKind::Free { next } => write!(f, "Free: next -> {}", next),
        }
    }
}

impl<'mem> ObjectKind<'mem> {
    pub fn new(data: impl Into<ObjectKind<'mem>>) -> Self {
        data.into()
    }

    pub fn string(&self) -> &String {
        match &self {
            ObjectKind::String(s) => s,
            _ => unreachable!("Expected a string, got a {:?}", self),
        }
    }

    pub fn string_mut(&mut self) -> &mut String {
        match self {
            ObjectKind::String(s) => s,
            _ => unreachable!("Expected a string, got a {:?}", self),
        }
    }

    pub fn class(&self) -> &Class<'mem> {
        match &self {
            ObjectKind::Class(s) => s,
            _ => unreachable!("Expected a structure, got a {:?}", self),
        }
    }

    pub fn class_mut(&mut self) -> &mut Class<'mem> {
        match self {
            ObjectKind::Class(s) => s,
            _ => unreachable!("Expected a structure, got a {:?}", self),
        }
    }

    pub fn list(&self) -> &Vec<VMData> {
        match &self {
            ObjectKind::List(l) => l,
            _ => unreachable!("Expected a list, got a {:?}", self),
        }
    }

    pub fn list_mut(&mut self) -> &mut Vec<VMData> {
        match self {
            ObjectKind::List(l) => l,
            _ => unreachable!("Expected a list, got a {:?}", self),
        }
    }
}

impl<'mem> From<Class<'mem>> for ObjectKind<'mem> {
    fn from(value: Class<'mem>) -> Self {
        ObjectKind::Class(value)
    }
}

impl From<String> for ObjectKind<'_> {
    fn from(value: String) -> Self {
        ObjectKind::String(value)
    }
}

impl<'mem> From<Vec<VMData>> for ObjectKind<'mem> {
    fn from(value: Vec<VMData>) -> Self {
        ObjectKind::List(value)
    }
}

#[derive(Debug, Clone)]
pub struct Class<'mem> {
    pub fields: RawClass<'mem>,
    pub class_descriptor: usize,
}

#[repr(C)]
#[derive(Debug)]
pub struct RawClass<'mem> {
    pub ptr: &'mem mut [VMData],
}

impl<'mem> Clone for RawClass<'mem> {
    fn clone(&self) -> Self {
        let ptr = Box::leak(Box::new(self.ptr.to_vec())).as_mut_slice();
        RawClass { ptr }
    }
}

impl<'mem> Class<'mem> {
    pub fn new(class_descriptor: usize, fields: &'mem mut [VMData]) -> Self {
        Self {
            class_descriptor,
            fields: RawClass { ptr: fields },
        }
    }
}
impl<'mem> Index<usize> for Class<'mem> {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.fields[index]
    }
}

impl IndexMut<usize> for Class<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.fields[index]
    }
}

impl<'mem> Index<usize> for RawClass<'mem> {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.ptr[index]
    }
}

impl<'mem> IndexMut<usize> for RawClass<'mem> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.ptr[index]
    }
}
