use crate::atlas_vm::vm_data::VMData;
use std::borrow::Borrow;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Index, IndexMut};

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

#[derive(Debug, Clone)]
pub enum ObjectKind {
    String(String),
    Structure(Structure),
    List(Vec<VMData>),
    Free { next: ObjectIndex },
    // Used for error handling
    Primitive,
}
impl Default for ObjectKind {
    fn default() -> Self {
        ObjectKind::Free {
            next: ObjectIndex::default(),
        }
    }
}

impl Display for ObjectKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ObjectKind::String(s) => write!(f, "`String`: \"{}\"", s),
            ObjectKind::Structure(s) => write!(f, "{:?}", s),
            ObjectKind::List(l) => {
                write!(f, "[")?;
                for element in l.iter() {
                    write!(f, "{}, ", element)?;
                }
                write!(f, "]")
            }
            ObjectKind::Free { next } => write!(f, "Free: next -> {}", next),
            ObjectKind::Primitive => write!(f, "Primitive"),
        }
    }
}

impl ObjectKind {
    pub fn new(data: impl Into<ObjectKind>) -> Self {
        data.into()
    }

    pub fn string(&self) -> Option<&String> {
        match &self {
            ObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn string_mut(&mut self) -> Option<&mut String> {
        match self {
            ObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn structure(&self) -> &Structure {
        match &self {
            ObjectKind::Structure(s) => s,
            _ => unreachable!("Expected a structure, got a {:?}", self),
        }
    }

    pub fn structure_mut(&mut self) -> &mut Structure {
        match self {
            ObjectKind::Structure(s) => s,
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

#[derive(Debug, Clone)]
pub struct Object {
    pub kind: ObjectKind,
}

impl Borrow<ObjectKind> for Object {
    fn borrow(&self) -> &ObjectKind {
        &self.kind
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub fields: RawStructure,
    pub struct_descriptor: usize,
}

impl Display for Structure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        //Let's pretty print the structure:
        write!(f, "struct {{ ")?;
        for (i, field) in self.fields.ptr.iter().enumerate() {
            write!(f, "\n\t{}: {}", i, field)?;
            if i != self.fields.len - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, " }}")
    }
}

#[repr(C)]
pub struct RawStructure {
    //Temporary workaround for raw pointer issues
    pub ptr: Vec<VMData>,
    pub len: usize,
}

impl Clone for RawStructure {
    //TODO: Deep clone
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            len: self.len,
        }
    }
}

impl Debug for RawStructure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.ptr.clone()).finish()
    }
}
/*
impl RawStructure {
    pub unsafe fn as_slice(&self) -> &mut [VMData] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr, self.len)
        }
    }
}*/

impl Structure {
    pub fn new(struct_descriptor: usize, fields: Vec<VMData>, len: usize) -> Self {
        Self {
            struct_descriptor,
            fields: RawStructure { ptr: fields, len },
        }
    }
}
impl Index<usize> for Structure {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.fields[index]
    }
}

impl IndexMut<usize> for Structure {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.fields[index]
    }
}

impl Index<usize> for RawStructure {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.ptr[index]
    }
}

impl IndexMut<usize> for RawStructure {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.ptr[index]
    }
}
