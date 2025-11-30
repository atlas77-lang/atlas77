use crate::atlas_vm::vm_data::VMData;
use std::borrow::Borrow;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

pub type ObjectDescriptorId = u64;

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
pub enum ObjectKind<'heap> {
    String(String),
    Structure(Structure<'heap>),
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

impl Display for ObjectKind<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ObjectKind::String(s) => write!(f, "`String`: \"{}\"", s),
            ObjectKind::Structure(s) => write!(f, "{:?}", s),
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

    pub fn structure(&self) -> &Structure<'mem> {
        match &self {
            ObjectKind::Structure(s) => s,
            _ => unreachable!("Expected a structure, got a {:?}", self),
        }
    }

    pub fn structure_mut(&mut self) -> &mut Structure<'mem> {
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
pub struct Object<'heap> {
    pub kind: ObjectKind<'heap>,
    pub rc: usize,
}

impl<'heap> Borrow<ObjectKind<'heap>> for Object<'heap> {
    fn borrow(&self) -> &ObjectKind<'heap> {
        &self.kind
    }
}

impl Display for Object<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} (rc: {})", self.kind, self.rc)
    }
}

#[derive(Debug, Clone)]
pub struct Structure<'heap> {
    pub fields: RawStructure<'heap>,
    pub struct_descriptor: usize,
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct RawStructure<'heap> {
    pub ptr: &'heap mut [VMData],
}

impl<'mem> Clone for RawStructure<'mem> {
    fn clone(&self) -> Self {
        let ptr = Box::leak(Box::new(self.ptr.to_vec())).as_mut_slice();
        RawStructure { ptr }
    }
}

impl<'mem> Structure<'mem> {
    pub fn new(struct_descriptor: usize, fields: &'mem mut [VMData]) -> Self {
        Self {
            struct_descriptor,
            fields: RawStructure { ptr: fields },
        }
    }
}
impl<'mem> Index<usize> for Structure<'mem> {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.fields[index]
    }
}

impl IndexMut<usize> for Structure<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.fields[index]
    }
}

impl<'mem> Index<usize> for RawStructure<'mem> {
    type Output = VMData;

    fn index(&self, index: usize) -> &Self::Output {
        &self.ptr[index]
    }
}

impl<'mem> IndexMut<usize> for RawStructure<'mem> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.ptr[index]
    }
}
