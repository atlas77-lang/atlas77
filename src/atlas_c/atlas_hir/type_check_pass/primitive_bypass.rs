use std::collections::BTreeMap;

use crate::atlas_c::{
    atlas_hir::{
        signature::{HirFlag, HirStructSignature, HirVisibility},
        ty::HirTy,
    },
    utils::Span,
};

/**
 * Due to how shit this compiler is designed, I am just gonna bypass
 * a lot of how it works internally until the refactor and bootstrap
 */

pub(crate) const PRIMITIVE_TYPE_NAMES: &[&str] = &[
    "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "float32", "float64",
    "char", "bool",
];

pub(crate) fn synthetic_primitive_signature(name: &'static str) -> HirStructSignature<'static> {
    HirStructSignature {
        declaration_span: Span::default(),
        vis: HirVisibility::Public,
        flag: HirFlag::None,
        name,
        pre_mangled_ty: None,
        name_span: Span::default(),
        methods: BTreeMap::new(),
        fields: BTreeMap::new(),
        generics: Vec::new(),
        operators: BTreeMap::new(),
        constants: BTreeMap::new(),
        destructor: None,
        had_user_defined_destructor: false,
        is_trivially_copyable: true,
        nullable_attribute_span: None,
        is_instantiated: true,
        docstring: None,
        // avoid struct generation during HIR lowering
        is_extern: true,
        // Allow for the lowering to just properly
        //  remap those names to actual C primitives
        //  default to int, thought shouldn't be a worry
        c_name: Some(primitive_to_c_primitive(name).unwrap_or("int")),
        represents_ty: None,
    }
}

pub(crate) fn primitive_type_name(ty: &HirTy) -> Option<&'static str> {
    match ty {
        HirTy::Integer(i) => Some(match i.size_in_bits {
            8 => "int8",
            16 => "int16",
            32 => "int32",
            64 => "int64",
            _ => return None,
        }),
        HirTy::UnsignedInteger(i) => Some(match i.size_in_bits {
            8 => "uint8",
            16 => "uint16",
            32 => "uint32",
            64 => "uint64",
            _ => return None,
        }),
        HirTy::Float(f) => Some(match f.size_in_bits {
            32 => "float32",
            64 => "float64",
            _ => return None,
        }),
        HirTy::Char(_) => Some("char"),
        HirTy::Boolean(_) => Some("bool"),
        _ => None,
    }
}

pub(crate) fn primitive_to_c_primitive(name: &str) -> Option<&'static str> {
    match name {
        "int8" => Some("int8_t"),
        "int16" => Some("int16_t"),
        "int32" => Some("int32_t"),
        "int64" => Some("int64_t"),
        "uint8" => Some("uint8_t"),
        "uint16" => Some("uint16_t"),
        "uint32" => Some("uint32_t"),
        "uint64" => Some("uint64_t"),
        "float32" => Some("float"),
        "float64" => Some("double"),
        "char" => Some("uint32_t"),
        "bool" => Some("bool"),
        _ => None,
    }
}
