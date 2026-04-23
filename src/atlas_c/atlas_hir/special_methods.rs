use crate::atlas_c::atlas_frontend::parser::ast::AstFlag;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecialMethodKind {
    Copy,
    Default,
    Hash,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecialMethodReceiver {
    Instance,
    Static,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpecialMethodDescriptor {
    pub kind: SpecialMethodKind,
    pub name: &'static str,
    pub expected_signature: &'static str,
    pub receiver: SpecialMethodReceiver,
}

pub static SPECIAL_METHOD_REGISTRY: [SpecialMethodDescriptor; 3] = [
    SpecialMethodDescriptor {
        kind: SpecialMethodKind::Copy,
        name: "copy",
        expected_signature: "fun copy(*const this) -> {Self}",
        receiver: SpecialMethodReceiver::Instance,
    },
    SpecialMethodDescriptor {
        kind: SpecialMethodKind::Default,
        name: "default",
        expected_signature: "fun default() -> {Self}",
        receiver: SpecialMethodReceiver::Static,
    },
    SpecialMethodDescriptor {
        kind: SpecialMethodKind::Hash,
        name: "hash",
        expected_signature: "fun hash(*const this) -> uint64",
        receiver: SpecialMethodReceiver::Instance,
    },
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrimitiveSpecialCallDescriptor {
    pub kind: SpecialMethodKind,
    pub receiver: SpecialMethodReceiver,
    pub intrinsic_name: &'static str,
}

pub const INTRINSIC_PRIMITIVE_DEFAULT: &str = "__atlas_primitive_default";
pub const INTRINSIC_PRIMITIVE_COPY: &str = "__atlas_primitive_copy";
pub const INTRINSIC_PRIMITIVE_HASH: &str = "__atlas_primitive_hash";
pub const INTRINSIC_SIZEOF: &str = "size_of";
pub const INTRINSIC_ALIGNOF: &str = "align_of";
pub const INTRINSIC_TYPE_ID: &str = "type_id";
pub const INTRINSIC_TYPE_OF: &str = "type_of";

pub static PRIMITIVE_SPECIAL_CALL_REGISTRY: [PrimitiveSpecialCallDescriptor; 3] = [
    PrimitiveSpecialCallDescriptor {
        kind: SpecialMethodKind::Default,
        receiver: SpecialMethodReceiver::Static,
        intrinsic_name: INTRINSIC_PRIMITIVE_DEFAULT,
    },
    PrimitiveSpecialCallDescriptor {
        kind: SpecialMethodKind::Copy,
        receiver: SpecialMethodReceiver::Instance,
        intrinsic_name: INTRINSIC_PRIMITIVE_COPY,
    },
    PrimitiveSpecialCallDescriptor {
        kind: SpecialMethodKind::Hash,
        receiver: SpecialMethodReceiver::Instance,
        intrinsic_name: INTRINSIC_PRIMITIVE_HASH,
    },
];

pub fn special_method_by_name(name: &str) -> Option<&'static SpecialMethodDescriptor> {
    SPECIAL_METHOD_REGISTRY
        .iter()
        .find(|descriptor| descriptor.name == name)
}

pub fn primitive_special_call_descriptor(
    method_name: &str,
    receiver: SpecialMethodReceiver,
) -> Option<&'static PrimitiveSpecialCallDescriptor> {
    let kind = special_method_by_name(method_name)?.kind;
    PRIMITIVE_SPECIAL_CALL_REGISTRY
        .iter()
        .find(|descriptor| descriptor.kind == kind && descriptor.receiver == receiver)
}

pub fn expected_signature_for_struct(kind: SpecialMethodKind, struct_name: &str) -> String {
    let template = SPECIAL_METHOD_REGISTRY
        .iter()
        .find(|descriptor| descriptor.kind == kind)
        .map(|descriptor| descriptor.expected_signature)
        .unwrap_or("<unknown special method>");
    template.replace("{Self}", struct_name)
}

pub fn special_method_enabled_by_flag(kind: SpecialMethodKind, flag: AstFlag) -> bool {
    match kind {
        SpecialMethodKind::Copy => {
            matches!(flag, AstFlag::Copyable(_) | AstFlag::TriviallyCopyable(_))
        }
        SpecialMethodKind::Default => matches!(flag, AstFlag::Default(_)),
        SpecialMethodKind::Hash => matches!(flag, AstFlag::Hashable(_)),
    }
}
