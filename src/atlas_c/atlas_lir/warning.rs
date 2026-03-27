// For some reason I get unused assignment warnings in this file
#![allow(unused_assignments)]

use crate::declare_error_type;

declare_error_type! {
    #[error("lir_warning: {0}")]
    pub enum LirLoweringWarning {}
}
