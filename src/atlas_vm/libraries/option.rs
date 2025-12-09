use crate::atlas_vm::{
    error::RuntimeResult,
    runtime::{CallBack, vm_state::VMState},
    vm_data::VMData,
};

pub const OPTION_FUNCTIONS: [(&str, CallBack); 1] = [("option_default", option_default)];

/// Returns the default value for the None option
pub fn option_default(_state: VMState) -> RuntimeResult<VMData> {
    Ok(VMData::new_unit())
}
