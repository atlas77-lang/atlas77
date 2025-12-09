use crate::atlas_vm::{
    error::RuntimeResult,
    runtime::{CallBack, vm_state::VMState},
    vm_data::VMData,
};

pub const RESULT_FUNCTIONS: [(&str, CallBack); 1] = [("result_default", result_default)];

/// Returns the default value for the None option
pub fn result_default(_state: VMState) -> RuntimeResult<VMData> {
    Ok(VMData::new_unit())
}
