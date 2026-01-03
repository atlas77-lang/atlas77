// All of this is assuming that Time is of type: Time(sec: 64, nsec: i64)

use crate::atlas_vm::error::RuntimeError;
use crate::atlas_vm::object::{ObjectKind, Structure};
use crate::atlas_vm::runtime::CallBack;
use crate::atlas_vm::runtime::vm_state::VMState;
use crate::atlas_vm::vm_data::{VMData, VMTag};
use time::{OffsetDateTime, format_description};

pub const TIME_FUNCTIONS: [(&str, CallBack); 4] = [
    ("now", now),
    ("format_time_iso", format_time_iso),
    ("format_time", format_time),
    ("sleep", sleep),
];

//now() -> Time
pub fn now(state: VMState) -> Result<VMData, RuntimeError> {
    let time = std::time::SystemTime::now();
    let duration = time.duration_since(std::time::UNIX_EPOCH).unwrap();

    let sec = duration.as_secs();
    let nsec = duration.subsec_nanos();

    let fields = vec![VMData::new_i64(sec as i64), VMData::new_i64(nsec as i64)];

    let obj_idx = state
        .object_map
        .put(ObjectKind::Structure(Structure::new(256, fields, 2)));
    match obj_idx {
        Ok(index) => Ok(VMData::new_object(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

//format_time_iso(time: Time) -> string
pub fn format_time_iso(state: VMState) -> Result<VMData, RuntimeError> {
    let time_ptr = state.stack.pop()?.as_object();
    let raw_time_obj = state.object_map.get(time_ptr)?;
    let time_obj = raw_time_obj.structure();

    let sec = time_obj[0].as_i64();
    let nsec = time_obj[1].as_i64();

    let time =
        OffsetDateTime::from_unix_timestamp(sec).unwrap() + time::Duration::nanoseconds(nsec);

    let fmt =
        format_description::parse("[year]-[month]-[day]T[hour]:[minute]:[second].[frac][offset]")
            .unwrap();
    let formatted = time.format(&fmt).unwrap();

    let obj_idx = state.object_map.put(ObjectKind::String(formatted));
    state.object_map.free(time_ptr)?;
    match obj_idx {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

//format_time(time: Time, format: string) -> string
pub fn format_time(state: VMState) -> Result<VMData, RuntimeError> {
    let format_ptr = state.stack.pop()?.as_object(); // a string is an object
    let time_ptr = state.stack.pop()?.as_object();
    let obj_kind = state.object_map.get(format_ptr)?;
    let fmt_str = if let Some(s) = obj_kind.string() {
        &s.clone()
    } else {
        return Err(RuntimeError::InvalidObjectAccess(VMTag::String, obj_kind));
    };
    let raw_time_obj = state.object_map.get(time_ptr)?;
    let time_obj = raw_time_obj.structure();

    let sec = time_obj[0].as_i64();
    let nsec = time_obj[1].as_i64();

    let time =
        OffsetDateTime::from_unix_timestamp(sec).unwrap() + time::Duration::nanoseconds(nsec);

    let fmt = format_description::parse(fmt_str).unwrap();
    let formatted = time.format(&fmt).unwrap();

    let obj_idx = state.object_map.put(ObjectKind::String(formatted));
    state.object_map.free(time_ptr)?;
    state.object_map.free(format_ptr)?;
    match obj_idx {
        Ok(index) => Ok(VMData::new_string(index)),
        Err(_) => Err(RuntimeError::OutOfMemory),
    }
}

fn sleep(state: VMState) -> Result<VMData, RuntimeError> {
    let time_ptr = state.stack.pop()?.as_object();
    let raw_time_obj = state.object_map.get(time_ptr)?;
    let time_obj = raw_time_obj.structure();

    let sec = time_obj[0].as_i64();
    let nsec = time_obj[1].as_i64();

    let duration = std::time::Duration::new(sec as u64, nsec as u32);
    std::thread::sleep(duration);

    state.object_map.free(time_ptr)?;
    Ok(VMData::new_unit())
}
