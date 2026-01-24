use libc::{c_char, c_int, c_void};

#[repr(C)]
pub struct TCCState {
    _private: [u8; 0],
}

#[repr(u32)]
pub enum OutputType {
    Memory = 1,
    Exe = 2,
    Dll = 3,
    Obj = 4,
    Preprocess = 5,
}

impl Into<c_int> for OutputType {
    fn into(self) -> c_int {
        self as c_int
    }
}

unsafe extern "C" {
    /// Create a new TCC state
    pub fn tcc_new() -> *mut TCCState;
    /// Delete a TCC state
    pub fn tcc_delete(s: *mut TCCState);
    /// Set the output type (exe, dll, obj, memory, preprocess)
    pub fn tcc_set_output_type(s: *mut TCCState, output_type: c_int) -> c_int;
    /// Compile code from a string
    pub fn tcc_compile_string(s: *mut TCCState, code: *const c_char) -> c_int;
    /// Relocate the code to a given memory location
    pub fn tcc_relocate(s: *mut TCCState, ptr: *mut c_void) -> c_int;
    /// Get a symbol from the compiled code
    pub fn tcc_get_symbol(s: *mut TCCState, name: *const c_char) -> *mut c_void;
    /// Add an include path for header files
    pub fn tcc_add_include_path(s: *mut TCCState, path: *const c_char) -> c_int;
    /// Add a library path for libtcc1.a and other libs
    pub fn tcc_add_library_path(s: *mut TCCState, path: *const c_char) -> c_int;
    /// Set the output file name
    pub fn tcc_output_file(s: *mut TCCState, filename: *const c_char) -> c_int;
}