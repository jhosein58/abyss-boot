use include_dir::{Dir, include_dir};
use std::ffi::{CString, c_char, c_int, c_void};
use tempfile::TempDir;

static TCC_MINIMAL_FS: Dir = include_dir!("tcc_minimal");

#[repr(C)]
pub struct TCCState {
    _private: [u8; 0],
}

pub const TCC_OUTPUT_MEMORY: i32 = 1;
pub const TCC_RELOCATE_AUTO: *mut c_void = 1 as *mut c_void;

unsafe extern "C" {
    pub fn tcc_new() -> *mut TCCState;
    pub fn tcc_delete(s: *mut TCCState);
    pub fn tcc_compile_string(s: *mut TCCState, code: *const c_char) -> c_int;
    pub fn tcc_set_output_type(s: *mut TCCState, ty: c_int) -> c_int;
    pub fn tcc_relocate(s: *mut TCCState, ptr: *mut c_void) -> c_int;
    pub fn tcc_get_symbol(s: *mut TCCState, name: *const c_char) -> *mut c_void;
    pub fn tcc_add_include_path(s: *mut TCCState, pathname: *const c_char) -> c_int;
    pub fn tcc_set_lib_path(s: *mut TCCState, pathname: *const c_char) -> c_int;
    pub fn tcc_add_symbol(s: *mut TCCState, name: *const c_char, func_ptr: *const c_void) -> c_int;
    pub fn tcc_set_options(s: *mut TCCState, options: *const c_char) -> c_int;
}

pub struct AbyssJit {
    state: *mut TCCState,
    _temp_dir: TempDir,
    relocated: bool,
}

impl AbyssJit {
    pub fn new() -> Result<Self, String> {
        unsafe {
            let temp_dir =
                TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
            let root_path = temp_dir.path();

            TCC_MINIMAL_FS
                .extract(root_path)
                .map_err(|e| format!("Failed to extract minimal resources: {}", e))?;

            let c_root_path = CString::new(root_path.to_str().unwrap()).unwrap();
            let include_path = root_path.join("include");
            let c_include_path = CString::new(include_path.to_str().unwrap()).unwrap();

            let state = tcc_new();
            if state.is_null() {
                return Err("Failed to create TCC state".to_string());
            }

            tcc_set_output_type(state, TCC_OUTPUT_MEMORY);

            tcc_set_lib_path(state, c_root_path.as_ptr());

            tcc_add_include_path(state, c_include_path.as_ptr());

            let opt_nostdlib = CString::new("-nostdlib").unwrap();
            tcc_set_options(state, opt_nostdlib.as_ptr());

            Ok(AbyssJit {
                state,
                _temp_dir: temp_dir,
                relocated: false,
            })
        }
    }

    pub fn compile(&mut self, c_code: &str) -> Result<(), String> {
        if self.relocated {
            return Err("Cannot compile after relocation. Create a new instance.".to_string());
        }

        let c_str = CString::new(c_code).unwrap();
        unsafe {
            let ret = tcc_compile_string(self.state, c_str.as_ptr());
            if ret == -1 {
                return Err("Compilation failed".to_string());
            }
        }
        Ok(())
    }

    pub fn add_function(&self, name: &str, func_ptr: *const c_void) {
        let c_name = CString::new(name).unwrap();
        unsafe {
            tcc_add_symbol(self.state, c_name.as_ptr(), func_ptr);
        }
    }

    pub fn finalize(&mut self) -> Result<(), String> {
        if self.relocated {
            return Ok(());
        }

        unsafe {
            let ret = tcc_relocate(self.state, TCC_RELOCATE_AUTO);
            if ret < 0 {
                return Err("Failed to relocate code (memory/permission error)".to_string());
            }
        }

        self.relocated = true;
        Ok(())
    }

    pub fn get_function<T>(&mut self, func_name: &str) -> Option<T> {
        if !self.relocated {
            if let Err(e) = self.finalize() {
                eprintln!("Auto-finalization failed: {}", e);
                return None;
            }
        }

        let c_name = CString::new(func_name).unwrap();
        unsafe {
            let sym = tcc_get_symbol(self.state, c_name.as_ptr());
            if sym.is_null() {
                None
            } else {
                Some(std::mem::transmute_copy(&sym))
            }
        }
    }
}

impl Drop for AbyssJit {
    fn drop(&mut self) {
        unsafe {
            if !self.state.is_null() {
                tcc_delete(self.state);
            }
        }
    }
}
