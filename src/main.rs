use std::{
    collections::HashMap,
    ffi::CStr,
    fs::{self},
    io::Read,
    os::raw::{c_char, c_double, c_longlong, c_void},
    sync::{
        Mutex, OnceLock,
        atomic::{AtomicI64, Ordering},
    },
    time::Instant,
};

use abyss::AbyssJit;
use abyss_analyzer::{flattener::Flattener, ir::Ir};
use abyss_codegen::{ctarget::ctarget::CTarget, director::Director};
use abyss_parser::ast::{FunctionBody, Stmt, get_debug_prog};
// use abyss_codegen::{ctarget::ctarget::CTarget, director::Director, target::Target};
// use abyss_parser::{ast::Type, parser::Parser};

unsafe extern "C" fn ab_print_str(ptr: *const c_char) {
    if ptr.is_null() {
        return;
    }
    let c_str = CStr::from_ptr(ptr);
    print!("{}", c_str.to_string_lossy());
}

unsafe extern "C" fn ab_print_i64(x: c_longlong) {
    print!("{}", x);
}

unsafe extern "C" fn ab_print_f64(x: c_double) {
    print!("{}", x);
}

unsafe extern "C" fn ab_newline() {
    println!();
}

static mut SEED: u64 = 0x123456789ABCDEF;

#[inline]
fn rand_u64() -> u64 {
    unsafe {
        let mut x = SEED;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        SEED = x;
        x
    }
}

#[inline]
pub fn rand() -> f64 {
    let x = rand_u64();
    (x as f64) / (u64::MAX as f64)
}

static NEXT_ARRAY_ID: AtomicI64 = AtomicI64::new(1);

fn array_heap() -> &'static Mutex<HashMap<i64, Vec<f64>>> {
    static HEAP: OnceLock<Mutex<HashMap<i64, Vec<f64>>>> = OnceLock::new();
    HEAP.get_or_init(|| Mutex::new(HashMap::new()))
}

pub extern "C" fn arr_alloc(size: i64) -> i64 {
    if size < 0 {
        eprintln!("[Runtime Error] Array size cannot be negative: {}", size);
        return 0;
    }

    let id = NEXT_ARRAY_ID.fetch_add(1, Ordering::Relaxed);
    let new_vec = vec![0.0; size as usize];

    let heap = array_heap();
    if let Ok(mut map) = heap.lock() {
        map.insert(id, new_vec);
    }

    id
}

pub extern "C" fn arr_set(id: i64, index: i64, value: f64) {
    let heap = array_heap();
    if let Ok(mut map) = heap.lock() {
        if let Some(vec) = map.get_mut(&id) {
            if index >= 0 && index < vec.len() as i64 {
                vec[index as usize] = value;
            } else {
                eprintln!(
                    "[Runtime Error] Array index out of bounds: ID={}, Index={}, Len={}",
                    id,
                    index,
                    vec.len()
                );
            }
        } else {
            eprintln!(
                "[Runtime Error] Accessing freed or invalid array ID: {}",
                id
            );
        }
    }
}

pub extern "C" fn arr_get(id: i64, index: i64) -> f64 {
    let heap = array_heap();
    if let Ok(map) = heap.lock() {
        if let Some(vec) = map.get(&id) {
            if index >= 0 && index < vec.len() as i64 {
                return vec[index as usize];
            } else {
                eprintln!(
                    "[Runtime Error] Array index out of bounds: ID={}, Index={}, Len={}",
                    id,
                    index,
                    vec.len()
                );
            }
        } else {
            eprintln!(
                "[Runtime Error] Accessing freed or invalid array ID: {}",
                id
            );
        }
    }
    0.0
}

pub extern "C" fn arr_free(id: i64) {
    let heap = array_heap();
    if let Ok(mut map) = heap.lock() {
        if map.remove(&id).is_none() {
            eprintln!("[Runtime Warning] Double free or invalid ID: {}", id);
        }
    }
}

pub extern "C" fn arr_len(id: i64) -> i64 {
    let heap = array_heap();
    if let Ok(map) = heap.lock() {
        if let Some(vec) = map.get(&id) {
            return vec.len() as i64;
        }
    }
    0
}

// fn main() {
//     let mut input = String::new();
//     fs::File::open("main.a")
//         .unwrap()
//         .read_to_string(&mut input)
//         .unwrap();

//     println!("Compiling...");

//     let t = Instant::now();

//     let mut target = CTarget::new();
//     let mut director = Director::new(&mut target);

//     let mut parser = Parser::new(&input);
//     println!("{}", parser.format_errors("test.a"));
//     director.process_program(&parser.parse_program());

//     let code = target.get_code();
//     //println!("{}", code);

//     let c_code = code;

//     let mut jit = AbyssJit::new();

//     jit.add_function("print", ab_print_str as *const c_void);
//     jit.add_function("print_i64", ab_print_i64 as *const c_void);
//     jit.add_function("print_f64", ab_print_f64 as *const c_void);
//     jit.add_function("newline", ab_newline as *const c_void);

//     jit.compile(c_code).expect("Compile error");
//     jit.finalize().expect("Relocation error");

//     println!("Finished in: {}ms", t.elapsed().as_millis());
//     println!("Running...\n");

//     type EntryFn = extern "C" fn();

//     if let Some(entry) = jit.get_function::<EntryFn>("entry") {
//         entry();
//     } else {
//         println!("Function 'entry' not found!");
//     }
// }
fn main() {
    let program = get_debug_prog();

    let flattener = Flattener::new();
    let flat_program = flattener.flatten(program);

    let ir = Ir::build(&flat_program);

    let mut target = CTarget::new();
    let mut compiler = Director::new(&mut target);
    compiler.process_program(&ir);

    println!("{}", target.get_code());

    let c_code = r#"



          void entry() {

              while(1) {

              }
          }
      "#;

    let mut jit = AbyssJit::new().unwrap();

    jit.compile(c_code).expect("Compile error");
    jit.finalize().expect("Relocation error");

    type FnType = extern "C" fn();

    let func = jit
        .get_function::<FnType>("entry")
        .expect("Function not found");

    func();
}
