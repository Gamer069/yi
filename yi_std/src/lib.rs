use std::{ffi::CStr, io::Write};

#[unsafe(no_mangle)]
pub extern "C" fn drukln() -> i32 {
    println!();
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn drukln_i64(val: i64) -> i32 {
    println!("{}", val);
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn drukln_str(str: *const i8) {
	unsafe {
		let c_str = CStr::from_ptr(str);
		let rust_str = c_str.to_str().unwrap();
		println!("{}", rust_str);
	}
}

#[unsafe(no_mangle)]
pub extern "C" fn druk() -> i32 {
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn druk_i64(val: i64) -> i32 {
    print!("{}", val);
    0
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn druk_str(str: *const i8) {
	unsafe {
		let c_str = CStr::from_ptr(str);
		let rust_str = c_str.to_str().unwrap();
		print!("{}", rust_str);
	}
}

#[unsafe(no_mangle)]
pub extern "C" fn skynuty_stdout() -> i32 {
    std::io::stdout().flush().map(|_| 0).unwrap_or(-1)
}
