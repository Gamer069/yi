use std::{ffi::CStr, io::Write as _};

use crate::{YiStr, std};

std! {
	fn drukln() as "друклн" {
		println!();
	}

	fn drukln_i64(val: i64) as "друклн" {
		println!("{}", val);
	}

	fn drukln_bool(val: bool) as "друклн" {
		if val { println!("так") } else { println!("ні") };
	}

	fn drukln_str(str: YiStr) as "друклн" {
		unsafe {
			let c_str = CStr::from_ptr(str);
			let rust_str = c_str.to_str().unwrap();
			println!("{}", rust_str);
		}
	}

	fn druk() as "друк" {}

	fn druk_i64(val: i64) as "друк" {
		print!("{}", val);
	}

	fn druk_bool(val: i8) as "друк" {
		if val == 1 { print!("так") } else { print!("ні") };
	}

	fn druk_str(str: YiStr) as "друк" {
		unsafe {
			let c_str = CStr::from_ptr(str);
			let rust_str = c_str.to_str().unwrap();
			print!("{}", rust_str);
		}
	}

	fn skynuty_stdout() -> i64 as "скинути_вивід" {
		std::io::stdout().flush().map(|_| 0).unwrap_or(-1)
	}
}
