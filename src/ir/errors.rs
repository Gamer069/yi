#[macro_export]
macro_rules! eeprintln {
	($($arg:tt)*) => {{
		eprintln!("{}помилка:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
	}};
}

