#[macro_export]
macro_rules! context {
	($got:expr,$line:expr) => {{
		eprintln!("\x1b[34m --> ряд {} стовп {}\x1b[0m", $got.line, $got.col);

		let len = $line.len();

		eprintln!("\x1b[34m{} |{}", " ".repeat($got.line.to_string().len()), " ".repeat(len));
		eprintln!("{} |  {}{}\x1b[0m", $got.line, "\x1b[1m\x1B[38;2;100;200;220m", $line);
		eprint!("{} |\x1b[0m", " ".repeat($got.line.to_string().len()));

		for i in 1..=len {
			if i == $got.col+3 {
				eprint!("\x1b[31m^\x1b[0m");
			} else {
				eprint!(" ");
			}
		}
		eprintln!("");
	}};
}


#[macro_export]
macro_rules! tok_err {
	($expected:expr,$got:expr,$line:expr) => {{
		eprintln!("{}помилка\x1b[0m: неочікуваний токен: очікувалось '{}', отримано '{}'", "\x1b[31m\x1b[1m", $expected, $got.tok);
		crate::context!($got, $line);
	}};
}

#[macro_export]
macro_rules! tok_err_end {
	($expected:expr) => {{
		eprintln!("{}помилка:{} неочікуваний токен: очікувалось '{}', отримано кінець файлу", "\x1b[31m", "\x1b[0m", $expected);
	}};
}

#[macro_export]
macro_rules! tok_err_unknown {
	($got:expr, $line:expr) => {{
		eprintln!("{}помилка:{} неочікуваний токен: '{}'", "\x1b[31m", "\x1b[0m", $got.tok);
		crate::context!($got, $line);
	}};
}

