use crate::{YiStr, construct_yi_str, std};

std! {
	fn chytln() -> YiStr as "читлн" {
		let mut line = String::new();

		std::io::stdin().read_line(&mut line).expect("Failed to read line");

		construct_yi_str(&line)
	}
}
