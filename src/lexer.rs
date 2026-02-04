use std::fs;
use std::iter::Peekable;
use std::str::Chars;

use crate::{keywords, tok::{SpannedTok, Tok}};

macro_rules! eeprintln {
	($($arg:tt)*) => {{
		eprintln!("{}помилка:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
	}};
}

pub struct Lexer {
	code: String
}

impl Lexer {
	pub fn new(fname: String) -> Lexer {
		let codestring = fs::read_to_string(fname).expect("Не вийшло прочитати файл!");
		Self { code: codestring }
	}

	pub fn lex(&self) -> (Vec<SpannedTok>, Vec<String>) {
		let mut toks: Vec<SpannedTok> = Vec::new();
		let mut chars = self.code.chars().peekable();
		let mut is_comment = false;
		let mut line = 1;
		let mut col = 0;
		let lines = self.code.lines().map(|s| s.to_owned()).collect::<Vec<String>>();

		while let Some(c) = chars.next() {
			col += 1;

			if c == '\n' {
				line += 1;
				col = 0;
				continue;
			}

			if !is_comment {
				match c {
					'A'..='z' => {
						eeprintln!("Англійський тут не працює");
						std::process::exit(-1);
					},
					c if unicode_ident::is_xid_start(c) => {
						let mut id = String::new();
						id.push(c);

						while let Some(&next) = chars.peek() {
							if unicode_ident::is_xid_continue(next) {
								id.push(chars.next().unwrap());
								col += 1;
							} else {
								break;
							}
						}

						keywords::keywords(&mut toks, id, line, col);
					},
					'=' => {
						match chars.peek() {
							Some(&'=')=>{
								toks.push(Tok::EqEq.to_spanned(line, col));
								chars.next();
							}
							_=>{
								toks.push(Tok::Eq.to_spanned(line, col));
							}
						}
					},
					',' => {
						toks.push(Tok::Comma.to_spanned(line, col));
					},
					':' => {
						toks.push(Tok::Colon.to_spanned(line, col));
					},
					';' => {
						toks.push(Tok::Sc.to_spanned(line, col));
					},
					'+' => {
						toks.push(Tok::Add.to_spanned(line, col));
					},
					'-' => {
						let peeked = chars.peek();
						if peeked.is_some() {
							let peekedchar = *peeked.unwrap();
							if '0' <= peekedchar && peekedchar <= '9' {
								self.lex_num(true, c, &mut chars, &mut toks, line, &mut col);
							} else if peekedchar == '>' {
								chars.next();
								col += 1;

								toks.push(Tok::Arrow.to_spanned(line, col));
							} else {
								toks.push(Tok::Sub.to_spanned(line, col));
							}
						}
					},
					'*' => {
						toks.push(Tok::Mul.to_spanned(line, col));
					},
					'/' => {
						let peeked = chars.peek();
						if peeked.is_some() {
							if *peeked.unwrap() == '/' {
								is_comment = true;

								continue;
							}
						}
						toks.push(Tok::Div.to_spanned(line, col));
					},
					'!' => {
						let peeked = chars.peek();
						if peeked == Some(&'=') {
							toks.push(Tok::NotEq.to_spanned(line, col));
							chars.next();
						} else {
							toks.push(Tok::Not.to_spanned(line, col));
						}
					},
					'(' => {
						toks.push(Tok::Lp.to_spanned(line, col));
					},
					')' => {
						toks.push(Tok::Rp.to_spanned(line, col));
					},
					'{' => {
						toks.push(Tok::Lc.to_spanned(line, col));
					},
					'}' => {
						toks.push(Tok::Rc.to_spanned(line, col));
					},
					'"' => {
						let mut s: String = String::new();
						let mut chars_iter = chars.clone();
						while let Some(peeked_char) = chars_iter.peek() {
							if *peeked_char != '"' {
								s.push(chars_iter.next().unwrap());
								chars = chars_iter.clone();
							} else {
								chars.next();
								break;
							}
						}
						toks.push(Tok::Val(keywords::TypeKwWithVal::Str(s)).to_spanned(line, col));
					},
					'0'..='9' => {
						self.lex_num(false, c, &mut chars, &mut toks, line, &mut col);
					},
					_ => {}
				};
			} else {
				if c == '\n' {
					is_comment = false;
				}
			};
		};
		(toks, lines)
	}
	fn lex_num<'a>(&self, neg: bool, c: char, chars: &mut Peekable<Chars<'a>>, toks: &mut Vec<SpannedTok>, line: usize, col: &mut usize) {
		let mut num: String = String::from(c);
		let mut chars_iter = chars.clone();
		while let Some(peeked) = chars_iter.peek() {
			match *peeked {
				'-' => {
					num.push(chars_iter.next().unwrap());
					*chars = chars_iter.clone();
				},
				'0'..='9' => {
					num.push(chars_iter.next().unwrap());
					*chars = chars_iter.clone();
				},
				'.' => {
					num.push(chars_iter.next().unwrap());
					*chars = chars_iter.clone();
				},
				_ => { 
					break;
				}
			}
			*col += 1;
		}
		if num.is_empty() {
			eeprintln!("Число пусте...");
			return;
		}

		if num.contains('.') {
			toks.push(Tok::Val(keywords::TypeKwWithVal::F64(num.parse::<f64>().unwrap())).to_spanned(line, *col));
		} else {
			if neg {
				toks.push(Tok::Val(keywords::TypeKwWithVal::I64(num.parse::<i64>().unwrap())).to_spanned(line, *col));
			} else {
				toks.push(Tok::Val(keywords::TypeKwWithVal::U64(num.parse::<u64>().unwrap())).to_spanned(line, *col));
			}
		}

	}
}

