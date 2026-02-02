use std::{cell::RefCell, rc::Rc};
use crate::keywords::TypeKw;
use crate::sym_table::SymTable;
use crate::{keywords, tok::*};

type AST = Vec<Statements>;

macro_rules! tok_err {
	($expected:expr,$got:expr,$line:expr) => {{
		eprintln!("{}помилка\x1b[0m: неочікуваний токен: очікувалось '{}', отримано '{}'", "\x1b[31m\x1b[1m", $expected, $got.tok);
		context!($got, $line);
	}};
}

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

macro_rules! tok_err_end {
	($expected:expr) => {{
		eprintln!("{}помилка:{} неочікуваний токен: очікувалось '{}', отримано кінець файлу", "\x1b[31m", "\x1b[0m", $expected);
	}};
}

macro_rules! tok_err_unknown {
	($got:expr, $line:expr) => {{
		eprintln!("{}помилка:{} неочікуваний токен: '{}'", "\x1b[31m", "\x1b[0m", $got.tok);
		context!($got, $line);
	}};
}

pub struct Parser {
	pub tokens: Vec<SpannedTok>,
	pub lines: Vec<String>,
	pub i: usize,
	pub sym_table: RefCell<SymTable>, // global
	pub cur_scope: Rc<RefCell<SymTable>>, // current local
	pub cur_scope_is_global: bool,
}

impl Parser {
	pub fn new(toks: Vec<SpannedTok>, lines: Vec<String>) -> Self {
		let gst = RefCell::new(SymTable::new());
		Self { tokens: toks, i: 0, lines, sym_table: gst.clone(), cur_scope: Rc::new(gst), cur_scope_is_global: true }
	}
	pub fn parse(&mut self) -> Result<AST, String> {
		let mut statements: AST = Vec::new();

		while self.i < self.tokens.len() {
			match self.cur_non_spanned() {
				Some(Tok::Func) => {
					let statement = self.parse_func_statement();
					statements.push(statement);
				},
				Some(Tok::Let) => {
					let statement = self.parse_let_statement();
					statements.push(statement);
				},
				Some(Tok::Sc) => {
					// Skip standalone semicolons
					self.eat();
					continue;
				},
				Some(Tok::Id(id)) => {
					self.eat();
					if self.cur_non_spanned() == Some(Tok::Eq) {
						self.eat();
						let expr = self.parse_expr();
						statements.push(Statements::Assign(id, Box::new(expr)));
					}
				},
				Some(_) => {
					let expr = self.parse_expr();
					statements.push(Statements::Expr(Box::new(expr)));

					if self.cur_non_spanned() == Some(Tok::Sc) {
						self.eat();
					}
				},
				None => break,
			}
		};

		Ok(statements)
	}

	fn parse_add_sub(&mut self) -> Expr {
		let mut left = self.parse_mul_div();
		while let Some(tok) = self.cur() {
			match tok.tok {
				Tok::Add | Tok::Sub => {
					let op = match tok.tok {
						Tok::Add => BinOp::Add,
						Tok::Sub => BinOp::Sub,
						_ => unreachable!(),
					};
					self.eat();
					let right = self.parse_mul_div();
					left = Expr::BinOp(Box::new(left), op, Box::new(right));
				},
				Tok::Sc | Tok::Rp => break, // Stop parsing expression at semicolon or right parenthesis
				_ => break,
			}
		};
		left
	}

	fn parse_expr(&mut self) -> Expr {
		self.parse_equality()
	}

	fn parse_equality(&mut self) -> Expr {
		let mut left = self.parse_add_sub();

		while let Some(tok) = self.cur() {
			if tok.tok == Tok::EqEq {
				self.eat(); // consume EqEq
				let right = self.parse_add_sub();
				left = Expr::BinOp(Box::new(left), BinOp::EqEq, Box::new(right));
			} else {
				break;
			}
		}
		left
	}

	fn parse_mul_div(&mut self) -> Expr {
		let mut left = self.parse_factor();

		while let Some(tok) = self.cur() {
			match tok.tok {
				Tok::Mul | Tok::Div => {
					let op = match tok.tok {
						Tok::Mul => BinOp::Mul,
						Tok::Div => BinOp::Div,
						_ => unreachable!(),
					};
					self.eat();
					let right = self.parse_factor();
					left = Expr::BinOp(Box::new(left), op, Box::new(right));
				},
				_ => break,
			}
		}
		left
	}

	fn cur_non_spanned(&self) -> Option<Tok> {
		if let Some(cur) = self.cur() { 
			Some(cur.tok)
		} else {
			None
		}
	}

	fn parse_factor(&mut self) -> Expr {
		let cur = self.cur_non_spanned();

		match cur {
			Some(Tok::Val(keywords::TypeKwWithVal::I64(num))) => {
				let value = num;
				self.eat();
				Expr::I64(value)
			},
			Some(Tok::Val(keywords::TypeKwWithVal::U64(num))) => {
				let value = num;
				self.eat();
				Expr::U64(value)
			},
			Some(Tok::Val(keywords::TypeKwWithVal::F64(num))) => {
				let value = num;
				self.eat();
				Expr::F64(value)
			},
			Some(Tok::Lp) => {
				self.eat();
				let expr_node = self.parse_expr();
				let cur = self.cur_non_spanned();
				if cur == Some(Tok::Rp) {
					self.eat();
				} else {
					let cur = self.cur().unwrap();
					tok_err!(Tok::Rp, cur, self.lines[cur.line - 1]);
					std::process::exit(-1);
				}

				expr_node
			},
			Some(Tok::Id(id)) => {
				let id_clone = id.clone();
				self.eat();

				if self.cur_non_spanned() == Some(Tok::Lp) {
					self.eat();

					let mut args = vec![];

					if self.cur_non_spanned() != Some(Tok::Rp) {
						loop {
							let expr = self.parse_expr();
							args.push(expr);

							if self.cur_non_spanned() == Some(Tok::Comma) {
								self.eat();
							} else {
								break;
							}
						}
					}

					self.eat_tok(Tok::Rp);
					Expr::Call(id_clone, args)
				} else {
					Expr::Id(id_clone)
				}
			},
			Some(Tok::Val(keywords::TypeKwWithVal::Str(s))) => {
				let s_clone = s.clone();
				self.eat();
				Expr::Str(s_clone)
			},
			Some(Tok::Val(keywords::TypeKwWithVal::Bool(val))) => {
				self.eat();
				Expr::Bool(val)
			},
			_ => {
				let cur = self.cur().unwrap();
				tok_err_unknown!(cur, self.lines[cur.line - 1]);
				std::process::exit(-1);
			},
		}
	}

	fn parse_let_statement(&mut self) -> Statements {
		self.eat_tok(Tok::Let);
		let id = self.parse_id();
		self.eat_tok(Tok::Eq);
		let expr = self.parse_expr();

		// Check for and consume semicolon if present
		if let Some(tok) = self.cur() && tok.tok == Tok::Sc {
			self.eat();
		}

		if self.cur_scope_is_global {
			self.sym_table.borrow_mut().put(&id, Symbol::Variable(expr.clone()));
		} else {
			self.cur_scope.borrow_mut().put(&id, Symbol::Variable(expr.clone()));
		}

		Statements::Let(id, Box::from(expr))
	}

	fn parse_func_statement(&mut self) -> Statements {
		self.eat_tok(Tok::Func);
		let id = self.parse_id();
		self.eat_tok(Tok::Lp);

		// // TODO: args
		let mut args = vec![];

		if self.cur_non_spanned() != Some(Tok::Rp) {
			loop {
				let id = self.eat_tok_var(Tok::Id("".to_string()), |tok| if let Tok::Id(id) = tok { Some(id.clone()) } else { None });

				self.eat_tok(Tok::Colon);

				let ty = self.eat_tok_var(Tok::Type(TypeKw::I64), |tok| if let Tok::Type(t) = tok { Some(t.clone()) } else { None });

				args.push((id, ty));

				if self.cur_non_spanned() == Some(Tok::Comma) {
					self.eat();  // eat comma
				} else {
					break;  // no more args
				}
			}
		}

		self.eat_tok(Tok::Rp);

		let ret_ty = if let Some(_) = self.opt_tok(Tok::Arrow) {
			// dummy val
			self.eat_tok_var(Tok::Type(TypeKw::I64), |tok| if let Tok::Type(t) = tok { Some(t.clone()) } else { None })
		} else {
			TypeKw::Void
		};

		self.eat_tok(Tok::Lc);

		let mut statements: AST = Vec::new();

		let prev_scope = self.cur_scope.clone();
		let func_scope = Rc::new(RefCell::new(SymTable::new()));
		self.cur_scope = func_scope.clone();
		self.cur_scope_is_global = false;

		for (name, _ty) in args.clone() {
			let val = Symbol::Variable(Expr::Arg(name.clone()));

			self.cur_scope.borrow_mut().put(&name, val);
		}

		while let Some(cur) = self.cur() {
			match cur.tok {
				Tok::Rc => {
					break;
				},
				Tok::Func => {
					self.parse_func_statement();
				},
				Tok::Let => {
					let statement = self.parse_let_statement();
					statements.push(statement);
				},
				Tok::Sc => {
					self.eat();

					continue;
				},
				Tok::Id(id) => {
					self.eat();

					if self.cur_non_spanned() == Some(Tok::Lp) {
						self.eat();

						// // TODO: args
						let mut args = vec![];

						if self.cur_non_spanned() != Some(Tok::Rp) {
							loop {
								let expr = self.parse_expr();
								args.push(expr);

								if self.cur_non_spanned() == Some(Tok::Comma) {
									self.eat();
								} else {
									break;
								}
							}
						}

						self.eat_tok(Tok::Rp);
						statements.push(Statements::Expr(Box::new(Expr::Call(id.clone(), args))));
					}

					if let Some(tok) = self.cur() && let Tok::Eq = tok.tok {
						self.eat();
						let expr = self.parse_expr();
						self.cur_scope.borrow_mut().set(&id, Symbol::Variable(expr)).expect("Failed to assign to variable");
					}
				},
				Tok::Return => {
					self.eat();

					let expr = self.parse_expr();
					statements.push(Statements::Return(Box::new(expr)));

					if let Some(tok) = self.cur() && let Tok::Sc = tok.tok {
						self.eat();
					}
				},
				_ => {
					let expr = self.parse_expr();
					statements.push(Statements::Expr(Box::new(expr)));

					if let Some(tok) = self.cur() && let Tok::Sc = tok.tok {
						self.eat();
					}
				},
			}
		};

		self.sym_table.borrow_mut().put(&id, Symbol::Function(statements.clone(), self.cur_scope.clone()));

		self.cur_scope = prev_scope;

		// TODO: use a smarter way to determine current scope globalness - nested functions will
		// in-fact exist
		self.cur_scope_is_global = true;

		self.eat_tok(Tok::Rc);

		Statements::Func(id, args, ret_ty, statements)
	}

	fn parse_id(&mut self) -> String {
		if let Some(tok) = self.cur() && let Tok::Id(id) = tok.tok {
			let id_clone = id.clone();
			self.eat();
			id_clone
		} else {
			let cur = self.cur().unwrap();
			tok_err!(Tok::Id(String::new()), cur, self.lines[cur.line - 1]);
			std::process::exit(-1);
		}
	}

	fn cur(&self) -> Option<SpannedTok> {
		self.tokens.get(self.i).cloned()
	}

	fn eat(&mut self) -> Option<SpannedTok> {
		let tok = self.cur();
		self.i += 1;
		tok
	}

	fn opt_tok(&mut self, expected_tok: Tok) -> Option<SpannedTok> {
		if let Some(cur_tok) = self.cur() {
			if std::mem::discriminant(&cur_tok.tok) == std::mem::discriminant(&expected_tok) {
				self.eat();
				return self.tokens.get(self.i - 1).cloned();
			}
		}
		None
	}

	fn eat_tok(&mut self, expected_tok: Tok) {
		if let Some(cur_tok) = self.cur() {
			// Use discriminant comparison instead of direct equality
			// This ensures we only check the enum variant, not the contained values
			if std::mem::discriminant(&cur_tok.tok) == std::mem::discriminant(&expected_tok) {
				self.eat();
			} else {
				tok_err!(expected_tok, cur_tok, self.lines[cur_tok.line-1]);
				std::process::exit(-1);
			}
		} else {
			tok_err_end!(expected_tok);
			std::process::exit(-1);
		}
	}

	fn eat_tok_var<T>(&mut self, expected_tok: Tok, extractor: impl FnOnce(&Tok) -> Option<T>) -> T {
		if let Some(cur_tok) = self.cur() {
			if std::mem::discriminant(&cur_tok.tok) == std::mem::discriminant(&expected_tok) {
				if let Some(value) = extractor(&cur_tok.tok) {
					self.eat();
					return value;
				}
			}
			tok_err!(expected_tok, cur_tok, self.lines[cur_tok.line-1]);
			std::process::exit(-1);
		} else {
			tok_err_end!(expected_tok);
			std::process::exit(-1);
		}
	}
}

// Define BinOp enum to match the new token structure
#[derive(Debug, Clone)]
pub enum BinOp {
	Add,
	Sub,
	Mul,
	Div,
	EqEq,
}

#[derive(Debug, Clone)]
pub enum Symbol {
	Variable(Expr),
	Function(Vec<Statements>, Rc<RefCell<SymTable>>)
}

#[derive(Debug, Clone)]
pub enum Expr {
	U64(u64),
	F64(f64),
	I64(i64),
	Str(String),
	Bool(bool),
	Id(String),
	Call(String, Vec<Expr>),
	Arg(String),
	BinOp(Box<Expr>, BinOp, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statements {
	Let(String, Box<Expr>),
	Assign(String, Box<Expr>),
	Func(String, Vec<(String, keywords::TypeKw)>, keywords::TypeKw, Vec<Statements>),
	Expr(Box<Expr>),
	Return(Box<Expr>),
}
