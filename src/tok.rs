use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Tok {
	Func,
	Let,
	Id(String),
	I64(i64),
	U64(u64),
	F64(f64),
	Str(String),
	Bool(bool),
	Add,
	Sub,
	Mul,
	Div,
	Eq,
	EqEq,
	Lp,
	Rp,
	Sc,
	Rc,
	Lc,
}

impl Tok {
	pub fn to_spanned(&self, line: usize, col: usize) -> SpannedTok {
		SpannedTok { tok: self.clone(), line, col }
	}
}

impl fmt::Display for Tok {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Tok::Let => write!(f, "let"),
			Tok::Func => write!(f, "func"),
			Tok::Id(s) => write!(f, "Id({})", s),
			Tok::U64(n) => write!(f, "u64({})", n),
			Tok::I64(n) => write!(f, "i64({})", n),
			Tok::F64(float) => write!(f, "f64({})", float),
			Tok::Str(s) => write!(f, "\"{}\"", s),
			Tok::Bool(b) => write!(f, "{}", b),
			Tok::Add => write!(f, "+"),
			Tok::Sub => write!(f, "-"),
			Tok::Mul => write!(f, "*"),
			Tok::Div => write!(f, "/"),
			Tok::Eq => write!(f, "="),
			Tok::EqEq => write!(f, "=="),
			Tok::Lp => write!(f, "("),
			Tok::Rp => write!(f, ")"),
			Tok::Sc => write!(f, ";"),
			Tok::Lc => write!(f, "{{"),
			Tok::Rc => write!(f, "}}"),
		}
	}
}

#[derive(PartialEq, Debug, Clone)]
pub struct SpannedTok {
	pub tok: Tok,
	pub line: usize,
	pub col: usize,
}

impl SpannedTok {
	pub fn new(tok: Tok, line: usize, col: usize) -> Self {
		Self { tok, line, col }
	}
}
