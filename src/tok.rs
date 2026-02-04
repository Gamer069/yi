use std::fmt;

use crate::keywords;

#[derive(PartialEq, Debug, Clone)]
pub enum Tok {
	Func,
	Let,
	If,
	Else,
	Return,
	Id(String),
	Type(keywords::TypeKw),
	Val(keywords::TypeKwWithVal),
	Arrow,
	Colon,
	Comma,
	Add,
	Sub,
	Mul,
	Div,
	Not,
	Eq,
	EqEq,
	NotEq,
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
			Tok::Return => write!(f, "return"),
			Tok::If => write!(f, "if"),
			Tok::Else => write!(f, "else"),
			Tok::Id(s) => write!(f, "Id({})", s),
			Tok::Type(t) => write!(f, "Type({})", t),
			Tok::Val(ty) => match ty {
				keywords::TypeKwWithVal::I64(v) => write!(f, "I64({})", v),
				keywords::TypeKwWithVal::U64(v) => write!(f, "U64({})", v),
				keywords::TypeKwWithVal::F64(v) => write!(f, "F64({})", v),
				keywords::TypeKwWithVal::Str(v) => write!(f, "Str({})", v),
				keywords::TypeKwWithVal::Bool(v) => write!(f, "Bool({})", v),
				keywords::TypeKwWithVal::Void(()) => write!(f, "Void"),
			},
			Tok::Add => write!(f, "+"),
			Tok::Sub => write!(f, "-"),
			Tok::Mul => write!(f, "*"),
			Tok::Div => write!(f, "/"),
			Tok::Not => write!(f, "!"),
			Tok::Eq => write!(f, "="),
			Tok::NotEq => write!(f, "!="),
			Tok::EqEq => write!(f, "=="),
			Tok::Lp => write!(f, "("),
			Tok::Rp => write!(f, ")"),
			Tok::Sc => write!(f, ";"),
			Tok::Lc => write!(f, "{{"),
			Tok::Rc => write!(f, "}}"),
			Tok::Arrow => write!(f, "->"),
			Tok::Colon => write!(f, ":"),
			Tok::Comma => write!(f, ","),
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
