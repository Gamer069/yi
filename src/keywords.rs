use std::str::FromStr;

use crate::{ir::Signedness, keywords, tok::{SpannedTok, Tok}};
use yi_std::Type as YiType;

macro_rules! enum_str {
    // with optional `pub`
    ($(#[$meta:meta])* $vis:vis $name:ident { $($variant:ident => $str:expr),* $(,)? }) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant),*
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$variant => write!(f, $str)),*
                }
            }
        }

        impl std::str::FromStr for $name {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($str => Ok(Self::$variant)),*,
                    _ => Err(format!("Unknown {}: {}", stringify!($name), s))
                }
            }
        }
    };
}

macro_rules! enum_str_with_data {
    ($(#[$meta:meta])* $vis:vis $name:ident { $($variant:ident($ty:ty) => $str:expr),* $(,)? }) => {
        $(#[$meta])*
        $vis enum $name {
            $($variant($ty)),*
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$variant(_) => write!(f, $str)),*
                }
            }
        }
    };
}

pub fn keywords(toks: &mut Vec<SpannedTok>, id: String, line: usize, col: usize) {
	let ty = TypeKw::from_str(id.as_str());

	if let Ok(ty) = ty {
		toks.push(Tok::Type(ty).to_spanned(line, col));
		return;
	}

	match id.as_str() {
		"так" => toks.push(Tok::Val(keywords::TypeKwWithVal::Bool(true)).to_spanned(line, col)),
		"ні" => toks.push(Tok::Val(keywords::TypeKwWithVal::Bool(false)).to_spanned(line, col)),

		"змінна" => toks.push(Tok::Let.to_spanned(line, col)),
		"функ" => toks.push(Tok::Func.to_spanned(line, col)),
		"якщо" => toks.push(Tok::If.to_spanned(line, col)),
		"інакше" => toks.push(Tok::Else.to_spanned(line, col)),

		"верни" => toks.push(Tok::Return.to_spanned(line, col)),

		_ => toks.push(Tok::Id(id).to_spanned(line, col))
	}
}

enum_str! {
	#[derive(PartialEq, Debug, Clone)]
    pub TypeKw {
		I64 => "і64",
		// TODO: change later - ю,ф don't really make sense here
		U64 => "ю64",
		F64 => "ф64",

		Str => "str",
		Bool => "бул",

		Void => "ніщо",
    }
}

impl TypeKw {
	pub fn cranelift(&self) -> cranelift::prelude::Type {
		match self {
			TypeKw::I64 => cranelift::prelude::types::I64,
			TypeKw::U64 => cranelift::prelude::types::I64,
			TypeKw::F64 => cranelift::prelude::types::F64,
			TypeKw::Str => cranelift::prelude::types::I64,
			TypeKw::Bool => cranelift::prelude::types::I8,
			TypeKw::Void => cranelift::prelude::types::I32,
		}
	}

	pub fn from_cranelift(cr: cranelift::prelude::Type, signedness: Signedness) -> Self {
		match (cr, signedness) {
			(cranelift::prelude::types::I64, Signedness::Signed) => TypeKw::I64,
			(cranelift::prelude::types::I64, Signedness::Unsigned) => TypeKw::U64,
			(cranelift::prelude::types::F64, _) => TypeKw::F64,
			(cranelift::prelude::types::I8, _) => TypeKw::Bool,
			(cranelift::prelude::types::I32, Signedness::Signed) => TypeKw::Void,
			(cranelift::prelude::types::I32, Signedness::Unsigned) => TypeKw::Void,
			_ => { TypeKw::Void }
		}
	}

	pub fn std(&self) -> YiType {
		match self {
			TypeKw::I64 => YiType::I64,
			TypeKw::U64 => YiType::U64,
			TypeKw::F64 => YiType::F64,
			TypeKw::Str => YiType::Str,
			TypeKw::Bool => YiType::Bool,
			TypeKw::Void => YiType::Void,
		}
	}

	pub fn from_yi_std(ty: YiType) -> Self {
		match ty {
			YiType::I64 => TypeKw::I64,
			YiType::U64 => TypeKw::U64,
			YiType::I8 => TypeKw::Bool,
			YiType::F64 => TypeKw::F64,
			YiType::Str => TypeKw::Str,
			YiType::Bool => TypeKw::Bool,
			YiType::Void => TypeKw::Void,
		}
	}

	pub fn signedness(&self) -> Signedness {
		match self {
			TypeKw::I64 => Signedness::Signed,
			TypeKw::U64 => Signedness::Unsigned,
			// doesn't make any sense but idc anyway
			TypeKw::F64 => Signedness::Signed,
			// doesn't make any sense but idrc
			TypeKw::Str => Signedness::Unsigned,
			// doesn't make any sense but idc
			TypeKw::Bool => Signedness::Unsigned,
			TypeKw::Void => Signedness::Unsigned,
		}
	}
}

enum_str_with_data! {
	#[derive(PartialEq, Debug, Clone)]
    pub TypeKwWithVal {
		I64(i64) => "і64",
		// TODO: change later - ю,ф don't really make sense here
		U64(u64) => "ю64",
		F64(f64) => "ф64",

		Str(String) => "str",
		Bool(bool) => "бул",
		Void(()) => "ніщо",
    }
}
