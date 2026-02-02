use crate::tok::{SpannedTok, Tok};

pub fn keywords(toks: &mut Vec<SpannedTok>, id: String, line: usize, col: usize) {
	match id.as_str() {
		"так" => toks.push(Tok::Bool(true).to_spanned(line, col)),
		"ні" => toks.push(Tok::Bool(false).to_spanned(line, col)),
		"змінна" => toks.push(Tok::Let.to_spanned(line, col)),
		"функ" => toks.push(Tok::Func.to_spanned(line, col)),
		_ => toks.push(Tok::Id(id).to_spanned(line, col))
	}
}
