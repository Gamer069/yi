#![allow(dead_code)]
#![feature(fn_traits)]

mod lexer;
mod tok;
mod parser;
mod ir;
mod sym_table;
mod keywords;

use clap::{Parser, Subcommand};
use std::path::Path;
use crate::{ir::IRGenerator, lexer::Lexer};

macro_rules! eeprintln {
	($($arg:tt)*) => {{
		eprintln!("{}помилка в аргументах:{} {}", "\x1b[31m", "\x1b[0m", format!($($arg)*));
	}};
}

#[derive(Parser)]
#[command(version = "1.0", about = "Українська мова програмування")]
struct Cli {
	#[command(subcommand)]
	command: Commands,
}

#[derive(Subcommand)]
enum Commands {
	#[clap(alias = "b")]
	Build {
		file: String
	},
}

fn main() {
	let cli = Cli::parse();

	match cli.command {
		Commands::Build { file } => {
			let path = Path::new(&file);
			if !path.exists() {
				eeprintln!("{}: файл не знайдено", file);
				std::process::exit(-1);
			}
			if !path.is_file() {
				eeprintln!("{} директорія", file);
				std::process::exit(-1);
			}
			lex(file);
		}
	}
}

fn lex(file: String) {
	let lexer: Lexer = Lexer::new(file);
	let (toks, lines) = lexer.lex();

	let mut parser: parser::Parser = crate::parser::Parser::new(toks, lines);

	let ast = parser.parse().expect("Failed to parse");

	let mut generator: IRGenerator = IRGenerator::new(ast, parser.sym_table.into_inner());
	generator.generate();
}
