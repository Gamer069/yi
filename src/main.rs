mod lexer;
mod tok;
mod parser;

use clap::{Parser, Subcommand};
use std::path::Path;
use crate::lexer::Lexer;
use crate::tok::Tok;

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
            }
            lex(file);
        }
    }
}

fn lex(file: String) {
    println!("File: {}", file);
    let lexer: Lexer = Lexer::new(file);
    let toks: Vec<Tok> = lexer.lex();
    let mut parser: crate::parser::Parser = crate::parser::Parser::new(toks);
    let ast = parser.parse();
    println!("{:?}", ast);
    println!("{:?}", parser.sym_table.into_inner());
}
