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
		file: String,

		#[arg(short, long)]
		ir: bool,

		#[arg(short, long)]
		verbose: bool,

		#[arg(short, long, default_value = "yi_out")]
		output: String,
	},
}

fn main() {
	let cli = Cli::parse();

	match cli.command {
		Commands::Build { file, ir, verbose, output } => {
			let path = Path::new(&file);
			if !path.exists() {
				eeprintln!("{}: файл не знайдено", file);
				std::process::exit(-1);
			}
			if !path.is_file() {
				eeprintln!("{} директорія", file);
				std::process::exit(-1);
			}
			lex(file, ir, verbose, output);
		}
	}
}

fn lex(file: String, ir: bool, verbose: bool, output: String) {
	let lexer: Lexer = Lexer::new(file);
	let (toks, lines) = lexer.lex();

	let mut parser: parser::Parser = crate::parser::Parser::new(toks, lines);

	let ast = parser.parse().expect("Failed to parse");

	let mut generator: IRGenerator = IRGenerator::new(ast, parser.sym_table.into_inner(), ir, verbose, output);
	generator.generate();
}

#[cfg(test)]
mod tests {
    use std::{fs, path::Path, process::Command};

    /// Helper to run examples with a given target executable
    fn run_examples(yi_executable: &str, stdout_dir: &Path) {
        let examples_dir = Path::new("examples");

        // Collect all .yi files
        let mut examples: Vec<_> = fs::read_dir(examples_dir)
            .unwrap()
            .filter_map(|e| {
                let entry = e.unwrap();
                let path = entry.path();
                if path.extension().map(|ext| ext == "yi").unwrap_or(false) {
                    Some(path)
                } else {
                    None
                }
            })
            .collect();

        examples.sort(); // deterministic order

        for example_path in examples {
            let example_name = example_path.file_name().unwrap().to_string_lossy();
            println!("Running {}...", example_name);

            // Run yi build <file>
            let build_output = Command::new(yi_executable)
                .args(["build", &example_path.to_string_lossy()])
                .output()
                .expect("Failed to run yi build");

            assert!(
                build_output.status.success(),
                "yi build failed for {}:\nstdout:\n{}\nstderr:\n{}",
                example_name,
                String::from_utf8_lossy(&build_output.stdout),
                String::from_utf8_lossy(&build_output.stderr)
            );

            // Run the output
            let output = Command::new("./output")
                .output()
                .expect("Failed to run compiled output");

            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            assert!(
                output.status.success(),
                "Program {} exited with code {:?}\nstdout:\n{}\nstderr:\n{}",
                example_name,
                output.status.code(),
                stdout,
                stderr
            );

            // Compare stdout to reference file
            let ref_file = stdout_dir.join(example_name.replace(".yi", ".txt"));
            assert!(
                ref_file.exists(),
                "Reference stdout file {} does not exist",
                ref_file.display()
            );

            let expected = fs::read_to_string(ref_file).expect("Failed to read reference stdout");
            assert_eq!(stdout, expected, "Output mismatch for {}", example_name);
        }
    }

    fn cargo_build(debug: bool) {
        let args = if debug { vec!["build"] } else { vec!["build", "--release"] };

        let output = Command::new("cargo")
            .args(args)
            .output()
            .expect("Failed to run cargo build");

        assert!(
            output.status.success(),
            "cargo build failed:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn run_all_examples_debug() {
        cargo_build(true);
        run_examples("./target/debug/yi", Path::new("examples/stdout"));
    }

    #[test]
    fn run_all_examples_release() {
        cargo_build(false);
        run_examples("./target/release/yi", Path::new("examples/stdout"));
    }
}

