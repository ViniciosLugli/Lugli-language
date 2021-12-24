mod ast;
mod lexer;
mod parser;
mod token;

use std::{env, fs};

fn main() {
	// Clear terminal only
	std::process::Command::new("clear").status().unwrap();

	let file = if let Some(file) = env::args().nth(1) {
		file
	} else {
		panic!("No file specified.");
	};

	let contents = if let Ok(contents) = fs::read_to_string(&file) {
		contents
	} else {
		panic!("Could not read file {}.", file);
	};

	let lexer = lexer::Lexer::new(contents);
	let mut parser = parser::Parser::new(lexer);

	let program = parser.parse();

	println!("{:?}", program);
}
