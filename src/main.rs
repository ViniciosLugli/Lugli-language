use clap::{App, Arg};
use std::fs::read_to_string;

mod ast;
mod environment;
mod interpreter;
mod parser;
mod stdlib;
mod token;

const VERSION: &str = "0.2-beta";

fn main() {
	let matches = App::new("Lugli")
		.version(VERSION)
		.author("Vinicios Lugli <vinicioslugli@gmail.com>")
		.about("My 'lugli' language compiler for learning purposes.")
		.subcommand(App::new("run").about("Run a Lugli file.").version(VERSION).arg(Arg::new("file").required(true)))
		.get_matches();

	if let Some(ref run) = matches.subcommand_matches("run") {
		let file = run.value_of("file").unwrap();
		let contents = read_to_string(file).unwrap();
		let path = std::path::PathBuf::from(file);
		let tokens = token::generate(contents.as_str());
		match parser::parse(tokens) {
			Ok(ast) => {
				match interpreter::interpret(ast, path) {
					Ok(_) => {}
					Err(e) => {
						e.print();
					}
				};
			}
			Err(e) => {
				e.print();
			}
		};
	}
}
