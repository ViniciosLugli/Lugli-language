use clap::{Arg, Command};
use lugli;
use std::fs::read_to_string;

const VERSION: &str = "0.3";

fn main() {
	let matches = Command::new("Lugli")
		.version(VERSION)
		.author("Vinicios Lugli <vinicioslugli@gmail.com>")
		.about("My 'lugli' language for learning purposes.")
		.subcommand(Command::new("run").about("Run a Lugli file.").version(VERSION).arg(Arg::new("file").required(true)))
		.get_matches();

	match matches.subcommand() {
		Some(("run", sub_matches)) => {
			let file = sub_matches.get_one::<String>("file").unwrap();

			let input = read_to_string(file).unwrap();

			match lugli::interpret(&input) {
				Ok(_) => {}
				Err(e) => e.print(),
			}
		}
		_ => unreachable!(),
	}
}
