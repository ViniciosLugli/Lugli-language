use hashbrown::HashMap;

use super::arity;
use crate::environment::{NativeFunctionCallback, Value};

pub struct GlobalObject;

impl GlobalObject {
	pub fn get_all_functions() -> HashMap<String, NativeFunctionCallback> {
		let mut global_functions = HashMap::<String, NativeFunctionCallback>::new();

		global_functions.insert("type?".to_string(), functions::global_type);
		global_functions.insert("import!".to_string(), functions::global_import);
		global_functions.insert("input!".to_string(), functions::global_input);

		global_functions
	}

	pub fn get_all_structs() -> HashMap<String, HashMap<String, Value>> {
		let mut global_struct = HashMap::<String, HashMap<String, Value>>::new();

		let mut application_methods = HashMap::<String, Value>::new();
		application_methods.insert("exit!".to_string(), Value::NativeFunction { name: "exit!".to_string(), callback: structs::application::exit });
		global_struct.insert("Application".to_string(), application_methods);

		let mut console_methods = HashMap::<String, Value>::new();
		console_methods.insert("print!".to_string(), Value::NativeFunction { name: "print!".to_string(), callback: structs::console::print });
		console_methods.insert("println!".to_string(), Value::NativeFunction { name: "println!".to_string(), callback: structs::console::println });
		global_struct.insert("Console".to_string(), console_methods);

		global_struct
	}
}

mod functions {
	use super::arity;
	use crate::parser::parse;
	use crate::token::generate;

	use crate::{environment::Value, interpreter::Interpreter};

	pub fn global_type(_: &mut Interpreter, args: Vec<Value>) -> Value {
		arity("type!", 1, &args, false);

		let arg = args.first().unwrap();

		Value::String(arg.clone().typestring())
	}

	pub fn global_import(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
		arity("import!", 1, &args, false);

		let path = args.first().unwrap().clone().to_string();
		let directory = interpreter.path().parent().unwrap().to_path_buf();

		let mut module_path = directory.clone();
		if path.ends_with(".lg") {
			module_path.push(path);
		} else {
			module_path.push(path + ".lg");
		}

		let module_path = module_path.canonicalize().unwrap();
		let contents = if module_path.exists() {
			std::fs::read_to_string(&module_path).unwrap()
		} else {
			panic!("File {} does not exist.", module_path.to_str().unwrap())
		};

		let tokens = generate(&contents);
		let ast = if let Ok(ast) = parse(tokens) {
			ast
		} else {
			panic!("Failed to parse module {}.", module_path.to_str().unwrap());
		};

		let value = match interpreter.exec(ast) {
			Ok(_) => Value::Null,
			Err(e) => {
				e.print();
				std::process::exit(1);
			}
		};

		return value;
	}

	pub fn global_input(_interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
		arity("input!", 0, &args, false);

		let mut input = String::new();

		std::io::stdin().read_line(&mut input).unwrap();

		Value::String(input)
	}
}

mod structs {
	use super::arity;
	pub mod application {
		use crate::{environment::Value, interpreter::Interpreter};

		pub fn exit(_interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
			// arity("exit!", 0, &args, false); TODO: Implement arity with optional arguments

			std::process::exit(if args.is_empty() { 0 } else { args.get(0).unwrap().clone().to_number() as i32 });
		}
	}

	pub mod console {
		use super::arity;
		use crate::{environment::Value, interpreter::Interpreter};
		use std::io::{stdout, Write};

		pub fn println(_: &mut Interpreter, args: Vec<Value>) -> Value {
			arity("println!", 1, &args, true);

			let arg = args.get(0).unwrap().clone();
			let mut stdout = stdout();

			stdout.write(format!("{}\n", arg.to_string()).as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}

		pub fn print(_: &mut Interpreter, args: Vec<Value>) -> Value {
			arity("print!", 1, &args, true);

			let arg = args.get(0).unwrap().clone();
			let mut stdout = stdout();

			stdout.write(arg.to_string().as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}
	}
}
