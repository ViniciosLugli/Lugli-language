use hashbrown::HashMap;

use super::arity;
use crate::{
	ast::ArgumentValues,
	environment::{NativeFunctionCallback, Value},
	interpreter::Interpreter,
};

pub struct GlobalObject;

// FIXME: Change all file to new ArgumentValues

impl GlobalObject {
	pub fn get_all_functions() -> HashMap<String, NativeFunctionCallback> {
		let mut global_functions = HashMap::<String, NativeFunctionCallback>::new();

		global_functions.insert("type?".to_string(), functions::global_type);
		global_functions.insert("import!".to_string(), functions::global_import);

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
		console_methods.insert("input!".to_string(), Value::NativeFunction { name: "input!".to_string(), callback: structs::console::input });
		global_struct.insert("Console".to_string(), console_methods);

		let mut time_methods = HashMap::<String, Value>::new();
		time_methods.insert("sleep!".to_string(), Value::NativeFunction { name: "sleep!".to_string(), callback: structs::time::sleep });
		time_methods.insert("now?".to_string(), Value::NativeFunction { name: "now?".to_string(), callback: structs::time::now });
		time_methods.insert("datetime?".to_string(), Value::NativeFunction { name: "datetime?".to_string(), callback: structs::time::datetime });
		global_struct.insert("Time".to_string(), time_methods);

		global_struct
	}
}

mod functions {
	use super::arity;
	use crate::ast::ArgumentValues;
	use crate::parser::parse;
	use crate::token::generate;

	use crate::{environment::Value, interpreter::Interpreter};

	pub fn global_type(_: &mut Interpreter, args: ArgumentValues) -> Value {
		arity("type!", 1, &args, false);

		let arg = args.get_from_name_or_index("value".to_string(), 0).unwrap();

		Value::String(arg.clone().typestring())
	}

	pub fn global_import(interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
		arity("import!", 1, &args, false);

		let path = args.get_from_name_or_index("path".to_string(), 0).unwrap().to_string();
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
}

mod structs {
	use super::arity;
	pub mod application {
		use crate::{ast::ArgumentValues, environment::Value, interpreter::Interpreter};

		pub fn exit(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			// arity("exit!", 0, &args, false); TODO: Implement arity with optional arguments

			std::process::exit(if args.is_empty() {
				0
			} else {
				args.get_from_name_or_index("code".to_string(), 0).unwrap().clone().to_number() as i32
			});
		}
	}

	pub mod console {
		use super::arity;
		use crate::{ast::ArgumentValues, environment::Value, interpreter::Interpreter};
		use std::io::{stdout, Write};

		pub fn println(_: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("println!", 1, &args, true);

			let content = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();
			let mut stdout = stdout();

			stdout.write(format!("{}\n", content).as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}

		pub fn print(_: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("print!", 1, &args, true);

			let content = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();
			let mut stdout = stdout();

			stdout.write(content.as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}

		pub fn input(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("input!", 0, &args, false);

			let mut input = String::new();

			std::io::stdin().read_line(&mut input).unwrap();

			Value::String(input)
		}
	}

	pub mod time {
		use super::arity;
		use crate::{ast::ArgumentValues, environment::Value, interpreter::Interpreter};
		use chrono::Utc;

		pub fn sleep(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("sleep!", 1, &args, false);

			let duration = args.get_from_name("duration".to_string()).unwrap().to_number();

			std::thread::sleep(std::time::Duration::from_millis(duration as u64));

			Value::Null
		}

		pub fn now(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("now?", 0, &args, false);

			Value::Number(Utc::now().timestamp() as f64)
		}

		pub fn datetime(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("datetime?", 0, &args, false);

			Value::DateTime(chrono::offset::Utc::now())
		}
	}
}
