use hashbrown::HashMap;

use self::structs::console;

use super::arity;
use crate::environment::{NativeFunctionCallback, Value};

pub struct GlobalObject;

impl GlobalObject {
	pub fn get_all_functions() -> HashMap<String, NativeFunctionCallback> {
		let mut global_functions = HashMap::<String, NativeFunctionCallback>::new();

		global_functions.insert("type".to_string(), functions::global_type);

		global_functions
	}

	pub fn get_all_structs() -> HashMap<String, HashMap<String, Value>> {
		let mut global_struct = HashMap::<String, HashMap<String, Value>>::new();

		let mut application_methods = HashMap::<String, Value>::new();
		application_methods.insert("exit".to_string(), Value::NativeFunction { name: "exit!".to_string(), callback: structs::application::exit });
		global_struct.insert("Application".to_string(), application_methods);

		let mut console_methods = HashMap::<String, Value>::new();
		console_methods.insert("print".to_string(), Value::NativeFunction { name: "print!".to_string(), callback: structs::console::print });
		console_methods.insert("println".to_string(), Value::NativeFunction { name: "println!".to_string(), callback: structs::console::println });
		console_methods.insert("input".to_string(), Value::NativeFunction { name: "input!".to_string(), callback: structs::console::input });
		console_methods.insert("clear".to_string(), Value::NativeFunction { name: "clear!".to_string(), callback: structs::console::clear });
		global_struct.insert("Console".to_string(), console_methods);

		global_struct
	}
}

mod functions {
	use super::arity;
	use crate::ast::ArgumentValues;

	use crate::{environment::Value, interpreter::Interpreter};

	pub fn global_type(_: &mut Interpreter, args: ArgumentValues) -> Value {
		arity("type", 1, &args, false);

		let arg = args.get_from_name_or_index("value".to_string(), 0).unwrap();

		Value::String(arg.clone().typestring())
	}
}

mod structs {
	use super::arity;
	pub mod application {
		use crate::{ast::ArgumentValues, environment::Value, interpreter::Interpreter};

		pub fn exit(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
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
			arity("println", 1, &args, true);

			let content = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();
			let mut stdout = stdout();

			stdout.write(format!("{}\n", content).as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}

		pub fn print(_: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("print", 1, &args, true);

			let content = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();
			let mut stdout = stdout();

			stdout.write(content.as_bytes()).unwrap();
			stdout.flush().unwrap();

			Value::Null
		}

		pub fn input(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("input", 0, &args, false);

			let mut input = String::new();

			std::io::stdin().read_line(&mut input).unwrap();

			Value::String(input)
		}

		pub fn clear(_interpreter: &mut Interpreter, args: ArgumentValues) -> Value {
			arity("clear", 0, &args, false);

			let mut stdout = stdout();

			stdout.write(b"\x1b[2J").unwrap();
			stdout.flush().unwrap();

			Value::Null
		}
	}
}
