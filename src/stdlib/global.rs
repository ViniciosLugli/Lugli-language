use std::collections::HashMap;
use std::io::{stdout, Write};

use crate::parser::parse;
use crate::token::generate;

use crate::{
	environment::{NativeFunctionCallback, Value},
	interpreter::Interpreter,
};

pub struct GlobalObject;

impl GlobalObject {
	pub fn get_all() -> HashMap<String, NativeFunctionCallback> {
		let mut global_functions = HashMap::<String, NativeFunctionCallback>::new();

		global_functions.insert("println!".to_string(), global_println);
		global_functions.insert("print!".to_string(), global_print);
		global_functions.insert("type?".to_string(), global_type);
		global_functions.insert("import!".to_string(), global_import);
		global_functions.insert("exit!".to_string(), global_exit);
		global_functions.insert("input!".to_string(), global_input);

		global_functions
	}
}

fn global_println(_: &mut Interpreter, args: Vec<Value>) -> Value {
	super::arity("println!", 1, &args, true);

	let arg = args.get(0).unwrap().clone();
	let mut stdout = stdout();

	stdout.write(format!("{}\n", arg.to_string()).as_bytes()).unwrap();
	stdout.flush().unwrap();

	Value::Null
}

fn global_print(_: &mut Interpreter, args: Vec<Value>) -> Value {
	super::arity("print!", 1, &args, true);

	let arg = args.get(0).unwrap().clone();
	let mut stdout = stdout();

	stdout.write(arg.to_string().as_bytes()).unwrap();
	stdout.flush().unwrap();

	Value::Null
}

fn global_type(_: &mut Interpreter, args: Vec<Value>) -> Value {
	super::arity("type!", 1, &args, false);

	let arg = args.first().unwrap();

	Value::String(arg.clone().typestring())
}

fn global_import(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
	super::arity("import!", 1, &args, false);

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

fn global_exit(_interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
	std::process::exit(if args.is_empty() { 0 } else { args.get(0).unwrap().clone().to_number() as i32 });
}

fn global_input(_interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
	super::arity("input!", 0, &args, false);

	let mut input = String::new();

	std::io::stdin().read_line(&mut input).unwrap();

	Value::String(input)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_get_all_functions() {
		let functions = GlobalObject::get_all();

		assert!(!functions.is_empty())
	}
}
