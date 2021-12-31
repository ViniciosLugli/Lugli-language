use crate::parser::parse;
use crate::token::generate;
use crate::{environment::Value, interpreter::Interpreter};

mod list;
mod number;
mod string;

pub use list::ListObject;
pub use number::NumberObject;
pub use string::StringObject;

pub fn arity(name: &str, arity: usize, arguments: &Vec<Value>, multiples_entry: bool) -> () {
	if multiples_entry {
		if arguments.len() < arity {
			panic!("{} expects {} arguments, but {} were given", name, arity, arguments.len());
		}
	} else {
		if arguments.len() != arity {
			panic!("{} expects {} arguments, but {} were given", name, arity, arguments.len());
		}
	}
}

pub fn println(_: &mut Interpreter, args: Vec<Value>) -> Value {
	arity("println!", 1, &args, true);
	let arg = args.get(0).unwrap().clone();

	println!("{}", arg.to_string());

	Value::Null
}

pub fn print(_: &mut Interpreter, args: Vec<Value>) -> Value {
	arity("print!", 1, &args, true);

	let arg = args.get(0).unwrap().clone();

	print!("{}", arg.to_string());

	Value::Null
}

pub fn r#type(_: &mut Interpreter, args: Vec<Value>) -> Value {
	arity("type!", 1, &args, false);

	let arg = args.first().unwrap();

	Value::String(arg.clone().typestring())
}

pub fn import(interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
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

pub fn exit(_interpreter: &mut Interpreter, args: Vec<Value>) -> Value {
	std::process::exit(if args.is_empty() { 0 } else { args.get(0).unwrap().clone().to_number() as i32 });
}
