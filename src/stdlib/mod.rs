use crate::{environment::Value, interpreter::Interpreter};

mod list;
mod number;
mod string;

pub use list::ListObject;
pub use number::NumberObject;
pub use string::StringObject;

pub fn arity(name: &str, arity: usize, arguments: &Vec<Value>) {
	if arity != arguments.len() {
		panic!("Method {} expected {} arguments, received {}.", name, arity, arguments.len());
	}
}

pub fn println(_: &mut Interpreter, args: Vec<Value>) -> Value {
	let arg = args.get(0).unwrap().clone();

	println!("{}", arg.to_string());

	Value::Null
}

pub fn print(_: &mut Interpreter, args: Vec<Value>) -> Value {
	let arg = args.get(0).unwrap().clone();

	print!("{}", arg.to_string());

	Value::Null
}

pub fn r#type(_: &mut Interpreter, args: Vec<Value>) -> Value {
	if args.is_empty() || args.len() > 1 {
		panic!("Function {} expects {} argument, received {}", "type", 1, args.len());
	}

	let arg = args.first().unwrap();

	Value::String(arg.clone().typestring())
}
