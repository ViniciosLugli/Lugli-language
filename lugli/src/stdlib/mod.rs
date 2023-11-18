use crate::{ast::ArgumentValues, environment::Value};

mod global;

pub use global::GlobalObject;

pub fn arity(name: &str, arity: usize, arguments: &ArgumentValues, multiples_entry: bool) -> () {
	if multiples_entry {
		if arguments.len() < arity {
			panic!("{} expects {} arguments, but {} were given", name, arity, arguments.len());
		}
	} else {
		if arguments.len() != arity {
			panic!("{} expects exactly {} arguments, but {} were given", name, arity, arguments.len());
		}
	}
}

pub fn parse_callback(callback: Value) -> Value {
	match &callback {
		Value::Function { .. } => callback,
		Value::NativeFunction { .. } => callback,
		Value::NativeMethod { .. } => callback,
		_ => panic!("Callback is not a function callable"),
	}
}
