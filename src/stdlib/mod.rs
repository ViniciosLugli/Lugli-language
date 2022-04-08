use crate::{
	ast::{ArgumentValues, CallArguments},
	environment::Value,
};

mod datetime;
mod global;
mod list;
mod number;
mod string;

use clap::ValueHint;
pub use datetime::DateTimeObject;
pub use global::GlobalObject;
pub use list::ListObject;
pub use number::NumberObject;
pub use string::StringObject;

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
