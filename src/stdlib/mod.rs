use crate::{environment::Value, interpreter::Interpreter};

mod global;
mod list;
mod number;
mod string;

pub use global::GlobalObject;
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
