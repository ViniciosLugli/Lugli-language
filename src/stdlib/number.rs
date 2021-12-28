use crate::{
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct NumberObject;

impl NumberObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"integer?" => number_is_integer,
			"float?" => number_is_float,
			"round!" => round_number,
			_ => panic!("Undefined method: `{}` for Number object", name),
		}
	}
}

fn number_is_integer(_: &mut Interpreter, context: Value, arguments: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("Number.integer?", 0, &arguments);

	let number = context.to_number();

	Ok(Value::Bool(number == number.trunc()))
}

fn number_is_float(_: &mut Interpreter, context: Value, arguments: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("Number.float?", 0, &arguments);

	let number = context.to_number();

	Ok(Value::Bool(number != number.trunc()))
}

fn round_number(_: &mut Interpreter, context: Value, arguments: Vec<Value>) -> Result<Value, InterpreterResult> {
	let number = context.to_number();
	let precision = if arguments.is_empty() { 0 } else { arguments.get(0).unwrap().clone().to_number() as usize };

	if precision == 0 {
		return Ok(Value::Number(number.trunc()));
	}

	let rounded: f64 = format!("{:.1$}", number, precision).parse().unwrap();

	Ok(Value::Number(rounded))
}
