use crate::{
	ast::ArgumentValues,
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
			"even?" => number_is_even,
			"odd?" => number_is_odd,
			_ => panic!("Undefined method: `{}` for Number object", name),
		}
	}
}

fn number_is_integer(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("Number.integer?", 0, &args, false);

	let number = context.to_number();

	Ok(Value::Bool(number == number.trunc()))
}

fn number_is_float(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("Number.float?", 0, &args, false);

	let number = context.to_number();

	Ok(Value::Bool(number != number.trunc()))
}

fn round_number(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	let number = context.to_number();
	// Fix: Change to new ArgumentValues
	//let precision = if args.is_empty() { 0 } else { args.get(0).unwrap().clone().to_number() as usize };
	let precision = 0; // Temp
	if precision == 0 {
		return Ok(Value::Number(number.trunc()));
	}

	let rounded: f64 = format!("{:.1$}", number, precision).parse().unwrap();

	Ok(Value::Number(rounded))
}

fn number_is_even(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("Number.even?", 0, &args, false);

	let number = context.to_number();
	println!("{}", number);
	println!("{:?}", Value::Bool(number % 2.0 == 0.0));
	Ok(Value::Bool(number % 2.0 == 0.0))
}

fn number_is_odd(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("Number.odd?", 0, &args, false);

	let number = context.to_number();

	Ok(Value::Bool(number % 2.0 == 1.0))
}
