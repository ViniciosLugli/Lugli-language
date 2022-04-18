use crate::{
	ast::{ArgumentValued, ArgumentValues},
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct StringObject;

impl StringObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"contains?" => string_contains,
			"startsWith?" => string_starts_with,
			"endsWith?" => string_ends_with,
			"finish!" => string_finish,
			"append!" => string_append,
			"tap" => string_tap,
			"upper!" => string_to_upper,
			"lower!" => string_to_lower,
			"length?" => string_length,
			_ => panic!("Undefined method: `{}` for String object", name),
		}
	}
}

fn string_contains(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.contains?", 1, &args, false);

	let string = context.to_string();

	for argument in args.get_all_values() {
		if string.contains(&argument.to_string()) {
			return Ok(Value::Bool(true));
		}
	}

	Ok(Value::Bool(false))
}

fn string_starts_with(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.startsWith?", 1, &args, false);

	let string = context.to_string();

	for argument in args.get_all_values() {
		if string.starts_with(&argument.to_string()) {
			return Ok(Value::Bool(true));
		}
	}

	Ok(Value::Bool(false))
}

fn string_ends_with(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.endsWith?", 1, &args, false);

	let string = context.to_string();

	for argument in args.get_all_values() {
		if string.ends_with(&argument.to_string()) {
			return Ok(Value::Bool(true));
		}
	}

	Ok(Value::Bool(false))
}

fn string_finish(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.finish!", 1, &args, false);

	let mut string = context.to_string();
	let append = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();

	if !string.ends_with(&append) {
		string.push_str(append.as_str());
	}

	Ok(Value::String(string))
}

fn string_append(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.append!", 1, &args, false);

	let mut string = context.to_string();
	let append = args.get_from_name_or_index("content".to_string(), 0).unwrap().to_string();

	string.push_str(append.as_str());

	Ok(Value::String(string))
}

fn string_tap(interpreter: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.tap", 1, &args, false);

	let string = context.clone();

	let mut callback = args.get_from_name_or_index("callback".to_string(), 0).unwrap();

	callback = super::parse_callback(callback);

	let mut arguments_values = ArgumentValues::new();
	arguments_values.push(ArgumentValued::new(None, string));

	interpreter.call(callback, arguments_values)?;

	Ok(context)
}

fn string_to_upper(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.upper!", 0, &args, false);

	Ok(Value::String(context.to_string().to_uppercase()))
}

fn string_to_lower(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.lower!", 0, &args, false);

	Ok(Value::String(context.to_string().to_lowercase()))
}

fn string_length(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("String.length?", 0, &args, false);

	Ok(Value::Number(context.to_string().len() as f64))
}
