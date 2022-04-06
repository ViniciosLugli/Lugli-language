use crate::{
	ast::CallArguments,
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct StringObject;
// FIX: Change all file to new CallArguments
impl StringObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			//"contains?" => string_contains,
			//"startsWith?" => string_starts_with,
			//"endsWith?" => string_ends_with,
			//"finish!" => string_finish,
			//"append!" => string_append,
			//"tap" => string_tap,
			//"upper!" => string_to_upper,
			//"lower!" => string_to_lower,
			_ => panic!("Undefined method: `{}` for String object", name),
		}
	}
}

//fn string_contains(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.contains?", 1, &args, false);

//	let string = context.to_string();

//	for argument in args {
//		if string.contains(&argument.to_string()) {
//			return Ok(Value::Bool(true));
//		}
//	}

//	Ok(Value::Bool(false))
//}

//fn string_starts_with(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.startsWith?", 1, &args, false);

//	let string = context.to_string();

//	for argument in args {
//		if string.starts_with(&argument.to_string()) {
//			return Ok(Value::Bool(true));
//		}
//	}

//	Ok(Value::Bool(false))
//}

//fn string_ends_with(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.endsWith?", 1, &args, false);

//	let string = context.to_string();

//	for argument in args {
//		if string.ends_with(&argument.to_string()) {
//			return Ok(Value::Bool(true));
//		}
//	}

//	Ok(Value::Bool(false))
//}

//fn string_finish(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.finish!", 1, &args, false);

//	let mut string = context.to_string();
//	let append = args[0].clone().to_string();

//	if !string.ends_with(&append) {
//		string.push_str(append.as_str());
//	}

//	Ok(Value::String(string))
//}

//fn string_append(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.append!", 1, &args, false);

//	let mut string = context.to_string();
//	let append = args[0].clone().to_string();

//	string.push_str(append.as_str());

//	Ok(Value::String(string))
//}

//fn string_tap(interpreter: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.tap", 1, &args, false);

//	let string = context.clone();

//	// TODO: Add some better error handling here. Maybe check that
//	let callback = match args.get(0) {
//		Some(f) => f.clone(),
//		_ => unreachable!(),
//	};

//	interpreter.call(callback, vec![string])?;

//	Ok(context)
//}

//fn string_to_upper(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.upper!", 0, &args, false);

//	Ok(Value::String(context.to_string().to_uppercase()))
//}

//fn string_to_lower(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("String.lower!", 0, &args, false);

//	Ok(Value::String(context.to_string().to_lowercase()))
//}
