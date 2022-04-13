use std::{cell::RefCell, rc::Rc};

use crate::{
	ast::{ArgumentValued, ArgumentValues},
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct ListObject;

impl ListObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"empty?" => list_is_empty,
			"notEmpty?" => list_is_not_empty,
			"reverse!" => list_reverse,
			"join!" => list_join,
			"filter!" => list_filter,
			"each!" => list_each,
			"map!" => list_map,
			"first!" => list_first,
			"push!" => list_push,
			_ => panic!("Undefined method: {} for List Object", name),
		}
	}
}

fn list_is_empty(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.empty?()", 0, &args, false);

	Ok(Value::Bool(context.to_vec().borrow().is_empty()))
}

fn list_is_not_empty(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.notEmpty?()", 0, &args, false);

	Ok(Value::Bool(!context.to_vec().borrow().is_empty()))
}

fn list_reverse(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.reverse!()", 0, &args, false);

	let mut list = context.to_vec().borrow().clone();
	list.reverse();

	Ok(Value::List(Rc::new(RefCell::new(list))))
}

fn list_join(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.join!()", 1, &args, false);

	let list = context.to_vec().borrow().clone();
	let separator = args.get_from_name_or_index("list".to_string(), 0).unwrap().to_string();
	let result = list.into_iter().map(|a| a.to_string()).collect::<Vec<String>>().join(&separator);

	Ok(Value::String(result))
}

fn list_filter(interpreter: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.filter!()", 1, &args, false);

	let mut callback = args.get_from_name_or_index("callback".to_string(), 0).unwrap();
	callback = super::parse_callback(callback);

	let mut new_list = Vec::new();

	for item in context.to_vec().borrow().clone().into_iter() {
		let mut args = ArgumentValues::new();
		args.push(ArgumentValued::new(None, item.clone()));

		if interpreter.call(callback.clone(), args)?.to_bool() {
			new_list.push(item);
		}
	}

	Ok(Value::List(Rc::new(RefCell::new(new_list))))
}

fn list_each(interpreter: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.each!()", 1, &args, false);

	let mut callback = args.get_from_name_or_index("callback".to_string(), 0).unwrap();
	callback = super::parse_callback(callback);

	for v in context.clone().to_vec().borrow().iter() {
		let mut args = ArgumentValues::new();
		args.push(ArgumentValued::new(None, v.clone()));

		interpreter.call(callback.clone(), args)?.to_bool();
	}

	Ok(context)
}

fn list_map(interpreter: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.map!()", 1, &args, false);

	let mut callback = args.get_from_name_or_index("callback".to_string(), 0).unwrap();
	callback = super::parse_callback(callback);

	let mut list = context.clone().to_vec().borrow().clone();

	for (i, v) in list.clone().iter().enumerate() {
		let mut args = ArgumentValues::new();
		args.push(ArgumentValued::new(None, v.clone()));

		let result = interpreter.call(callback.clone(), args)?;

		list[i] = result;
	}

	Ok(Value::List(Rc::new(RefCell::new(list))))
}

fn list_first(interpreter: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	let list = context.clone().to_vec().borrow().clone();

	if list.is_empty() {
		return Ok(Value::Null);
	}

	if args.len() == 1 {
		let mut callback = args.get_from_name_or_index("callback".to_string(), 0).unwrap();
		callback = super::parse_callback(callback);

		for v in list.iter() {
			let mut args = ArgumentValues::new();
			args.push(ArgumentValued::new(None, v.clone()));

			let result = interpreter.call(callback.clone(), args)?;

			if result.clone().to_bool() {
				return Ok(v.clone());
			}
		}
	}

	Ok(list.first().unwrap().clone())
}

fn list_push(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
	super::arity("List.push!()", 1, &args, false);

	let mut list = context.clone().to_vec().borrow().clone();
	let new_item = args.get_from_name_or_index("item".to_string(), 0).unwrap();

	list.push(new_item);

	Ok(Value::Mutable(Box::new(Value::List(Rc::new(RefCell::new(list))))))
}
