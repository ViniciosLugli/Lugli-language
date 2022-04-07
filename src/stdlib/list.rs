use std::{cell::RefCell, rc::Rc};

use crate::{
	ast::CallArguments,
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct ListObject;

// FIXME: Change all file to new CallArguments

impl ListObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			//"empty?" => list_is_empty,
			//"notEmpty?" => list_is_not_empty,
			//"reverse!" => list_reverse,
			//"join!" => list_join,
			//"filter!" => list_filter,
			//"each!" => list_each,
			//"map!" => list_map,
			//"first!" => list_first,
			_ => panic!("Undefined method: {} for List Object", name),
		}
	}
}

//fn list_is_empty(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.empty?()", 0, &args, false);

//	Ok(Value::Bool(context.to_vec().borrow().is_empty()))
//}

//fn list_is_not_empty(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.notEmpty?()", 0, &args, false);

//	Ok(Value::Bool(!context.to_vec().borrow().is_empty()))
//}

//fn list_reverse(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.reverse!()", 0, &args, false);

//	let mut list = context.to_vec().borrow().clone();
//	list.reverse();

//	Ok(Value::List(Rc::new(RefCell::new(list))))
//}

//fn list_join(_: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.join!()", 1, &args, false);

//	let list = context.to_vec().borrow().clone();
//	let separator = args.get(0).unwrap().clone().to_string();
//	let result = list.into_iter().map(|a| a.to_string()).collect::<Vec<String>>().join(&separator);

//	Ok(Value::String(result))
//}

//fn list_filter(interpreter: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.filter!()", 1, &args, false);

//	let callback = args.get(0).unwrap().clone();
//	let mut new_list: CallArguments = Vec::new();

//	for item in context.to_vec().borrow().clone().into_iter() {
//		if interpreter.call(callback.clone(), vec![item.clone()])?.to_bool() {
//			new_list.push(item);
//		}
//	}

//	Ok(Value::List(Rc::new(RefCell::new(new_list))))
//}

//fn list_each(interpreter: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.each!()", 1, &args, false);

//	let callback = args.get(0).unwrap().clone();

//	for v in context.clone().to_vec().borrow().iter() {
//		interpreter.call(callback.clone(), vec![v.clone()])?.to_bool();
//	}

//	Ok(context)
//}

//fn list_map(interpreter: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	super::arity("List.map!()", 1, &args, false);

//	let callback = args.get(0).unwrap().clone();
//	let mut list = context.clone().to_vec().borrow().clone();

//	for (i, v) in list.clone().iter().enumerate() {
//		let result = interpreter.call(callback.clone(), vec![v.clone()])?;

//		list[i] = result;
//	}

//	Ok(Value::List(Rc::new(RefCell::new(list))))
//}

//fn list_first(interpreter: &mut Interpreter, context: Value, args: CallArguments) -> Result<Value, InterpreterResult> {
//	let list = context.clone().to_vec().borrow().clone();

//	if list.is_empty() {
//		return Ok(Value::Null);
//	}

//	if args.len() == 1 {
//		let callback = args.get(0).unwrap().clone();

//		for v in list.iter() {
//			let result = interpreter.call(callback.clone(), vec![v.clone()])?;

//			if result.clone().to_bool() {
//				return Ok(v.clone());
//			}
//		}
//	}

//	Ok(list.first().unwrap().clone())
//}
