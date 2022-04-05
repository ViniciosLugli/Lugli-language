use chrono::{Datelike, Timelike};

use crate::{
	environment::{NativeMethodCallback, Value},
	interpreter::{Interpreter, InterpreterResult},
};

pub struct DateTimeObject;

impl DateTimeObject {
	pub fn get(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"format!" => datetime_format,
			"hour?" => datetime_hour,
			"minute?" => datetime_minute,
			"second?" => datetime_second,
			"year?" => datetime_year,
			"month?" => datetime_month,
			"day?" => datetime_day,
			"weekday?" => datetime_weekday,
			"strweekday?" => datetime_strweekday,
			_ => panic!("Undefined method: `{}` for DateTime object", name),
		}
	}
}

fn datetime_format(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.format!", 1, &args, false);

	let datetime = context.to_datetime();
	let format = args.get(0).unwrap().clone().to_string();

	Ok(Value::String(datetime.format(&format).to_string()))
}

fn datetime_hour(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.hour?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.hour() as f64))
}

fn datetime_minute(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.minute?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.minute() as f64))
}

fn datetime_second(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.second?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.second() as f64))
}

fn datetime_nanosecond(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.nanosecond?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.nanosecond() as f64))
}

fn datetime_year(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.year?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.year() as f64))
}

fn datetime_month(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.month?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.month() as f64))
}

fn datetime_day(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.day?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.day() as f64))
}

fn datetime_weekday(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.weekday?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::Number(datetime.weekday().num_days_from_sunday() as f64))
}

fn datetime_strweekday(_: &mut Interpreter, context: Value, args: Vec<Value>) -> Result<Value, InterpreterResult> {
	super::arity("DateTime.strWeekday?", 0, &args, false);

	let datetime = context.to_datetime();

	Ok(Value::String(datetime.weekday().to_string()))
}
