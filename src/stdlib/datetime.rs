use super::arity;
use crate::environment::NativeMethodCallback;
pub struct DateTimeObject;

impl DateTimeObject {
	pub fn get_method(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"format!" => methods::datetime_format,
			"strweekday?" => methods::datetime_strweekday,
			_ => panic!("Undefined method: `{}` for DateTime object", name),
		}
	}

	pub fn get_property(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"nanoseconds" => gets::datetime_nanoseconds,
			"seconds" => gets::datetime_seconds,
			"minutes" => gets::datetime_minutes,
			"hours" => gets::datetime_hours,
			"days" => gets::datetime_days,
			"weeks" => gets::datetime_weeks,
			"months" => gets::datetime_months,
			"years" => gets::datetime_years,
			_ => panic!("Undefined property: `{}` for DateTime object", name),
		}
	}

	pub fn set_property(name: String) -> NativeMethodCallback {
		match name.as_str() {
			_ => panic!("Undefined property: `{}` for DateTime object", name),
		}
	}
}

mod methods {
	use crate::{
		ast::ArgumentValues,
		environment::Value,
		interpreter::{Interpreter, InterpreterResult},
	};
	use chrono::Datelike;

	pub fn datetime_format(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.format!", 1, &args, false);

		let datetime = context.to_datetime();
		let format = args.get_from_name_or_index("format".to_string(), 0).unwrap().to_string();

		Ok(Value::String(datetime.format(&format).to_string()))
	}

	pub fn datetime_strweekday(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.strWeekday?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::String(datetime.weekday().to_string()))
	}
}

mod gets {
	use crate::{
		ast::ArgumentValues,
		environment::Value,
		interpreter::{Interpreter, InterpreterResult},
	};
	use chrono::{Datelike, Timelike};

	pub fn datetime_hours(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.hour?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.hour() as f64))
	}

	pub fn datetime_minutes(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.minute?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.minute() as f64))
	}

	pub fn datetime_seconds(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.seconds", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.second() as f64))
	}

	pub fn datetime_nanoseconds(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.nanosecond?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.nanosecond() as f64))
	}

	pub fn datetime_years(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.year?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.year() as f64))
	}

	pub fn datetime_months(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.month?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.month() as f64))
	}

	pub fn datetime_days(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.day?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.day() as f64))
	}

	pub fn datetime_weeks(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.weekday?", 0, &args, false);

		let datetime = context.to_datetime();

		Ok(Value::Number(datetime.weekday().num_days_from_sunday() as f64))
	}
}
