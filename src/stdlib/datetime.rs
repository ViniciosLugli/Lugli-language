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

	pub fn getter_property(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"nanoseconds" => getters::datetime_nanoseconds,
			"seconds" => getters::datetime_seconds,
			"minutes" => getters::datetime_minutes,
			"hours" => getters::datetime_hours,
			"days" => getters::datetime_days,
			"weeks" => getters::datetime_weeks,
			"months" => getters::datetime_months,
			"years" => getters::datetime_years,
			_ => panic!("Undefined property: `{}` for DateTime object", name),
		}
	}

	pub fn setter_property(name: String) -> NativeMethodCallback {
		match name.as_str() {
			"nanoseconds" => setters::datetime_nanoseconds,
			"seconds" => setters::datetime_seconds,
			"minutes" => setters::datetime_minutes,
			"hours" => setters::datetime_hours,
			"days" => setters::datetime_days,
			"months" => setters::datetime_months,
			"years" => setters::datetime_years,
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

mod getters {
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

mod setters {
	use chrono::{Datelike, Timelike};

	use crate::{
		ast::ArgumentValues,
		environment::Value,
		interpreter::{Interpreter, InterpreterResult},
	};

	pub fn datetime_seconds(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.seconds", 1, &args, true);

		let datetime = context.to_datetime();
		let seconds = args.get_from_name_or_index("seconds".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_second(seconds as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set seconds for DateTime: {}", seconds)))
		}
	}

	pub fn datetime_minutes(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.minutes", 1, &args, true);

		let datetime = context.to_datetime();
		let minutes = args.get_from_name_or_index("minutes".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_minute(minutes as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set minutes for DateTime: {}", minutes)))
		}
	}

	pub fn datetime_hours(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.hours", 1, &args, true);

		let datetime = context.to_datetime();
		let hours = args.get_from_name_or_index("hours".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_hour(hours as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set hours for DateTime: {}", hours)))
		}
	}

	pub fn datetime_nanoseconds(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.nanoseconds", 1, &args, true);

		let datetime = context.to_datetime();
		let nanoseconds = args.get_from_name_or_index("nanoseconds".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_nanosecond(nanoseconds as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set nanoseconds for DateTime: {}", nanoseconds)))
		}
	}

	pub fn datetime_years(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.years", 1, &args, true);

		let datetime = context.to_datetime();
		let years = args.get_from_name_or_index("years".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_year(years as i32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set years for DateTime: {}", years)))
		}
	}

	pub fn datetime_months(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.months", 1, &args, true);

		let datetime = context.to_datetime();
		let months = args.get_from_name_or_index("months".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_month(months as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set months for DateTime: {}", months)))
		}
	}

	pub fn datetime_days(_: &mut Interpreter, context: Value, args: ArgumentValues) -> Result<Value, InterpreterResult> {
		super::arity("DateTime.days", 1, &args, true);

		let datetime = context.to_datetime();
		let days = args.get_from_name_or_index("days".to_string(), 0).unwrap().to_number();
		if let Some(result) = datetime.with_day(days as u32) {
			Ok(Value::DateTime(result))
		} else {
			Err(InterpreterResult::Error(format!("Invalid set days for DateTime: {}", days)))
		}
	}
}
