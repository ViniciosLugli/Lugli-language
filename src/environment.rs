use chrono::{DateTime, TimeZone, Utc};
use hashbrown::HashMap;
use std::{
	cell::RefCell,
	fmt::{Debug, Formatter, Result as FmtResult},
	iter::Iterator,
	rc::Rc,
};

use crate::{
	ast::{ArgumentValues, Block, Expression, Parameter},
	interpreter::{Interpreter, InterpreterResult},
};

pub type NativeFunctionCallback = fn(&mut Interpreter, ArgumentValues) -> Value;
pub type NativeMethodCallback = fn(&mut Interpreter, Value, ArgumentValues) -> Result<Value, InterpreterResult>;

#[derive(Debug, Clone)]
pub struct Environment {
	values: HashMap<String, Value>,
}

impl Environment {
	pub fn new() -> Self {
		Self { values: HashMap::new() }
	}

	pub fn set(&mut self, name: impl Into<String>, value: Value) {
		self.values.insert(name.into(), value);
	}

	pub fn get(&self, name: impl Into<String>) -> Option<Value> {
		if let Some(value) = self.values.get(&name.into()) {
			Some(value.clone())
		} else {
			None
		}
	}

	pub fn drop(&mut self, name: impl Into<String>) {
		self.values.remove(&name.into());
	}

	pub fn dump(&self) {
		dbg!(self.values.clone());
	}
}

#[derive(Clone)]
pub enum Value {
	Number(f64),
	String(String),
	Null,
	Bool(bool),
	DateTime(DateTime<Utc>),
	Struct { name: String, fields: Vec<Parameter>, methods: Rc<RefCell<HashMap<String, Value>>>, propreties: Option<Vec<Value>> },
	StructInstance { environment: Rc<RefCell<Environment>>, definition: Box<Value> },
	List(Rc<RefCell<Vec<Value>>>),
	Function { name: String, params: Vec<Parameter>, body: Block, environment: Option<Environment>, context: Option<Expression> },
	NativeFunction { name: String, callback: NativeFunctionCallback },
	NativeMethod { name: String, callback: NativeMethodCallback, context: Expression },
	Constant(Box<Value>),
	Mutable(Box<Value>),
}

impl Debug for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(
			f,
			"{}",
			match self {
				Value::Constant(v) => format!("{:?}", v),
				Value::Number(n) => n.to_string(),
				Value::String(s) => s.to_string(),
				Value::DateTime(dt) => dt.to_string(),
				Value::Null => "null".to_string(),
				Value::NativeFunction { name, .. } | Value::NativeMethod { name, .. } => format!("<{}>", name),
				Value::Mutable(v) => format!("<mutable {:?}>", v),
				Value::Function { name, params, .. } =>
					format!("<{}>({})", name, params.into_iter().map(|p| p.name.clone()).collect::<Vec<String>>().join(", ")),
				Value::StructInstance { definition, .. } => {
					let name = match *definition.clone() {
						Value::Struct { name, .. } => name,
						_ => unreachable!(),
					};

					format!("<{}>", name)
				}
				Value::Struct { name, methods, fields, .. } => {
					let name = format!("<struct:{}>", name);
					let mut fields = fields.into_iter().map(|p| p.name.clone()).collect::<Vec<String>>();
					let mut methods = methods
						.borrow()
						.keys()
						.into_iter()
						.map(|p| {
							let mut p = p.clone();
							p.push_str("()");
							p
						})
						.collect::<Vec<String>>();

					fields.append(&mut methods);

					let fields = format!("{} {{ {} }}", name, fields.join(", "));

					fields
				}
				Value::List(items) => {
					let mut buffer = String::from("[");
					let items = items.borrow();

					for (i, item) in items.iter().enumerate() {
						match &item {
							Value::String(_) => buffer.push_str(&format!("\"{}\"", &item.clone().to_string())),
							_ => buffer.push_str(&item.clone().to_string()),
						}
						if i != items.len() - 1 {
							buffer.push_str(", ");
						}
					}

					buffer.push_str("]");
					buffer
				}
				Value::Bool(true) => "true".to_string(),
				Value::Bool(false) => "false".to_string(),

				_ => todo!(),
			}
		)
	}
}

impl Value {
	pub fn to_vec(self) -> Rc<RefCell<Vec<Value>>> {
		match self {
			Value::List(list) => list,
			Value::Constant(v) => v.to_vec(),
			_ => unreachable!(),
		}
	}

	pub fn to_number(self) -> f64 {
		match self {
			Value::Number(n) => n,
			Value::Bool(true) => 1.0,
			Value::Null | Value::Bool(false) => 0.0,
			Value::String(s) => match s.trim().parse::<f64>() {
				Ok(f) => f,
				Err(_) => 0.0,
			},
			Value::Constant(v) => v.to_number(),
			_ => unreachable!(),
		}
	}

	pub fn to_string(self) -> String {
		match self {
			Value::String(s) => s,
			Value::Number(n) => n.to_string(),
			Value::Bool(_) => {
				if self.to_bool() {
					"true".to_string()
				} else {
					"false".to_string()
				}
			}
			Value::DateTime(dt) => dt.to_rfc3339(),
			Value::Null => "".to_string(),
			v @ Value::Function { .. } | v @ Value::StructInstance { .. } | v @ Value::List(..) => format!("{:?}", v),
			Value::Mutable(v) => format!("<mutable {:?}>", v),
			Value::Constant(v) => v.to_string(),
			Value::NativeFunction { name, .. } | Value::NativeMethod { name, .. } => format!("<{}>", name),
			Value::Struct { name, methods, fields, .. } => {
				let name = format!("<struct:{}>", name);
				let mut fields = fields.into_iter().map(|p| p.name.clone()).collect::<Vec<String>>();
				let mut methods = methods
					.borrow()
					.keys()
					.into_iter()
					.map(|p| {
						let mut p = p.clone();
						p.push_str("()");
						p
					})
					.collect::<Vec<String>>();

				fields.append(&mut methods);

				let fields = format!("{} {{ {} }}", name, fields.join(", "));

				fields
			}
			_ => todo!(),
		}
	}

	pub fn to_bool(self) -> bool {
		match self {
			Value::Bool(true) | Value::Function { .. } => true,
			Value::String(s) => !s.is_empty(),
			Value::Number(n) => n > 0.0,
			Value::Constant(v) => v.to_bool(),
			_ => false,
		}
	}

	pub fn to_datetime(self) -> DateTime<Utc> {
		match self {
			Value::String(s) => {
				let s = s.trim();

				if s.len() == 0 {
					return Utc::now();
				}

				let mut parts = s.split(' ');

				let date = parts.next().unwrap();
				let time = parts.next().unwrap();

				let date = date.split('-').collect::<Vec<&str>>();
				let time = time.split(':').collect::<Vec<&str>>();

				let year = date[0].parse::<i32>().unwrap();
				let month = date[1].parse::<u32>().unwrap();
				let day = date[2].parse::<u32>().unwrap();

				let hour = time[0].parse::<u32>().unwrap();
				let minute = time[1].parse::<u32>().unwrap();
				let second = time[2].parse::<u32>().unwrap();

				Utc.ymd(year, month, day).and_hms(hour, minute, second)
			}
			Value::Number(n) => Utc.timestamp(n as i64, 0),
			Value::Constant(v) => v.to_datetime(),
			Value::DateTime(dt) => dt,
			_ => unreachable!(),
		}
	}

	pub fn is(self, other: Value) -> bool {
		match (self, other.clone()) {
			(Value::String(l), r) => l == r.to_string(),
			(Value::Number(n), r) => n == r.to_number(),
			(Value::Bool(true), r) => r.to_bool() == true,
			(Value::Bool(false), r) => r.to_bool() == false,
			(Value::DateTime(dt), r) => dt == r.to_datetime(),
			(Value::Null, Value::Null) => true,
			(Value::Constant(v), _) => v.is(other),
			_ => false,
		}
	}

	pub fn typestring(self) -> String {
		match self {
			Value::String(..) => "string".into(),
			Value::Number(..) => "number".into(),
			Value::Bool(..) => "bool".into(),
			Value::DateTime(..) => "datetime".into(),
			Value::Null => "null".into(),
			Value::Function { .. } | Value::NativeFunction { .. } | Value::NativeMethod { .. } => "function".into(),
			Value::StructInstance { definition, .. } => match *definition.clone() {
				Value::Struct { name, .. } => name,
				_ => unreachable!(),
			},
			Value::Struct { .. } => "struct".into(),
			Value::List(..) => "list".into(),
			Value::Constant(v) => v.typestring(),
			_ => unreachable!(),
		}
	}
}
