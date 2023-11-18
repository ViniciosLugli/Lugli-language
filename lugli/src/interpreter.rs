use crate::{
	ast::{ArgumentValued, ArgumentValues, Expression, Op, Parameter, Program, Statement},
	environment::{self, Environment, NativeFunctionCallback, Value},
};
use colored::Colorize;
use hashbrown::HashMap;
use std::{
	cell::{Ref, RefCell, RefMut},
	fs::canonicalize,
	path::PathBuf,
	rc::Rc,
	slice::Iter,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum InterpreterResult {
	#[error("")]
	Return(Value),

	#[error("")]
	Break,

	#[error("")]
	Continue,

	#[error("{0}")]
	Error(String),

	#[error("Undefined variable: {0}.")]
	UndefinedVariable(String),

	#[error("Undefined index: {0}.")]
	UndefinedIndex(usize),

	#[error("Undefined field: {0}.{1}")]
	UndefinedField(String, String),

	#[error("Undefined method: {0}.{1}()")]
	UndefinedMethod(String, String),

	#[error("Unable to iterate over value of type {0}.")]
	InvalidIterable(String),

	#[error("Too few arguments to function {0}(), {1} passed in, {2} expected.")]
	TooFewArguments(String, usize, usize),

	#[error("Cannot append to value of type {0}.")]
	InvalidAppendTarget(String),

	#[error("Cannot assign method to static property of type {0}.")]
	InvalidMethodAssignmentTarget(String),

	#[error("Cannot assign value to constant.")]
	CannotAssignValueToConstant,
}

impl InterpreterResult {
	pub fn print(self) {
		eprintln!("{}", format!("{}", self).red().bold());
		std::process::exit(1);
	}
}

#[derive(Debug, Clone)]
pub struct Interpreter<'i> {
	ast: Iter<'i, Statement>,
	environment: Rc<RefCell<Environment>>,
	pub globals: HashMap<String, Value>,
	path: PathBuf,
}

impl<'i> Interpreter<'i> {
	pub fn new(ast: Iter<'i, Statement>, path: PathBuf) -> Self {
		Self { ast, environment: Rc::new(RefCell::new(Environment::new())), globals: HashMap::new(), path }
	}

	fn run_statement(&mut self, statement: Statement) -> Result<(), InterpreterResult> {
		Ok(match statement {
			Statement::VariableDeclaration { name, initial } => {
				if initial.is_none() {
					self.env_mut().set(name, Value::Null)
				} else {
					let initial = initial.unwrap();
					let value = self.run_expression(initial)?;

					self.env_mut().set(name, value)
				}
			}
			Statement::ConstantDeclaration { name, initial } => {
				let value = Value::Constant(Box::new(self.run_expression(initial)?));

				self.env_mut().set(name, value)
			}
			Statement::FunctionDeclaration { name, params, body } => {
				self.globals.insert(name.clone(), Value::Function { name, params, body, environment: None, context: None });
			}
			Statement::ClassDeclaration { name, fields } => {
				let methods: Rc<RefCell<hashbrown::HashMap<String, environment::Value>>> = Rc::new(RefCell::new(hashbrown::HashMap::new()));
				let mut fields_filtred: Vec<Parameter> = Vec::new();
				for field in fields.clone() {
					match field.clone().default {
						Some(e) => match e {
							Expression::Closure(params, body) => {
								methods.borrow_mut().insert(
									field.name.clone(),
									Value::Function {
										name: field.name.clone(),
										params,
										body,
										environment: Some(self.environment.borrow().clone()),
										context: None,
									},
								);
							}
							_ => {
								fields_filtred.push(field);
							}
						},
						None => fields_filtred.push(field),
					}
				}

				self.globals.insert(name.clone(), Value::Struct { name, fields: fields_filtred, methods, propreties: None });
			}
			Statement::For { iterable, value, index, then } => {
				let iterable = self.run_expression(iterable)?;

				let items = match iterable {
					Value::List(items) => items,
					_ => return Err(InterpreterResult::InvalidIterable(iterable.typestring())),
				};

				// If there aren't any items in the list, we can leave this execution
				// cycle early.
				if items.borrow().is_empty() {
					return Ok(());
				}

				let set_index: bool = index.is_some();

				'outer_for: for (i, item) in items.borrow().iter().enumerate() {
					self.env_mut().set(value.clone(), item.clone());

					if set_index {
						self.env_mut().set(index.clone().unwrap(), Value::Number(i as f64));
					}

					for statement in then.clone() {
						match self.run_statement(statement) {
							Err(InterpreterResult::Break) => break 'outer_for,
							Err(InterpreterResult::Continue) => break,
							Err(err) => return Err(err),
							_ => (),
						}
					}
				}

				self.env_mut().drop(value);

				if set_index {
					self.env_mut().drop(index.unwrap());
				}
			}

			Statement::While { condition } => {
				'outer_while: while self.run_expression(condition.expression.clone())?.to_bool() {
					for statement in condition.then.clone() {
						match self.run_statement(statement) {
							Err(InterpreterResult::Break) => break 'outer_while,
							Err(InterpreterResult::Continue) => break,
							Err(err) => return Err(err),
							_ => (),
						}
					}
				}
			}

			Statement::Loop { body } => 'outer_loop: loop {
				for statement in body.clone() {
					match self.run_statement(statement) {
						Err(InterpreterResult::Break) => break 'outer_loop,
						Err(InterpreterResult::Continue) => break,
						Err(err) => return Err(err),
						_ => (),
					}
				}
			},

			Statement::If { condition, others_conditions, otherwise } => {
				let expression = self.run_expression(condition.expression)?;
				let mut satisfied = false;

				if expression.to_bool() {
					satisfied = true;

					for statement in condition.then {
						self.run_statement(statement)?;
					}
				} else if let Some(conditions_blocks) = others_conditions {
					for condition_block in conditions_blocks {
						let expression_result = self.run_expression(condition_block.expression)?;

						if expression_result.to_bool() {
							satisfied = true;

							for statement in condition_block.then {
								self.run_statement(statement)?;
							}

							break;
						}
					}
				}

				if otherwise.is_some() && !satisfied {
					for statement in otherwise.unwrap() {
						self.run_statement(statement)?;
					}
				}
			}

			Statement::Expression { expression } => {
				self.run_expression(expression)?;
			}
			Statement::Return { value } => {
				return Err(InterpreterResult::Return(self.run_expression(value)?));
			}
			Statement::Break => {
				return Err(InterpreterResult::Break);
			}
			Statement::Continue => {
				return Err(InterpreterResult::Continue);
			}
			_ => todo!("{:?}", statement),
		})
	}

	pub fn call(&mut self, callable: Value, arguments: ArgumentValues) -> Result<Value, InterpreterResult> {
		Ok(match callable {
			Value::Constant(v) => self.call(*v, arguments)?,
			Value::NativeFunction { callback, .. } => callback(self, arguments),
			Value::NativeMethod { callback, context, .. } => {
				let context = self.run_expression(context)?;

				callback(self, context, arguments)?
			}
			Value::Function { name, mut params, body, environment, context } => {
				let old_environment = Rc::clone(&self.environment);

				let new_environment =
					if environment.is_some() { Rc::new(RefCell::new(environment.unwrap())) } else { Rc::new(RefCell::new(Environment::new())) };

				if context.is_some() && params.first() == Some(&Parameter { name: "this".to_string(), default: None }) {
					let context = self.run_expression(context.unwrap())?;
					new_environment.borrow_mut().set("this", context);
					params = params.iter().filter(|p| p.name != "this").cloned().collect();
				}

				let mut params_to_satisfy = params.clone();

				for argument in arguments.clone() {
					if argument.get_name().is_some() && params_to_satisfy.iter().any(|param| param.get_name() == argument.get_name().unwrap()) {
						params_to_satisfy.retain(|param| param.get_name() != argument.get_name().unwrap());
					}
				}

				let params_without_value = params_to_satisfy.iter().filter(|param| !param.has_default()).count();

				if params_without_value > arguments.len() {
					return Err(InterpreterResult::TooFewArguments(name.clone(), arguments.len(), params_without_value));
				}

				for params_with_initial in params.iter().filter(|param| param.has_default()) {
					let initial = self.run_expression(params_with_initial.get_default().unwrap())?;
					new_environment.borrow_mut().set(params_with_initial.get_name(), initial);
				}

				for argument in arguments.clone().filter(|arg| arg.get_name().is_some()) {
					new_environment.borrow_mut().set(argument.get_name().unwrap(), argument.get_value());
				}
				for (param, ArgumentValued { value, .. }) in params_to_satisfy.iter().zip(arguments.filter(|param| param.get_name().is_none())) {
					new_environment.borrow_mut().set(param.get_name(), value);
				}

				self.environment = new_environment;

				let mut return_value: Option<Value> = None;

				for statement in body {
					match self.run_statement(statement) {
						Err(InterpreterResult::Return(value)) => {
							return_value = Some(value);
							break;
						}
						Err(err) => return Err(err),
						_ => (),
					};
				}

				self.environment = old_environment;

				if return_value.is_some() {
					return_value.unwrap()
				} else {
					Value::Null
				}
			}
			_ => todo!(),
		})
	}

	fn run_expression(&mut self, expression: Expression) -> Result<Value, InterpreterResult> {
		Ok(match expression.clone() {
			Expression::Number(n) => Value::Number(n),
			Expression::String(s) => Value::String(s),
			Expression::Bool(b) => Value::Bool(b),
			Expression::Identifier(n) => {
				if self.globals.contains_key(&n) {
					self.globals[&n].clone()
				} else {
					if let Some(v) = self.env().get(n.clone()) {
						v
					} else {
						return Err(InterpreterResult::UndefinedVariable(n));
					}
				}
			}
			Expression::Index(target, index) => {
				let instance = self.run_expression(*target)?;
				let index = self.run_expression(*index.expect("Expected index."))?.to_number() as usize;

				match instance {
					Value::List(items) => match items.borrow().get(index) {
						Some(v) => v.clone(),
						None => return Err(InterpreterResult::UndefinedIndex(index)),
					},
					_ => unreachable!(),
				}
			}

			Expression::Infix(left, op, right) => {
				let left = self.run_expression(*left)?;
				let right = self.run_expression(*right)?;

				match (left, op, right) {
					(Value::Number(l), Op::Add, Value::Number(r)) => Value::Number(l + r),
					(Value::Number(l), Op::Multiply, Value::Number(r)) => Value::Number(l * r),
					(Value::Number(l), Op::Divide, Value::Number(r)) => Value::Number(l / r),
					(Value::Number(l), Op::Subtract, Value::Number(r)) => Value::Number(l - r),
					(Value::Number(l), Op::Add, Value::String(r)) => {
						let mut l = l.to_string();
						l.push_str(r.as_str());
						Value::String(l)
					}
					(Value::String(l), Op::Add, Value::Number(r)) => {
						let mut l = l;
						l.push_str(r.to_string().as_str());
						Value::String(l)
					}
					(Value::String(l), Op::Add, Value::String(r)) => {
						let mut l = l;
						l.push_str(r.as_str());
						Value::String(l)
					}
					(Value::Number(l), Op::Module, Value::Number(r)) => Value::Number(l % r),
					(Value::String(l), Op::Equals, Value::String(r)) => Value::Bool(l == r),
					(Value::Number(l), Op::Equals, Value::Number(r)) => Value::Bool(l == r),
					(Value::Bool(l), Op::Equals, Value::Bool(r)) => Value::Bool(l == r),
					(Value::String(l), Op::NotEquals, Value::String(r)) => Value::Bool(l != r),
					(Value::Number(l), Op::NotEquals, Value::Number(r)) => Value::Bool(l != r),
					(Value::Bool(l), Op::NotEquals, Value::Bool(r)) => Value::Bool(l != r),
					(Value::Number(l), Op::LessThan, Value::Number(r)) => Value::Bool(l < r),
					(Value::Number(l), Op::GreaterThan, Value::Number(r)) => Value::Bool(l > r),
					(Value::Number(l), Op::LessThanOrEquals, Value::Number(r)) => Value::Bool(l <= r),
					(Value::Number(l), Op::GreaterThanOrEquals, Value::Number(r)) => Value::Bool(l >= r),
					(l, Op::And, r) => Value::Bool(l.to_bool() && r.to_bool()),
					(l, Op::Or, r) => Value::Bool(l.to_bool() || r.to_bool()),
					(Value::Number(l), Op::Pow, Value::Number(r)) => Value::Number(l.powf(r)),
					(l, Op::In, Value::List(r)) => {
						let filtered: Vec<Value> = r.borrow().clone().into_iter().filter(|v| v.clone().is(l.clone())).collect();

						Value::Bool(!filtered.is_empty())
					}
					(Value::String(l), Op::In, Value::String(r)) => Value::Bool(r.contains(l.as_str())),
					(l, Op::NotIn, Value::List(r)) => {
						let filtered: Vec<Value> = r.borrow().clone().into_iter().filter(|v| v.clone().is(l.clone())).collect();

						Value::Bool(filtered.is_empty())
					}
					(Value::String(l), Op::NotIn, Value::String(r)) => Value::Bool(!r.contains(l.as_str())),
					_ => todo!(),
				}
			}
			Expression::List(items) => {
				let mut values: Vec<Value> = Vec::new();

				for item in items.into_iter() {
					values.push(self.run_expression(item)?);
				}

				Value::List(Rc::new(RefCell::new(values)))
			}
			Expression::Closure(params, body) => {
				Value::Function { name: String::from("Closure"), params, body, environment: Some(self.environment.borrow().clone()), context: None }
			}
			Expression::Class(definition, fields) => {
				let definition = self.run_expression(*definition)?;

				let (name, field_definitions, methods) = match definition.clone() {
					Value::Struct { name, fields, methods, .. } => (name, fields, methods),
					_ => unreachable!(),
				};

				let mut environment = Environment::new();

				for parameter in field_definitions.iter().find(|param| param.has_default()) {
					let value = self.run_expression(parameter.get_default().unwrap())?;

					environment.set(parameter.get_name(), value);
				}

				for (field, value) in fields {
					if !field_definitions.iter().any(|f| f.name == field) {
						return Err(InterpreterResult::UndefinedField(name, field.clone()));
					}

					let value = self.run_expression(value)?;

					environment.set(
						field,
						match value {
							Value::ClassInstance { environment, definition } => {
								let environment = environment.borrow().clone();

								Value::ClassInstance { definition, environment: Rc::new(RefCell::new(environment)) }
							}
							_ => value,
						},
					);
				}

				let environment = Rc::new(RefCell::new(environment));

				for (name, method) in methods.borrow().clone() {
					let method = match method {
						Value::Function { name, body, params, .. } => Value::Function { name, params, body, environment: None, context: None },
						_ => unreachable!(),
					};

					environment.borrow_mut().set(name, method);
				}

				Value::ClassInstance { environment, definition: Box::new(definition) }
			}
			Expression::Call(callable, arguments) => {
				let callable = self.run_expression(*callable)?;

				let mut arguments_value = ArgumentValues::new();

				for argument in arguments.get_arguments().clone() {
					arguments_value.push(ArgumentValued::new(argument.get_name().clone(), self.run_expression(argument.get_expression().clone())?));
				}

				self.call(callable, arguments_value)?
			}
			Expression::Prefix(op, right) => {
				let right = self.run_expression(*right)?;

				match op {
					Op::Bang => Value::Bool(!right.to_bool()),
					Op::Subtract => Value::Number(-right.to_number()),
					_ => unreachable!(),
				}
			}

			Expression::MathAssign(target, op, value) => {
				let target_expr = self.run_expression(*target.clone())?;
				let value = self.run_expression(*value)?;

				match target_expr.clone() {
					Value::Number(n) => {
						match *target.clone() {
							Expression::Identifier(i) => {
								self.env_mut().set(
									i,
									Value::Number(match op {
										Op::Add => n + value.clone().to_number(),
										Op::Subtract => n - value.clone().to_number(),
										Op::Multiply => n * value.clone().to_number(),
										Op::Divide => n / value.clone().to_number(),
										_ => unreachable!(),
									}),
								);
							}
							_ => unimplemented!(),
						}

						Value::Number(n + value.to_number())
					}
					_ => unreachable!(),
				}
			}

			Expression::Assign(target, value) => {
				let value = self.run_expression(*value)?;

				fn assign_to_list(
					interpreter: &mut Interpreter,
					instance: Value,
					index: Option<Box<Expression>>,
					value: Value,
				) -> Result<(), InterpreterResult> {
					Ok(match instance {
						Value::List(items) => match index {
							Some(i) => {
								let index = interpreter.run_expression(*i)?.to_number();
								items.borrow_mut()[index as usize] = value.clone();
							}
							None => {
								items.borrow_mut().push(value.clone());
							}
						},
						_ => return Err(InterpreterResult::InvalidAppendTarget(instance.typestring())),
					})
				}

				match *target.clone() {
					Expression::Index(instance, index) => {
						let instance = self.run_expression(*instance)?;

						assign_to_list(self, instance, index, value.clone())?;
					}

					_ => {
						match self.run_expression(*target.clone())? {
							Value::Constant(_) => return Err(InterpreterResult::CannotAssignValueToConstant),
							_ => (),
						};

						match *target.clone() {
							Expression::Identifier(i) => {
								self.env_mut().set(i, value.clone());
							}
							_ => todo!(),
						}
					}
				};

				value
			}
			_ => todo!("{:?}", expression),
		})
	}

	pub fn path(&self) -> PathBuf {
		self.path.clone()
	}

	fn define_global_function(&mut self, name: impl Into<String>, callback: NativeFunctionCallback) {
		let name = name.into();

		self.globals.insert(name.clone(), Value::NativeFunction { name, callback });
	}

	fn define_global_struct(&mut self, struct_name: impl Into<String>, methods: HashMap<String, Value>) {
		let struct_name = struct_name.into();

		self.globals.insert(
			struct_name.clone(),
			Value::Struct { name: struct_name, methods: Rc::new(RefCell::new(methods)), fields: vec![], propreties: None },
		);
	}

	fn env(&self) -> Ref<Environment> {
		RefCell::borrow(&self.environment)
	}

	fn env_mut(&mut self) -> RefMut<Environment> {
		RefCell::borrow_mut(&self.environment)
	}

	fn get_property(&mut self, value: Value, field: String, target: Expression, expression: Expression) -> Result<Value, InterpreterResult> {
		Ok(match value {
			Value::ClassInstance { environment, definition, .. } => {
				//if let Some(value) = environment.borrow().get(field.clone()) {
				//	match value {
				//		Value::Function { name, params, body, environment, .. } => match expression.clone() {
				//			Expression::MethodCall(..) => Value::Function { name, params, body, environment, context: Some(target) },
				//			_ => {
				//				if let Expression::Identifier(i) = target {
				//					return Err(InterpreterResult::UndefinedField(i, field));
				//				} else {
				//					return Err(InterpreterResult::UndefinedField("None".to_string(), field));
				//				}
				//			}
				//		},
				//		_ => match expression.clone() {
				//			Expression::GetProperty(..) => value,
				//			_ => {
				//				if let Expression::Identifier(i) = target {
				//					return Err(InterpreterResult::UndefinedField(i, field));
				//				} else {
				//					return Err(InterpreterResult::UndefinedField("None".to_string(), field));
				//				}
				//			}
				//		},
				//	}
				//} else
				if let Some(value) = match *definition.clone() {
					Value::Struct { fields, .. } => fields.iter().find(|p| p.name == field).map(|p| p.get_default().clone()).unwrap_or_else(|| None),
					_ => None,
				} {
					let initial = self.run_expression(value)?;
					match initial {
						Value::Function { name, params, body, environment, .. } => {
							Value::Function { name, params, body, environment, context: Some(target) }
						}
						_ => initial,
					}
				} else {
					let name = match *definition {
						Value::Struct { name, .. } => name,
						_ => unreachable!(),
					};

					return Err(InterpreterResult::UndefinedField(name, field));
				}
			}
			Value::Struct { name, methods, fields, .. } => {
				if let Some(value) = methods.borrow().get(&field.clone()) {
					value.clone()
				} else if let Some(value) = fields.iter().find(|p| p.name == field).map(|p| p.get_default().clone()).unwrap_or_else(|| None) {
					let initial = self.run_expression(value)?;

					match initial {
						Value::Function { name, params, body, environment, .. } => {
							Value::Function { name, params, body, environment, context: Some(target) }
						}
						_ => initial,
					}
				} else {
					return Err(InterpreterResult::UndefinedMethod(name, field));
				}
			}
			Value::Constant(v) => self.get_property(*v, field, target, expression)?,
			_ => todo!(),
		})
	}

	pub fn exec(&mut self, ast: Program) -> Result<(), InterpreterResult> {
		let mut ast = ast.into_iter();

		while let Some(statement) = ast.next() {
			self.run_statement(statement)?;
		}

		Ok(())
	}

	fn run(&mut self) -> Result<(), InterpreterResult> {
		while let Some(statement) = self.ast.next() {
			self.run_statement(statement.clone())?;
		}

		if !::std::env::args().filter(|a| a == "--debug").collect::<Vec<String>>().is_empty() {
			self.env().dump();
			dbg!(self.globals.clone());
		}

		Ok(())
	}
}
