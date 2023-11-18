use crate::{
	ast::Statement,
	environment::{Environment, Value},
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
