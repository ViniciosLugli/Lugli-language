use hashbrown::HashMap;

use crate::{environment::Value, token::Token};

pub type Program = Vec<Statement>;
pub type Block = Vec<Statement>;
pub type Identifier = String;
pub type Numerator = u8;

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBlock {
	pub expression: Expression,
	pub then: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	Return { value: Expression },
	Break,
	Continue,
	FunctionDeclaration { name: Identifier, params: Vec<Parameter>, body: Block },
	StructDeclaration { name: Identifier, fields: Vec<Parameter> },
	CreateDeclaration { name: Identifier, initial: Option<Expression> },
	ConstDeclaration { name: Identifier, initial: Expression },
	If { condition: ConditionBlock, others_conditions: Option<Vec<ConditionBlock>>, otherwise: Option<Block> },
	For { iterable: Expression, value: Identifier, index: Option<Identifier>, then: Block },
	While { condition: ConditionBlock },
	Loop { body: Block },
	Expression { expression: Expression },
}

#[derive(Debug, Clone)]
pub struct ArgumentValued {
	pub name: Option<Identifier>,
	pub value: Value,
}

#[derive(Debug, Clone)]
pub struct ArgumentValues {
	params_values: Vec<ArgumentValued>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
	pub name: String,
	pub initial: Option<Expression>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	name: Option<String>,
	expression: Expression,
}

impl Parameter {
	pub fn new(name: String, initial: Option<Expression>) -> Self {
		Self { name, initial }
	}

	pub fn get_name(&self) -> String {
		self.name.clone()
	}

	pub fn get_initial(&self) -> Option<Expression> {
		self.initial.clone()
	}

	pub fn has_initial(&self) -> bool {
		self.initial.is_some()
	}
}

impl ArgumentValued {
	pub fn new(name: Option<String>, value: Value) -> Self {
		Self { name, value }
	}

	pub fn get_name(&self) -> Option<String> {
		self.name.clone()
	}

	pub fn get_value(&self) -> Value {
		self.value.clone()
	}
}

impl ArgumentValues {
	pub fn new() -> ArgumentValues {
		ArgumentValues { params_values: Vec::new() }
	}

	pub fn get_from_name(&self, name: Identifier) -> Option<Value> {
		for param in &self.params_values {
			if param.name == Some(name.clone()) {
				return Some(param.value.clone());
			}
		}
		None
	}

	pub fn get_from_index(&self, index: usize) -> Option<Value> {
		if index < self.params_values.len() {
			return Some(self.params_values[index].value.clone());
		}
		None
	}

	pub fn get_name_or_index(&self, name: String, index: usize) -> Option<Value> {
		if let Some(value) = self.get_from_name(name) {
			return Some(value);
		}
		self.get_from_index(index)
	}

	pub fn is_empty(&self) -> bool {
		self.params_values.is_empty()
	}

	pub fn push(&mut self, argument_valued: ArgumentValued) {
		self.params_values.push(argument_valued);
	}

	pub fn get_raw(&self) -> Vec<ArgumentValued> {
		self.params_values.clone()
	}

	pub fn len(&self) -> usize {
		self.params_values.len()
	}
}

impl Iterator for ArgumentValues {
	type Item = ArgumentValued;

	fn next(&mut self) -> Option<Self::Item> {
		self.params_values.pop()
	}
}

impl Argument {
	pub fn new(name: Option<String>, expression: Expression) -> Self {
		Self { name, expression }
	}

	pub fn get_expression(&self) -> &Expression {
		&self.expression
	}

	pub fn get_name(&self) -> &Option<String> {
		&self.name
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallArguments {
	arguments: Vec<Argument>,
}

impl CallArguments {
	pub fn new() -> Self {
		Self { arguments: Vec::new() }
	}

	pub fn add_argument(&mut self, argument: Argument) {
		self.arguments.push(argument);
	}

	pub fn get_arguments(&self) -> &Vec<Argument> {
		&self.arguments
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	Number(f64),
	String(String),
	Bool(bool),
	Null,
	Identifier(Identifier),
	Assign(Box<Expression>, Box<Expression>),
	MathAssign(Box<Expression>, Op, Box<Expression>),
	Infix(Box<Expression>, Op, Box<Expression>),
	Prefix(Op, Box<Expression>),
	Call(Box<Expression>, CallArguments),
	Struct(Box<Expression>, HashMap<Identifier, Expression>),
	Closure(Vec<Parameter>, Vec<Statement>),
	MethodCall(Box<Expression>, Identifier, CallArguments),
	GetProperty(Box<Expression>, Identifier),
	Index(Box<Expression>, Option<Box<Expression>>),
	List(Vec<Expression>),
}

#[allow(dead_code)]
impl Expression {
	pub fn some(self) -> Option<Self> {
		Some(self)
	}

	pub fn boxed(self) -> Box<Self> {
		Box::new(self)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Bang,
	Equals,
	NotEquals,
	Assign,
	LessThan,
	GreaterThan,
	LessThanOrEquals,
	GreaterThanOrEquals,
	And,
	Or,
	Pow,
	In,
	NotIn,
}

impl Op {
	pub fn token(token: Token) -> Self {
		match token {
			Token::Plus => Self::Add,
			Token::Minus => Self::Subtract,
			Token::Asterisk => Self::Multiply,
			Token::Slash => Self::Divide,
			Token::Bang => Self::Bang,
			Token::Percent => Self::Modulo,
			Token::Equals => Self::Equals,
			Token::NotEquals => Self::NotEquals,
			Token::Assign => Self::Assign,
			Token::LessThan => Self::LessThan,
			Token::GreaterThan => Self::GreaterThan,
			Token::LessThanOrEquals => Self::LessThanOrEquals,
			Token::GreaterThanOrEquals => Self::GreaterThanOrEquals,
			Token::And => Self::And,
			Token::Or => Self::Or,
			Token::Pow => Self::Pow,
			Token::In => Self::In,
			Token::NotIn => Self::NotIn,
			_ => unreachable!("{:?}", token),
		}
	}
}
