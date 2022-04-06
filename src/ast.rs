use hashbrown::HashMap;

use crate::token::Token;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
	pub name: String,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	pub name: Option<String>,
	pub number: Numerator,
	pub value: Expression,
}

impl Argument {
	pub fn new(name: Option<String>, number: Numerator, value: Expression) -> Self {
		Self { name, number, value }
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

	pub fn get_from_index(&self, number: Numerator) -> Option<&Argument> {
		self.arguments.iter().find(|arg| arg.number == number)
	}

	pub fn get_from_name(&self, name: String) -> Option<&Argument> {
		self.arguments.iter().find(|arg| arg.name == Some(name))
	}

	pub fn len(&self) -> usize {
		self.arguments.len()
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
