use crate::token::Token;
use hashbrown::HashMap;
pub type Block = Vec<Statement>;
pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionBlock {
	pub expression: Expression,
	pub then: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
	pub name: String,
	pub default: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
	Program(Vec<Statement>),
	Return { value: Expression },
	Break,
	Continue,

	FunctionDeclaration { name: Identifier, params: Vec<Parameter>, body: Block },
	ClassDeclaration { name: Identifier, fields: Vec<Parameter> },
	VariableDeclaration { name: Identifier, initial: Option<Expression> },
	ConstantDeclaration { name: Identifier, initial: Expression },

	If { condition: ConditionBlock, others_conditions: Option<Vec<ConditionBlock>>, otherwise: Option<Block> },
	For { iterable: Expression, value: Identifier, index: Option<Identifier>, then: Block },
	While { condition: ConditionBlock },
	Loop { body: Block },
	Expression { expression: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
	Number(f64),
	String(String),
	Bool(bool),
	Null,
	Identifier(Identifier),
	List(Vec<Expression>),
	Assign(Box<Expression>, Box<Expression>),
	MathAssign(Box<Expression>, Op, Box<Expression>),
	Infix(Box<Expression>, Op, Box<Expression>),
	Prefix(Op, Box<Expression>),
	Class(Box<Expression>, HashMap<Identifier, Expression>),
	Call(Box<Expression>, CallArguments),
	Closure(Vec<Parameter>, Vec<Statement>),
	Index(Box<Expression>, Option<Box<Expression>>),
	Get(Box<Expression>, Identifier),
}

impl Expression {
	pub fn boxed(self) -> Box<Expression> {
		Box::new(self)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
	Add,
	Subtract,
	Multiply,
	Divide,
	Module,
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
			Token::Percent => Self::Module,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
	name: Option<String>,
	expression: Expression,
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
