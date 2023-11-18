use hashbrown::HashMap;

pub type Block = Vec<Statement>;
pub type Identifier = String;

#[derive(Debug, PartialEq)]
pub struct ConditionBlock {
	pub expression: Expression,
	pub then: Block,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
	pub name: String,
	pub default: Option<Expression>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
	Call(Box<Expression>, Vec<Parameter>),
	Closure(Vec<Parameter>, Vec<Statement>),
	Index(Box<Expression>, Option<Box<Expression>>),
	Get(Box<Expression>, Identifier),
}

#[derive(Debug, PartialEq)]
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
