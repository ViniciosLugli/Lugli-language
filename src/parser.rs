use crate::ast::{Block, ConditionBlock, Expression, Op, Parameter, Statement};
use crate::token::{generate, Token};
use colored::*;
use std::slice::Iter;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
	#[error("Unexpected token `{0:?}`")]
	UnexpectedToken(Token),

	#[error("Unexpected token `{0:?}`, expected `{1:?}`")]
	UnexpectedTokenExpected(Token, Token),

	#[error("Entered unreachable code.")]
	Unreachable,
}
#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
	Lowest,
	Statement,
	Assign,
	PlusAssign,
	MinusAssign,
	MultiplyAssign,
	DivideAssign,
	Increment,
	Decrement,
	AndOr,
	LessThanGreaterThan,
	Equals,
	Sum,
	Product,
	Pow,
	Modulo,
	Prefix,
	Call,
}

impl Precedence {
	fn token(token: Token) -> Self {
		match token {
			Token::Asterisk | Token::Slash => Self::Product,
			Token::Plus | Token::Minus => Self::Sum,
			Token::Percent => Self::Modulo,
			Token::Pow => Self::Pow,
			Token::Equals | Token::NotEquals => Self::Equals,
			Token::And | Token::Or | Token::In | Token::NotIn => Self::AndOr,
			Token::Assign => Self::Assign,
			Token::PlusAssign => Self::PlusAssign,
			Token::MinusAssign => Self::MinusAssign,
			Token::MultiplyAssign => Self::MultiplyAssign,
			Token::DivideAssign => Self::DivideAssign,
			Token::Increment => Self::Increment,
			Token::Decrement => Self::Decrement,
			Token::LessThan | Token::GreaterThan | Token::LessThanOrEquals | Token::GreaterThanOrEquals => Self::LessThanGreaterThan,
			Token::LeftParen | Token::Dot | Token::LeftBracket => Self::Call,
			Token::LeftBrace => Self::Statement,
			_ => Self::Lowest,
		}
	}
}

struct Parser<'p> {
	tokens: Iter<'p, Token>,
	current: Token,
	peek: Token,
}

impl<'p> Parser<'p> {
	fn new(tokens: Vec<Token>) -> Self {
		let mut tokens = tokens.iter();
		let current = tokens.next().unwrap().clone();
		let peek = tokens.next().unwrap().clone();
		Self { tokens, current, peek }
	}

	fn next(&mut self) -> Token {
		let next = self.tokens.next();
		if let Some(next) = next {
			self.current = self.peek.clone();
			self.peek = next.clone();
			next.clone()
		} else {
			self.current.clone()
		}
	}

	fn peek(&self) -> Token {
		self.peek.clone()
	}

	fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
		let mut statements = Vec::new();
		while self.current != Token::Eof {
			statements.push(self.parse_statement()?);
		}
		Ok(statements)
	}

	fn parse_statement(&mut self) -> Result<Statement, ParseError> {
		match self.current {
			Token::Return => self.parse_return(),
			Token::Break => self.parse_break(),
			Token::Continue => self.parse_continue(),
			Token::Function => self.parse_function(),
			Token::Class => self.parse_class(),
			Token::Create => self.parse_variable_declaration(),
			Token::Constant => self.parse_constant_declaration(),
			Token::If => self.parse_if(),
			Token::For => self.parse_for(),
			Token::While => self.parse_while(),
			Token::Loop => self.parse_loop(),
			_ => self.parse_expression_statement(),
		}
	}

	fn parse_return(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let value = self
			.parse_expression(Precedence::Lowest)?
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		Ok(Statement::Return { value })
	}

	fn parse_break(&mut self) -> Result<Statement, ParseError> {
		self.next();
		Ok(Statement::Break)
	}

	fn parse_continue(&mut self) -> Result<Statement, ParseError> {
		self.next();
		Ok(Statement::Continue)
	}

	fn parse_function(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let name = self
			.next()
			.into()
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let params = self.parse_parameters()?;
		let body = self.parse_block()?;
		Ok(Statement::FunctionDeclaration { name, params, body })
	}

	fn parse_class(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let name = self
			.next()
			.into()
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let fields = self.parse_parameters()?;
		Ok(Statement::ClassDeclaration { name, fields })
	}

	fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let name = self
			.next()
			.into()
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let initial = if self.current == Token::Assign {
			self.next();
			self.parse_expression(Precedence::Lowest)?
		} else {
			None
		};
		Ok(Statement::VariableDeclaration { name, initial })
	}

	fn parse_constant_declaration(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let name = self
			.next()
			.into()
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		self.next();
		let initial = self
			.parse_expression(Precedence::Lowest)?
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		Ok(Statement::ConstantDeclaration { name, initial })
	}

	fn parse_if(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let condition = self.parse_condition_block()?;
		let mut others_conditions = None;
		let mut otherwise = None;
		while self.current == Token::ElseIf {
			self.next();
			let condition_block = self.parse_condition_block()?;
			if others_conditions.is_none() {
				others_conditions = Some(Vec::new());
			}
			others_conditions.as_mut().unwrap().push(condition_block);
		}
		if self.current == Token::Else {
			self.next();
			otherwise = Some(self.parse_block()?);
		}
		Ok(Statement::If { condition, others_conditions, otherwise })
	}

	fn parse_for(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let iterable = self
			.parse_expression(Precedence::Lowest)?
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let value = self
			.next()
			.into()
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let index = if self.current == Token::Comma {
			self.next();
			let index = self
				.next()
				.into()
				.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
			Some(index)
		} else {
			None
		};
		let then = self.parse_block()?;
		Ok(Statement::For { iterable, value, index, then })
	}

	fn parse_while(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let condition = self.parse_condition_block()?;
		Ok(Statement::While { condition })
	}

	fn parse_loop(&mut self) -> Result<Statement, ParseError> {
		self.next();
		let body = self.parse_block()?;
		Ok(Statement::Loop { body })
	}

	fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
		let expression = self.parse_expression(Precedence::Lowest)?;
		Ok(Statement::Expression { expression })
	}

	fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
		let mut params = Vec::new();
		if self.current == Token::LeftParen {
			self.next();
			while self.current != Token::RightParen {
				let name = self
					.next()
					.into()
					.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
				let default = if self.current == Token::Assign {
					self.next();
					self.parse_expression(Precedence::Lowest)?
				} else {
					None
				};
				params.push(Parameter { name, default });
				if self.current == Token::Comma {
					self.next();
				}
			}
			self.next();
		}
		Ok(params)
	}

	fn parse_condition_block(&mut self) -> Result<ConditionBlock, ParseError> {
		let expression = self
			.parse_expression(Precedence::Lowest)?
			.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
		let then = self.parse_block()?;
		Ok(ConditionBlock { expression, then })
	}

	fn parse_block(&mut self) -> Result<Block, ParseError> {
		let mut statements = Vec::new();
		if self.current == Token::LeftBrace {
			self.next();
			while self.current != Token::RightBrace {
				statements.push(self.parse_statement()?);
			}
			self.next();
		} else {
			statements.push(self.parse_statement()?);
		}
		Ok(statements)
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Option<Expression>, ParseError> {
		let mut left = match self.current {
			Token::Number(n) => {
				self.next();
				Some(Expression::Number(n))
			}
			Token::String(s) => {
				self.next();
				Some(Expression::String(s))
			}
			Token::True => {
				self.next();
				Some(Expression::Bool(true))
			}
			Token::False => {
				self.next();
				Some(Expression::Bool(false))
			}
			Token::Null => {
				self.next();
				Some(Expression::Null)
			}
			Token::Identifier(s) => {
				self.next();
				Some(Expression::Identifier(s))
			}
			Token::LeftParen => {
				self.next();
				let expression = self.parse_expression(Precedence::Lowest)?;
				if self.current != Token::RightParen {
					return Err(ParseError::UnexpectedToken(self.current.clone()));
				}
				self.next();
				expression
			}
			Token::LeftBracket => {
				self.next();
				let mut elements = Vec::new();
				while self.current != Token::RightBracket {
					let element = self.parse
				}
			}
			_ => None,
		};
		while precedence < Precedence::token(self.current.clone()) {
			match self.current {
				Token::Plus | Token::Minus | Token::Asterisk | Token::Slash | Token::Pow | Token::Percent | Token::Equals | Token::NotEquals | Token::And | Token::Or | Token::In | Token::NotIn | Token::LessThan | Token::GreaterThan | Token::LessThanOrEquals | Token::GreaterThanOrEquals => {
					let op = self.current.clone();
					self.next();
					let right = self.parse_expression(Precedence::token(self.current.clone()))?;
					left = Some(Expression::Infix(Box::new(left.unwrap()), op.into(), Box::new(right.unwrap())));
				}
				Token::Assign => {
					let op = self.current.clone();
					self.next();
					let right = self.parse_expression(Precedence::token(self.current.clone()))?;
					left = Some(Expression::Assign(Box::new(left.unwrap()), Box::new(right.unwrap())));
				}
				Token::PlusAssign => {
					let op = self.current.clone();
					self.next();
					let right = self.parse_expression(Precedence::token(self.current.clone()))?;
					left = Some(Expression::MathAssign(Box::new(left.unwrap()), op.into(), Box::new(right.unwrap())));
				}
				Token::MinusAssign => {
					let op = self.current.clone();
					self.next();
					let right = self.parse_expression(Precedence::token(self.current.clone()))?;
					left = Some(Expression::MathAssign(Box::new(left.unwrap()), op.into(), Box::new(right.unwrap())));
				}
				Token::MultiplyAssign => {
					let op = self.current.clone();
					self.next();
					let right = self.parse_expression(Precedence::token(self.current.clone()))?;
					left = Some(Expression::MathAssign(Box::new(left.unwrap()), op.into(), Box::new(right.unwrap())));
				}
				Token::DivideAssign => {
					let op = self.current.clone();
					self.next();
					let right = self
						.parse_expression(Precedence::token(self.current.clone()))?
						.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
					left = Some(Expression::MathAssign(Box::new(left.unwrap()), op.into(), Box::new(right)));
				}
				Token::Increment => {
					let op = self.current.clone();
					self.next();
					left = Some(Expression::Prefix(op.into(), Box::new(left.unwrap())));
				}
				Token::Decrement => {
					let op = self.current.clone();
					self.next();
					left = Some(Expression::Prefix(op.into(), Box::new(left.unwrap())));
				}
				Token::LeftParen => {
					self.next();
					let mut parameters = Vec::new();
					while self.current != Token::RightParen {
						let parameter = self.parse_expression(Precedence::Lowest)?;
						parameters.push(parameter.unwrap());
						if self.current == Token::Comma {
							self.next();
						}
					}
					self.next();
					left = Some(Expression::Call(Box::new(left.unwrap()), parameters));
				}
				Token::Dot => {
					self.next();
					let identifier = self
						.next()
						.into()
						.ok_or_else(|| ParseError::UnexpectedToken(self.current.clone()))?;
					left = Some(Expression::Index(Box::new(left.unwrap()), Some(Box::new(Expression::Identifier(identifier)))));
				}
				Token::LeftBracket => {
					self.next();
					let index = self.parse_expression(Precedence::Lowest)?;
					if self.current != Token::RightBracket {
						return Err(ParseError::UnexpectedToken(self.current.clone()));
					}
					self.next();
					left = Some(Expression::Index(Box::new(left.unwrap()), index));
				}
				_ => return Err(ParseError::UnexpectedToken(self.current.clone())),
			}
		}
		Ok(left)
	}
}
