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
	Module,
	Prefix,
	Call,
}

impl Precedence {
	fn token(token: Token) -> Self {
		match token {
			Token::Asterisk | Token::Slash => Self::Product,
			Token::Plus | Token::Minus => Self::Sum,
			Token::Percent => Self::Module,
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
	fn new(tokens: Iter<'p, Token>) -> Self {
		Self { current: Token::Eof, peek: Token::Eof, tokens }
	}
	fn parse_statement(&mut self) -> Result<Statement, ParseError> {
		match self.current {
			Token::Function => self.parse_fn(true),
			Token::Class => self.parse_struct(),
			Token::Create => self.parse_create(),
			Token::Constant => self.parse_const(),
			Token::If => self.parse_if(),
			Token::For => self.parse_for(),
			Token::While => self.parse_while(),
			Token::Loop => self.parse_loop(),
			Token::Return => self.parse_return(),
			Token::Break => self.parse_break(),
			Token::Continue => self.parse_continue(),
			_ => Ok(Statement::Expression { expression: self.parse_expression(Precedence::Lowest)? }),
		}
	}

	fn current_is(&self, token: Token) -> bool {
		std::mem::discriminant(&self.current) == std::mem::discriminant(&token)
	}

	fn expect_token(&mut self, token: Token) -> Result<Token, ParseError> {
		if self.current_is(token.clone()) {
			Ok(self.current.clone())
		} else {
			Err(ParseError::UnexpectedTokenExpected(self.current.clone(), token))
		}
	}

	fn expect_token_and_read(&mut self, token: Token) -> Result<Token, ParseError> {
		let result = self.expect_token(token)?;

		self.read();

		Ok(result)
	}

	fn expect_identifier_and_read(&mut self) -> Result<Token, ParseError> {
		self.expect_token_and_read(Token::Identifier(String::new()))
	}

	fn read(&mut self) {
		self.current = self.peek.clone();
		self.peek = if let Some(token) = self.tokens.next() { token.clone() } else { Token::Eof };
	}

	fn next(&mut self) -> Result<Option<Statement>, ParseError> {
		if self.current == Token::Eof {
			return Ok(None);
		}

		Ok(Some(self.parse_statement()?))
	}

	fn parse_block(&mut self) -> Result<Block, ParseError> {
		let mut statements = Vec::new();

		while !self.current_is(Token::RightBrace) && !self.current_is(Token::Eof) {
			statements.push(self.parse_statement()?);
		}

		self.expect_token_and_read(Token::RightBrace)?;

		Ok(statements)
	}
}
