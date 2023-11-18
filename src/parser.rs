use crate::ast::{Argument, Block, CallArguments, Expression, Identifier, Op, Parameter, Statement};
use crate::token::Token;
use hashbrown::HashMap;
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

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
		let mut left = match self.current.clone() {
			Token::String(s) => {
				self.expect_token_and_read(Token::String(String::new()))?;
				Expression::String(s.to_string())
			}
			Token::Null => {
				self.expect_token_and_read(Token::Null)?;
				Expression::Null
			}
			Token::Number(n) => {
				self.expect_token_and_read(Token::Number(0.0))?;
				Expression::Number(n)
			}
			Token::True => {
				self.expect_token_and_read(Token::True)?;
				Expression::Bool(true)
			}
			Token::False => {
				self.expect_token_and_read(Token::False)?;
				Expression::Bool(false)
			}
			Token::Identifier(s) => {
				self.expect_identifier_and_read()?;
				Expression::Identifier(s)
			}
			Token::Function => {
				let (params, body) = match self.parse_function(false)? {
					Statement::FunctionDeclaration { params, body, .. } => (params, body),
					_ => return Err(ParseError::Unreachable),
				};

				Expression::Closure(params, body)
			}
			t @ Token::Minus | t @ Token::Bang => {
				self.expect_token_and_read(t.clone())?;

				Expression::Prefix(Op::token(t), self.parse_expression(Precedence::Prefix)?.boxed())
			}
			Token::LeftBracket => {
				self.expect_token_and_read(Token::LeftBracket)?;

				let mut items: Vec<Expression> = Vec::new();

				while !self.current_is(Token::RightBracket) {
					items.push(self.parse_expression(Precedence::Lowest)?);

					if self.current_is(Token::Comma) {
						self.expect_token_and_read(Token::Comma)?;
					}
				}

				self.expect_token_and_read(Token::RightBracket)?;

				Expression::List(items)
			}
			_ => return Err(ParseError::UnexpectedToken(self.current.clone())),
		};

		while !self.current_is(Token::Eof) && precedence < Precedence::token(self.current.clone()) {
			if let Some(expression) = self.parse_postfix_expression(left.clone())? {
				left = expression;
			} else if let Some(expression) = self.parse_infix_expression(left.clone())? {
				left = expression
			} else {
				break;
			}
		}

		Ok(left)
	}

	fn parse_arguments(&mut self) -> Result<CallArguments, ParseError> {
		self.expect_token_and_read(Token::LeftParen)?;

		let mut args = CallArguments::new();
		let mut first_arg = true;

		while !self.current_is(Token::RightParen) {
			if !first_arg {
				self.expect_token_and_read(Token::Comma)?;
			}

			let expression = self.parse_expression(Precedence::Lowest)?;

			let argument = match expression {
				Expression::Assign(param, value) => match *param {
					Expression::Identifier(name) => Argument::new(Some(name), *value),
					_ => return Err(ParseError::UnexpectedToken(self.current.clone())),
				},
				_ => Argument::new(None, expression),
			};

			args.add_argument(argument);
			first_arg = false;
		}

		self.expect_token_and_read(Token::RightParen)?;
		Ok(args)
	}

	fn parse_object_fields(&mut self) -> Result<HashMap<Identifier, Expression>, ParseError> {
		let mut fields = HashMap::new();

		while !self.current_is(Token::RightBrace) {
			let field = self.expect_identifier_and_read()?;

			let value = if self.current_is(Token::Colon) {
				self.expect_token_and_read(Token::Colon)?;
				self.parse_expression(Precedence::Lowest)?
			} else {
				Expression::Identifier(field.clone().into())
			};

			fields.insert(field.into(), value);

			if self.current_is(Token::Comma) {
				self.read();
			}
		}

		self.expect_token_and_read(Token::RightBrace)?;
		Ok(fields)
	}

	fn parse_postfix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParseError> {
		Ok(match self.current {
			Token::Dot => unimplemented!(),

			Token::LeftBracket => {
				self.expect_token_and_read(Token::LeftBracket)?;

				let index = if self.current_is(Token::RightBracket) { None } else { Some(self.parse_expression(Precedence::Lowest)?.boxed()) };

				self.expect_token_and_read(Token::RightBracket)?;
				Some(Expression::Index(left.boxed(), index))
			}

			Token::LeftBrace => {
				self.expect_token_and_read(Token::LeftBrace)?;
				let fields = self.parse_object_fields()?;
				Some(Expression::Class(left.boxed(), fields))
			}

			Token::LeftParen => {
				let args = self.parse_arguments()?;
				Some(Expression::Call(Box::new(left), args))
			}

			_ => None,
		})
	}

	fn parse_assignment_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParseError> {
		let op = match self.current {
			Token::Assign => Op::Assign,
			Token::PlusAssign => Op::Add,
			Token::MinusAssign => Op::Subtract,
			Token::MultiplyAssign => Op::Multiply,
			Token::DivideAssign => Op::Divide,
			_ => return Err(ParseError::UnexpectedToken(self.current.clone())),
		};

		self.read();
		let right = self.parse_expression(Precedence::Lowest)?;
		Ok(Some(Expression::MathAssign(Box::new(left), op, Box::new(right))))
	}

	fn parse_increment_expression(&mut self, left: Expression, op: Op) -> Result<Option<Expression>, ParseError> {
		self.read();
		Ok(Some(Expression::MathAssign(Box::new(left), op, Box::new(Expression::Number(1.0)))))
	}

	fn parse_infix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParseError> {
		match self.current {
			Token::Plus
			| Token::Minus
			| Token::Asterisk
			| Token::Slash
			| Token::Equals
			| Token::NotEquals
			| Token::LessThanOrEquals
			| Token::LessThan
			| Token::GreaterThan
			| Token::GreaterThanOrEquals
			| Token::And
			| Token::Or
			| Token::Pow
			| Token::In
			| Token::NotIn
			| Token::Percent => {
				let token = self.current.clone();
				self.read();
				let right = self.parse_expression(Precedence::token(token.clone()))?;
				Ok(Some(Expression::Infix(Box::new(left), Op::token(token), Box::new(right))))
			}
			Token::Assign | Token::PlusAssign | Token::MinusAssign | Token::MultiplyAssign | Token::DivideAssign => {
				self.parse_assignment_expression(left)
			}
			Token::Increment => self.parse_increment_expression(left, Op::Add),
			Token::Decrement => self.parse_increment_expression(left, Op::Subtract),
			_ => Ok(None),
		}
	}

	// parse_assignment_expression and parse_increment_expression remain the same

	fn parse_statement(&mut self) -> Result<Statement, ParseError> {
		match self.current {
			Token::Function => self.parse_function(true),
			Token::Class => self.parse_class(),
			Token::Create => self.parse_create(),
			Token::Constant => self.parse_constant(),
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

	fn parse_function(&mut self, with_identifier: bool) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Function)?;

		let name = if with_identifier { self.expect_identifier_and_read()?.into() } else { String::from("<Closure>") };

		self.expect_token_and_read(Token::LeftParen)?;

		let mut params = Vec::new();
		let mut first_param = true;

		while !self.current_is(Token::RightParen) {
			if !first_param {
				self.expect_token_and_read(Token::Comma)?;
			}

			let param_name: String = self.expect_identifier_and_read()?.into();
			let default = if self.current_is(Token::Assign) {
				self.expect_token_and_read(Token::Assign)?;
				Some(self.parse_expression(Precedence::Lowest)?)
			} else {
				None
			};

			params.push(Parameter { name: param_name, default });
			first_param = false;
		}

		self.expect_token_and_read(Token::RightParen)?;
		let body = self.parse_block()?;

		Ok(Statement::FunctionDeclaration { name, params, body })
	}
}
