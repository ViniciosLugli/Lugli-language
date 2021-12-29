use colored::*;
use hashbrown::HashMap;
use std::slice::Iter;
use thiserror::Error;

use crate::{ast::ConditionBlock, ast::*, token::Token};

pub fn parse(tokens: Vec<Token>) -> Result<Program, ParseError> {
	let mut parser = Parser::new(tokens.iter());

	parser.read();
	parser.read();

	let mut program: Program = Vec::new();

	while let Some(statement) = parser.next()? {
		program.push(statement);
	}

	Ok(program)
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
	Lowest,
	Statement,
	Assign,
	AndOr,
	LessThanGreaterThan,
	Equals,
	Sum,
	Product,
	Pow,
	Prefix,
	Call,
}

impl Precedence {
	fn token(token: Token) -> Self {
		match token {
			Token::Asterisk | Token::Slash => Self::Product,
			Token::Plus | Token::Minus => Self::Sum,
			Token::LeftParen | Token::Dot | Token::LeftBracket => Self::Call,
			Token::LessThan | Token::GreaterThan | Token::LessThanOrEquals | Token::GreaterThanOrEquals => Self::LessThanGreaterThan,
			Token::Equals | Token::NotEquals => Self::Equals,
			Token::And | Token::Or | Token::In | Token::NotIn => Self::AndOr,
			Token::Assign => Self::Assign,
			Token::LeftBrace => Self::Statement,
			Token::Pow => Self::Pow,
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
			Token::Fn => self.parse_fn(true),
			Token::Struct => self.parse_struct(),
			Token::Create => self.parse_create(),
			Token::Const => self.parse_const(),
			Token::If => self.parse_if(),
			Token::For => self.parse_for(),
			Token::While => self.parse_while(),
			Token::Return => self.parse_return(),
			Token::Break => self.parse_break(),
			Token::Continue => self.parse_continue(),
			_ => Ok(Statement::Expression { expression: self.parse_expression(Precedence::Lowest)? }),
		}
	}

	fn parse_for(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::For)?;

		let (index, value) = if self.current_is(Token::LeftParen) {
			self.expect_token_and_read(Token::LeftParen)?;
			let index = self.expect_identifier_and_read()?;
			self.expect_token_and_read(Token::Comma)?;
			let tuple = (Some(index.into()), self.expect_identifier_and_read()?.into());
			self.expect_token_and_read(Token::RightParen)?;
			tuple
		} else {
			(None, self.expect_identifier_and_read()?.into())
		};

		self.expect_token_and_read(Token::In)?;

		let iterable = self.parse_expression(Precedence::Statement)?;
		let then = self.parse_block()?;

		Ok(Statement::For { index, value, iterable, then })
	}

	fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
		let mut left = match self.current.clone() {
			Token::String(s) => {
				self.expect_token_and_read(Token::String("".to_string()))?;
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
			Token::Fn => {
				let (params, body) = match self.parse_fn(false)? {
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

	fn parse_postfix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParseError> {
		Ok(match self.current {
			Token::Dot => {
				self.expect_token_and_read(Token::Dot)?;

				let field = self.expect_identifier_and_read()?.into();

				Some(Expression::Get(Box::new(left), field))
			}
			Token::LeftBracket => {
				self.expect_token_and_read(Token::LeftBracket)?;

				let index: Option<Box<Expression>> =
					if self.current_is(Token::RightBracket) { None } else { Some(self.parse_expression(Precedence::Lowest)?.boxed()) };

				self.expect_token_and_read(Token::RightBracket)?;

				Some(Expression::Index(left.boxed(), index))
			}
			Token::LeftBrace => {
				self.expect_token_and_read(Token::LeftBrace)?;

				let mut fields: HashMap<Identifier, Expression> = HashMap::new();

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

				Some(Expression::Struct(left.boxed(), fields))
			}
			Token::LeftParen => {
				self.expect_token_and_read(Token::LeftParen)?;

				let mut args = Vec::new();

				while !self.current_is(Token::RightParen) {
					args.push(self.parse_expression(Precedence::Lowest)?);

					if self.current_is(Token::Comma) {
						self.read();
					}
				}

				self.expect_token_and_read(Token::RightParen)?;

				Some(Expression::Call(Box::new(left), args))
			}
			_ => None,
		})
	}

	fn parse_infix_expression(&mut self, left: Expression) -> Result<Option<Expression>, ParseError> {
		Ok(match self.current {
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
			| Token::NotIn => {
				let token = self.current.clone();

				self.read();

				let right = self.parse_expression(Precedence::token(token.clone()))?;

				Some(Expression::Infix(Box::new(left), Op::token(token), Box::new(right)))
			}
			Token::Assign => {
				self.read();

				let right = self.parse_expression(Precedence::Lowest)?;

				Some(Expression::Assign(Box::new(left), Box::new(right)))
			}
			_ => None,
		})
	}

	fn parse_if(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::If)?;
		let condition_if;
		if self.current_is(Token::LeftParen) {
			self.expect_token_and_read(Token::LeftParen)?;
			condition_if = self.parse_expression(Precedence::Lowest)?;
			self.expect_token_and_read(Token::RightParen)?;
		} else {
			condition_if = self.parse_expression(Precedence::Statement)?;
		}

		let then_if = self.parse_block()?;

		let others_conditions = if self.current_is(Token::ElseIf) {
			let mut others_conditions: Vec<ConditionBlock> = Vec::new();

			while self.current_is(Token::ElseIf) {
				self.expect_token_and_read(Token::ElseIf)?;

				let condition_else_if;

				if self.current_is(Token::LeftParen) {
					self.expect_token_and_read(Token::LeftParen)?;
					condition_else_if = self.parse_expression(Precedence::Lowest)?;
					self.expect_token_and_read(Token::RightParen)?;
				} else {
					condition_else_if = self.parse_expression(Precedence::Statement)?;
				}
				others_conditions.push(ConditionBlock { expression: condition_else_if, then: self.parse_block()? });
			}

			Some(others_conditions)
		} else {
			None
		};

		let otherwise = if self.current_is(Token::Else) {
			self.expect_token_and_read(Token::Else)?;
			Some(self.parse_block()?)
		} else {
			None
		};

		Ok(Statement::If { condition: ConditionBlock { expression: condition_if, then: then_if }, others_conditions, otherwise })
	}

	fn parse_while(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::While)?;

		let condition = if self.current_is(Token::LeftParen) {
			self.expect_token_and_read(Token::LeftParen)?;
			let condition = self.parse_expression(Precedence::Statement)?;
			self.expect_token_and_read(Token::RightParen)?;
			condition
		} else {
			self.parse_expression(Precedence::Statement)?
		};

		let then = self.parse_block()?;

		Ok(Statement::While { condition, then })
	}

	fn parse_return(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Return)?;

		if let Ok(expression) = self.parse_expression(Precedence::Lowest) {
			Ok(Statement::Return { value: expression })
		} else {
			Ok(Statement::Return { value: Expression::Null })
		}
	}

	fn parse_break(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Break)?;

		Ok(Statement::Break)
	}

	fn parse_continue(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Continue)?;

		Ok(Statement::Continue)
	}

	fn parse_create(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Create)?;

		let name: Identifier = self.expect_identifier_and_read()?.into();
		let initial: Option<Expression> = if self.current_is(Token::Assign) {
			self.expect_token_and_read(Token::Assign)?;

			Some(self.parse_expression(Precedence::Lowest)?)
		} else {
			None
		};

		Ok(Statement::CreateDeclaration { name, initial })
	}

	fn parse_const(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Const)?;

		let name: Identifier = self.expect_identifier_and_read()?.into();
		self.expect_token_and_read(Token::Assign)?;

		let initial = self.parse_expression(Precedence::Lowest)?;

		Ok(Statement::ConstDeclaration { name, initial })
	}

	fn parse_struct(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Struct)?;

		let name: Identifier = self.expect_identifier_and_read()?.into();

		self.expect_token_and_read(Token::LeftBrace)?;

		let mut fields: Vec<Parameter> = Vec::new();

		while !self.current_is(Token::RightBrace) {
			if self.current_is(Token::Comma) {
				self.expect_token_and_read(Token::Comma)?;
			}

			let field: String = self.expect_identifier_and_read()?.into();

			fields.push(Parameter { name: field })
		}

		self.expect_token_and_read(Token::RightBrace)?;

		Ok(Statement::StructDeclaration { name, fields })
	}

	fn parse_fn(&mut self, with_identifier: bool) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Fn)?;

		let name: Identifier = if with_identifier { self.expect_identifier_and_read()?.into() } else { String::from("<Closure>") };

		self.expect_token_and_read(Token::LeftParen)?;

		let mut params: Vec<Parameter> = Vec::new();

		while !self.current_is(Token::RightParen) {
			if self.current_is(Token::Comma) {
				self.expect_token_and_read(Token::Comma)?;
			}

			let param: String = self.expect_identifier_and_read()?.into();

			params.push(Parameter { name: param })
		}

		self.expect_token_and_read(Token::RightParen)?;

		let body: Vec<Statement> = self.parse_block()?;

		Ok(Statement::FunctionDeclaration { name, params, body })
	}

	fn parse_block(&mut self) -> Result<Block, ParseError> {
		self.expect_token_and_read(Token::LeftBrace)?;

		let mut block = Vec::new();

		while !self.current_is(Token::RightBrace) {
			block.push(self.parse_statement()?);
		}

		self.expect_token_and_read(Token::RightBrace)?;

		Ok(block)
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
		self.expect_token_and_read(Token::Identifier("".to_string()))
	}

	fn current_is(&self, token: Token) -> bool {
		std::mem::discriminant(&self.current) == std::mem::discriminant(&token)
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
}

#[derive(Debug, Error)]
pub enum ParseError {
	#[error("Unexpected token `{0:?}`.")]
	UnexpectedToken(Token),
	#[error("Unexpected token `{0:?}`, expected `{1:?}`")]
	UnexpectedTokenExpected(Token, Token),
	#[error("Entered unreachable code.")]
	Unreachable,
}

impl ParseError {
	pub fn print(self) {
		eprintln!("{}", format!("{}", self).red().bold());
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::token;

	fn lex_and_parse(input: &str) -> Program {
		let tokens = token::generate(input);
		parse(tokens).unwrap()
	}

	#[test]
	fn it_can_parse_fn_declarations() {
		assert_eq!(lex_and_parse("fn name() {}"), vec![Statement::FunctionDeclaration { name: String::from("name"), body: vec![], params: vec![] }]);

		assert_eq!(
			lex_and_parse("fn name(person) {}"),
			vec![Statement::FunctionDeclaration {
				name: String::from("name"),
				body: vec![],
				params: vec![Parameter { name: String::from("person") }]
			}]
		);

		assert_eq!(
			lex_and_parse("fn say_hello(name, separator) {}"),
			vec![Statement::FunctionDeclaration {
				name: String::from("say_hello"),
				body: vec![],
				params: vec![Parameter { name: String::from("name") }, Parameter { name: String::from("separator") }]
			}]
		);

		assert_eq!(
			lex_and_parse(
				"
                fn say_hello() {
                    create name = true
                }
            "
			),
			vec![Statement::FunctionDeclaration {
				name: String::from("say_hello"),
				body: vec![Statement::CreateDeclaration { name: String::from("name"), initial: Expression::Bool(true).some() }],
				params: vec![]
			}]
		)
	}

	#[test]
	fn it_can_parse_let_declarations() {
		assert_eq!(lex_and_parse("create name"), vec![Statement::CreateDeclaration { name: String::from("name"), initial: None }]);

		assert_eq!(
			lex_and_parse("create name = true"),
			vec![Statement::CreateDeclaration { name: String::from("name"), initial: Expression::Bool(true).some() }]
		);
	}

	#[test]
	fn it_can_parse_literals() {
		assert_eq!(
			lex_and_parse(r##"123 "testing" true false 123.456"##),
			vec![
				Statement::Expression { expression: Expression::Number(123.0) },
				Statement::Expression { expression: Expression::String("testing".to_owned()) },
				Statement::Expression { expression: Expression::Bool(true) },
				Statement::Expression { expression: Expression::Bool(false) },
				Statement::Expression { expression: Expression::Number(123.456) },
			]
		);
	}

	#[test]
	fn it_can_parse_mathematical_operations() {
		assert_eq!(
			lex_and_parse("1 + 2"),
			vec![Statement::Expression {
				expression: Expression::Infix(Box::new(Expression::Number(1.0)), Op::Add, Box::new(Expression::Number(2.0)))
			}]
		);

		assert_eq!(
			lex_and_parse("1 - 2"),
			vec![Statement::Expression {
				expression: Expression::Infix(Box::new(Expression::Number(1.0)), Op::Subtract, Box::new(Expression::Number(2.0)))
			}]
		);

		assert_eq!(
			lex_and_parse("1 * 2"),
			vec![Statement::Expression {
				expression: Expression::Infix(Box::new(Expression::Number(1.0)), Op::Multiply, Box::new(Expression::Number(2.0)))
			}]
		);

		assert_eq!(
			lex_and_parse("1 / 2"),
			vec![Statement::Expression {
				expression: Expression::Infix(Box::new(Expression::Number(1.0)), Op::Divide, Box::new(Expression::Number(2.0)))
			}]
		);

		assert_eq!(
			lex_and_parse("1 + 2 * 3"),
			vec![Statement::Expression {
				expression: Expression::Infix(
					Box::new(Expression::Number(1.0)),
					Op::Add,
					Box::new(Expression::Infix(Box::new(Expression::Number(2.0)), Op::Multiply, Box::new(Expression::Number(3.0)),))
				)
			}]
		);

		assert_eq!(
			lex_and_parse("1 + 2 * 3 / 3"),
			vec![Statement::Expression {
				expression: Expression::Infix(
					Box::new(Expression::Number(1.0)),
					Op::Add,
					Box::new(Expression::Infix(
						Box::new(Expression::Infix(Box::new(Expression::Number(2.0)), Op::Multiply, Box::new(Expression::Number(3.0)),)),
						Op::Divide,
						Box::new(Expression::Number(3.0)),
					),)
				)
			}]
		);
	}

	#[test]
	fn it_can_parse_call_expressions() {
		assert_eq!(
			lex_and_parse("hello()"),
			vec![Statement::Expression { expression: Expression::Call(Box::new(Expression::Identifier("hello".to_owned())), vec![]) }]
		);

		assert_eq!(
			lex_and_parse("hello(true)"),
			vec![Statement::Expression {
				expression: Expression::Call(Box::new(Expression::Identifier("hello".to_owned())), vec![Expression::Bool(true)])
			}]
		);

		assert_eq!(
			lex_and_parse("hello(true, 1234)"),
			vec![Statement::Expression {
				expression: Expression::Call(
					Box::new(Expression::Identifier("hello".to_owned())),
					vec![Expression::Bool(true), Expression::Number(1234.0)]
				)
			}]
		);
	}

	#[test]
	fn it_can_parse_if_statements() {
		assert_eq!(lex_and_parse("if true {}"), vec![Statement::If { condition: Expression::Bool(true), then: vec![], otherwise: None }]);

		assert_eq!(
			lex_and_parse(
				"if true {
                create number = 1
            }"
			),
			vec![Statement::If {
				condition: Expression::Bool(true),
				then: vec![Statement::CreateDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },],
				otherwise: None
			}]
		);

		assert_eq!(
			lex_and_parse(
				"if false {
                create number = 1
            } else {
                create number = 2
            }"
			),
			vec![Statement::If {
				condition: Expression::Bool(false),
				then: vec![Statement::CreateDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },],
				otherwise: Some(vec![Statement::CreateDeclaration { name: String::from("number"), initial: Some(Expression::Number(2.0)) },])
			}]
		);
	}
}
