use crate::ast::Program;
use crate::ast::{Argument, Block, CallArguments, ConditionBlock, Expression, Identifier, Op, Parameter, Statement};
use crate::token::Token;
use colored::Colorize;
use hashbrown::HashMap;
use std::slice::Iter;
use thiserror::Error;

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

#[derive(Debug, Error)]
pub enum ParseError {
	#[error("Unexpected token `{0:?}`")]
	UnexpectedToken(Token),

	#[error("Unexpected token `{0:?}`, expected `{1:?}`")]
	UnexpectedTokenExpected(Token, Token),

	#[error("Expected function declaration inside class")]
	ExpectedFunctionDeclarationClass,

	#[error("Unexpected token in class declaration")]
	UnexpectedTokenClass,

	#[error("Unexpected end of file")]
	UnexpectedEof,

	#[error("Entered unreachable code.")]
	Unreachable,
}

impl ParseError {
	pub fn print(self) {
		eprintln!("{}", format!("{}", self).red().bold());
	}
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
			Token::Create => self.parse_variable(),
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
		self.expect_token_and_read(Token::LeftBrace)?;

		let mut statements = Vec::new();

		while !self.current_is(Token::RightBrace) && !self.current_is(Token::Eof) {
			statements.push(self.parse_statement()?);
		}

		self.expect_token_and_read(Token::RightBrace)?;

		Ok(statements)
	}

	fn parse_condition(&mut self) -> Result<Expression, ParseError> {
		if self.current_is(Token::LeftParen) {
			self.expect_token_and_read(Token::LeftParen)?;
			let condition = self.parse_expression(Precedence::Statement)?;
			self.expect_token_and_read(Token::RightParen)?;
			Ok(condition)
		} else {
			self.parse_expression(Precedence::Statement)
		}
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

	fn parse_if(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::If)?;

		let condition_if = self.parse_condition()?;

		let then_if = self.parse_block()?;

		let others_conditions = self.parse_else_if_conditions()?;

		let otherwise = self.parse_else_block()?;

		Ok(Statement::If { condition: ConditionBlock { expression: condition_if, then: then_if }, others_conditions, otherwise })
	}

	fn parse_else_if_conditions(&mut self) -> Result<Option<Vec<ConditionBlock>>, ParseError> {
		if self.current_is(Token::ElseIf) {
			let mut others_conditions: Vec<ConditionBlock> = Vec::new();

			while self.current_is(Token::ElseIf) {
				self.expect_token_and_read(Token::ElseIf)?;
				let condition_else_if = self.parse_condition()?;
				others_conditions.push(ConditionBlock { expression: condition_else_if, then: self.parse_block()? });
			}

			Ok(Some(others_conditions))
		} else {
			Ok(None)
		}
	}

	fn parse_else_block(&mut self) -> Result<Option<Block>, ParseError> {
		if self.current_is(Token::Else) {
			self.expect_token_and_read(Token::Else)?;
			Ok(Some(self.parse_block()?))
		} else {
			Ok(None)
		}
	}

	fn parse_for(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::For)?;

		let (index, value) = self.parse_for_loop_variables()?;

		self.expect_token_and_read(Token::In)?;

		let iterable = self.parse_expression(Precedence::Statement)?;
		let then = self.parse_block()?;

		Ok(Statement::For { index, value, iterable, then })
	}

	fn parse_for_loop_variables(&mut self) -> Result<(Option<String>, String), ParseError> {
		if self.current_is(Token::LeftParen) {
			self.expect_token_and_read(Token::LeftParen)?;
			let index = Some(self.expect_identifier_and_read()?);
			self.expect_token_and_read(Token::Comma)?;
			let value = self.expect_identifier_and_read()?;
			self.expect_token_and_read(Token::RightParen)?;
			Ok((index.map(|i| i.into()), value.into()))
		} else {
			let value = self.expect_identifier_and_read()?;
			Ok((None, value.into()))
		}
	}

	fn parse_while(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::While)?;

		let condition = self.parse_condition()?;

		let then = self.parse_block()?;

		Ok(Statement::While { condition: ConditionBlock { expression: condition, then } })
	}

	fn parse_loop(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Loop)?;

		let then = self.parse_block()?;

		Ok(Statement::Loop { body: then })
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

	fn parse_variable(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Create)?;

		let name: Identifier = self.expect_identifier_and_read()?.into();
		let initial: Option<Expression> = if self.current_is(Token::Assign) {
			self.expect_token_and_read(Token::Assign)?;

			Some(self.parse_expression(Precedence::Lowest)?)
		} else {
			None
		};

		Ok(Statement::VariableDeclaration { name, initial })
	}

	fn parse_constant(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Constant)?;

		let name: Identifier = self.expect_identifier_and_read()?.into();
		self.expect_token_and_read(Token::Assign)?;

		let initial = self.parse_expression(Precedence::Lowest)?;

		Ok(Statement::ConstantDeclaration { name, initial })
	}

	fn parse_class(&mut self) -> Result<Statement, ParseError> {
		self.expect_token_and_read(Token::Class)?;
		let name: Identifier = self.expect_identifier_and_read()?.into();
		self.expect_token_and_read(Token::LeftBrace)?;

		let mut fields: Vec<Parameter> = Vec::new();

		while !self.current_is(Token::RightBrace) {
			match self.current {
				Token::Function => {
					let function = self.parse_function(true)?;
					if let Statement::FunctionDeclaration { name, params, body } = function {
						fields.push(Parameter { name, default: Some(Expression::Closure(params, body)) });
					} else {
						return Err(ParseError::ExpectedFunctionDeclarationClass);
					}
				}
				Token::Identifier(_) => {
					let field: String = self.expect_identifier_and_read()?.into();
					let default = if self.current_is(Token::Assign) {
						self.expect_token_and_read(Token::Assign)?;
						Some(self.parse_expression(Precedence::Lowest)?)
					} else {
						None
					};
					fields.push(Parameter { name: field, default });
				}
				Token::Comma | Token::RightBrace => (),
				_ => return Err(ParseError::UnexpectedTokenClass),
			}
		}

		self.expect_token_and_read(Token::RightBrace)?;
		Ok(Statement::ClassDeclaration { name, fields })
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
	fn it_can_parse_function_declarations() {
		assert_eq!(lex_and_parse("fn name() {}"), vec![Statement::FunctionDeclaration { name: String::from("name"), body: vec![], params: vec![] }]);

		assert_eq!(
			lex_and_parse("fn name(person) {}"),
			vec![Statement::FunctionDeclaration {
				name: String::from("name"),
				body: vec![],
				params: vec![Parameter { name: String::from("person"), default: None }]
			}]
		);

		assert_eq!(
			lex_and_parse("fn say_hello(name, separator) {}"),
			vec![Statement::FunctionDeclaration {
				name: String::from("say_hello"),
				body: vec![],
				params: vec![Parameter { name: String::from("name"), default: None }, Parameter { name: String::from("separator"), default: None }]
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
				body: vec![Statement::VariableDeclaration { name: String::from("name"), initial: Some(Expression::Bool(true)) }],
				params: vec![]
			}]
		)
	}

	#[test]
	fn it_can_parse_create_declarations_and_constants() {
		assert_eq!(lex_and_parse("create name"), vec![Statement::VariableDeclaration { name: String::from("name"), initial: None }]);

		assert_eq!(
			lex_and_parse("create bool = true"),
			vec![Statement::VariableDeclaration { name: String::from("bool"), initial: Some(Expression::Bool(true)) }]
		);

		assert_eq!(
			lex_and_parse("constant bool = false"),
			vec![Statement::ConstantDeclaration { name: String::from("bool"), initial: Expression::Bool(false) }]
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
			vec![Statement::Expression { expression: Expression::Call(Box::new(Expression::Identifier("hello".to_owned())), CallArguments::new()) }]
		);

		assert_eq!(
			lex_and_parse("hello(true)"),
			vec![Statement::Expression {
				expression: Expression::Call(
					Box::new(Expression::Identifier("hello".to_owned())),
					CallArguments::new().with_argument(Argument::new(None, Expression::Bool(true)))
				)
			}]
		);

		assert_eq!(
			lex_and_parse("hello(true, 1234)"),
			vec![Statement::Expression {
				expression: Expression::Call(
					Box::new(Expression::Identifier("hello".to_owned())),
					CallArguments::new()
						.with_argument(Argument::new(None, Expression::Bool(true)))
						.with_argument(Argument::new(None, Expression::Number(1234.0)))
				)
			}]
		);
	}

	#[test]
	fn it_can_parse_if_statements() {
		assert_eq!(
			lex_and_parse("if true {}"),
			vec![Statement::If {
				condition: ConditionBlock { expression: Expression::Bool(true), then: vec![] },
				others_conditions: None,
				otherwise: None
			}]
		);

		assert_eq!(
			lex_and_parse(
				"if true {
					create number = 1
				}"
			),
			vec![Statement::If {
				condition: ConditionBlock {
					expression: Expression::Bool(true),
					then: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) }]
				},
				others_conditions: None,
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
				condition: ConditionBlock {
					expression: Expression::Bool(false),
					then: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },]
				},
				others_conditions: None,
				otherwise: Some(vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(2.0)) },])
			}]
		);

		assert_eq!(
			lex_and_parse(
				"if false {
					create number = 3
				} elif true{
					create number = 6
				}else {
					create number = 9
				}"
			),
			vec![Statement::If {
				condition: ConditionBlock {
					expression: Expression::Bool(false),
					then: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(3.0)) },]
				},
				others_conditions: Some(vec![ConditionBlock {
					expression: Expression::Bool(true),
					then: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(6.0)) },]
				}]),
				otherwise: Some(vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(9.0)) },])
			}]
		);
	}

	#[test]
	fn it_can_parse_while_statements() {
		assert_eq!(
			lex_and_parse("while true {}"),
			vec![Statement::While { condition: ConditionBlock { expression: Expression::Bool(true), then: vec![] } }]
		);

		assert_eq!(
			lex_and_parse(
				"while true {
					create number = 1
				}"
			),
			vec![Statement::While {
				condition: ConditionBlock {
					expression: Expression::Bool(true),
					then: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) }]
				}
			}]
		);

		assert_eq!(
			lex_and_parse(
				"while true {
					break
					create number = 1
				}"
			),
			vec![Statement::While {
				condition: ConditionBlock {
					expression: Expression::Bool(true),
					then: vec![
						Statement::Break,
						Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },
					]
				},
			}]
		);

		assert_eq!(
			lex_and_parse(
				"while true {
					if true {
						continue
					}
					# never touch this create number
					create number = 1
				}"
			),
			vec![Statement::While {
				condition: ConditionBlock {
					expression: Expression::Bool(true),
					then: vec![
						Statement::If {
							condition: ConditionBlock { expression: Expression::Bool(true), then: vec![Statement::Continue] },
							others_conditions: None,
							otherwise: None
						},
						Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },
					]
				},
			}]
		);
	}

	#[test]
	fn it_can_parse_loop_statements() {
		assert_eq!(lex_and_parse("loop {}"), vec![Statement::Loop { body: vec![] }]);

		assert_eq!(
			lex_and_parse(
				"loop {
					create number = 1
				}"
			),
			vec![Statement::Loop {
				body: vec![Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) }]
			}]
		);

		assert_eq!(
			lex_and_parse(
				"loop {
					break
					create number = 1
				}"
			),
			vec![Statement::Loop {
				body: vec![Statement::Break, Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },]
			}]
		);

		assert_eq!(
			lex_and_parse(
				"loop {
					if true {
						continue
					}
					-- never touch this create number
					create number = 1
				}"
			),
			vec![Statement::Loop {
				body: vec![
					Statement::If {
						condition: ConditionBlock { expression: Expression::Bool(true), then: vec![Statement::Continue] },
						others_conditions: None,
						otherwise: None
					},
					Statement::VariableDeclaration { name: String::from("number"), initial: Some(Expression::Number(1.0)) },
				]
			}]
		);
	}

	#[test]
	fn it_can_parse_class_declarations() {
		assert_eq!(
			lex_and_parse(
				"class Point {
					x, y
				}"
			),
			vec![Statement::ClassDeclaration {
				name: String::from("Point"),
				fields: vec![Parameter { name: String::from("x"), default: None }, Parameter { name: String::from("y"), default: None }]
			}]
		);

		let mut struct_fields: HashMap<Identifier, Expression> = HashMap::new();
		struct_fields.insert(Identifier::from("name"), Expression::Identifier("name".to_owned()));
		struct_fields.insert(Identifier::from("email"), Expression::Identifier("email".to_owned()));

		assert_eq!(
			lex_and_parse(
				"class Person {
					name,
					email
				}

				Person.new = fn (name, email) {
					return Person { name, email }
				}"
			),
			vec![
				Statement::ClassDeclaration {
					name: "Person".to_owned(),
					fields: vec![Parameter { name: "name".to_owned(), default: None }, Parameter { name: "email".to_owned(), default: None }]
				},
				Statement::Expression {
					expression: Expression::Assign(
						Box::new(Expression::Get(Box::new(Expression::Identifier("Person".to_owned())), "new".to_owned())),
						Box::new(Expression::Closure(
							vec![Parameter { name: "name".to_owned(), default: None }, Parameter { name: "email".to_owned(), default: None }],
							vec![Statement::Return {
								value: Expression::Class(Box::new(Expression::Identifier("Person".to_owned())), struct_fields)
							}]
						))
					)
				}
			]
		);
	}
}
