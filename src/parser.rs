use crate::{
	ast::{Expression, Statement},
	lexer::Lexer,
	token::{Token, TokenKind}
};

pub type Program = Vec<Statement>;

pub struct Parser {
	lexer: Lexer
}

impl Parser {
	pub fn new(lexer: Lexer) -> Self {
		Self {
			lexer
		}
	}

	pub fn parse(&mut self) -> Program {
		let mut statements: Vec<Statement> = Vec::<Statement>::new();

		while let Some(token) = self.lexer.next() {
			match token.kind {
				TokenKind::VariableStatement => {
					let identifier = if let Some(identifier) = self.lexer.next() {
						identifier
					} else {
						panic!("Expected an identifier.");
					};

					if !matches!(
						self.lexer.peek(),
						Some(Token {
							kind: TokenKind::Assign,
							..
						})
					) {
						panic!("Expected an `=` for assignment.");
					}

					self.lexer.next();

					let expression = self.parse_expression();

					statements.push(Statement::Variable {
						name: identifier.literal,
						initial: expression
					});
				}
				_ => unimplemented!()
			};
		}

		statements
	}

	fn parse_expression(&mut self) -> Expression {
		match self.lexer.next() {
			Some(Token {
				kind: TokenKind::Number,
				literal
			}) => Expression::Number(literal.parse().unwrap()),
			_ => unimplemented!()
		}
	}
}
