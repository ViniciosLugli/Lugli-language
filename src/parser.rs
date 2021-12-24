use crate::{
	ast::{BinaryOperator, Expression, Statement},
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

					let expression = self.parse_expression(0);

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

	fn parse_expression(&mut self, bp: u8) -> Expression {
		let mut lhs = match self.lexer.next() {
			Some(Token {
				kind: TokenKind::Number,
				literal
			}) => Expression::Number(literal.parse().unwrap()),
			_ => unimplemented!()
		};

		loop {
			let infix = if let Some(infix) = self.lexer.peek() {
				infix
			} else {
				break;
			};

			if let Some((lbp, rbp)) = infix_binding_power(infix.kind) {
				if lbp < bp {
					break;
				}

				let op = self.lexer.next().unwrap().kind;

				let rhs = self.parse_expression(rbp);

				lhs = make_infix_expression(lhs, op, rhs);

				continue;
			}
			break;
		}

		lhs
	}
}

fn make_infix_expression(lhs: Expression, operator: TokenKind, rhs: Expression) -> Expression {
	let lhs = Box::new(lhs);
	let rhs = Box::new(rhs);
	match operator {
		TokenKind::Plus => Expression::Binary(lhs, BinaryOperator::Plus, rhs),
		TokenKind::Minus => Expression::Binary(lhs, BinaryOperator::Minus, rhs),
		TokenKind::Multiply => Expression::Binary(lhs, BinaryOperator::Multiply, rhs),
		TokenKind::Divide => Expression::Binary(lhs, BinaryOperator::Divide, rhs),
		TokenKind::Modulo => Expression::Binary(lhs, BinaryOperator::Modulo, rhs),
		TokenKind::Power => Expression::Binary(lhs, BinaryOperator::Power, rhs),
		_ => unimplemented!()
	}
}

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
	let binding_power = match kind {
		TokenKind::Plus | TokenKind::Minus => (1, 2),
		TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo => (3, 4),
		TokenKind::Power => (5, 6),
		_ => return None
	};

	Some(binding_power)
}
