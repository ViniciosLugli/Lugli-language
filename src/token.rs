#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
	VariableStatement,
	Identifier,
	Assign,

	String,
	Number,

	Plus,
	Minus,
	Multiply,
	Divide,
	Modulo,
	Power
}

#[derive(Debug, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub literal: String
}

impl Token {
	pub fn new(kind: TokenKind, literal: String) -> Self {
		Self {
			kind,
			literal
		}
	}
}
