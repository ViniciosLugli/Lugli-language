#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
	VariableStatement,
	Indentifier,
	Assign,

	String,
	Number
}

#[derive(Debug, Clone)]
pub struct Token {
	pub token_type: TokenKind,
	pub literal: String
}

impl Token {
	pub fn new(token_type: TokenKind, literal: String) -> Self {
		Self {
			token_type,
			literal
		}
	}
}
