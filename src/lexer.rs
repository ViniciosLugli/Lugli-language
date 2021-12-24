#[path = "token.rs"]
mod token;

pub use token::{Token, TokenKind};

#[derive(Clone)]
pub struct Lexer {
	source: Vec<char>,
	current: usize,
	next: usize,

	char_: char
}

impl Lexer {
	pub fn new(contents: String) -> Self {
		let mut lexer = Self {
			source: contents.chars().collect(),

			current: 0,
			next: 1,

			char_: '\0'
		};
		lexer.char_ = lexer.source[lexer.current];

		lexer
	}

	fn read(&mut self) {
		if self.next >= self.source.len() {
			self.char_ = '\0';
		} else {
			self.char_ = self.source[self.next];
		}

		self.current = self.next;
		self.next = self.current + 1;
	}

	fn skip_whitespace(&mut self) {
		while self.char_.is_whitespace() {
			self.read();
		}
	}
}

impl Iterator for Lexer {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		if self.next >= self.source.len() {
			return None;
		}

		self.skip_whitespace();

		let t: Token = match self.char_ {
			'=' => Token::new(TokenKind::Assign, "=".to_owned()),

			_ if self.char_.is_alphabetic() => {
				let mut buffer: String = String::new();

				buffer.push(self.char_);
				self.read();

				while self.char_.is_alphabetic() {
					buffer.push(self.char_);
					self.read();
				}

				// TODO: Add support for config/custom keywords
				let kind = match buffer.as_str() {
					"let" => TokenKind::VariableStatement,
					_ => TokenKind::Indentifier
				};

				Token::new(kind, buffer)
			}

			_ if self.char_.is_numeric() => {
				let mut buffer: String = String::new();

				buffer.push(self.char_);
				self.read();

				loop {
					if self.current >= self.source.len() {
						break;
					}

					if self.char_ == '_' {
						self.read();
					}

					if !self.char_.is_numeric() && self.char_ != '.' {
						break;
					}

					buffer.push(self.char_);
					self.read();
				}

				Token::new(TokenKind::Number, buffer)
			}

			_ => unimplemented!()
		};

		self.read();

		Some(t)
	}
}
