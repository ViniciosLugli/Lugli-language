use crate::token::{Token, TokenKind};

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

	fn match_token(&mut self) -> Token {
		self.skip_whitespace();

		match self.char_ {
			'=' => {
				self.read();
				Token::new(TokenKind::Assign, "=".to_owned())
			}

			'+' => {
				self.read();
				Token::new(TokenKind::Plus, "+".to_owned())
			}

			'*' => {
				self.read();
				Token::new(TokenKind::Multiply, "*".to_owned())
			}

			'-' => {
				self.read();
				Token::new(TokenKind::Minus, "-".to_owned())
			}

			'/' => {
				self.read();

				Token::new(TokenKind::Divide, "/".to_owned())
			}

			'%' => {
				self.read();
				Token::new(TokenKind::Modulo, "%".to_owned())
			}

			'^' => {
				self.read();
				Token::new(TokenKind::Power, "^".to_owned())
			}

			'\'' | '"' => {
				let mut buffer = String::new();
				let b_char = self.char_;

				self.read();

				while self.char_ != b_char {
					if self.current >= self.source.len() {
						panic!("Unterminated string literal.");
					}

					if self.char_ == '\\' {
						self.read();

						match self.char_ {
							'n' => buffer.push('\n'),
							'r' => buffer.push('\r'),
							't' => buffer.push('\t'),
							'\\' => buffer.push('\\'),
							'\'' | '"' => buffer.push(self.char_),
							_ => buffer.push(self.char_)
						}
					} else {
						buffer.push(self.char_);
						self.read();
					}
				}

				self.read();

				Token::new(TokenKind::String, buffer)
			}

			_ if self.char_.is_alphabetic() => {
				let mut buffer = String::new();
				buffer.push(self.char_);

				self.read();

				while self.char_.is_alphabetic() {
					if self.current >= self.source.len() {
						panic!("Unterminated string literal.");
					}

					buffer.push(self.char_);
					self.read();
				}

				// TODO: Add support for keywords
				let kind = match buffer.as_str() {
					"let" => TokenKind::VariableStatement,
					_ => TokenKind::Identifier
				};

				Token::new(kind, buffer)
			}
			_ if self.char_.is_numeric() => {
				let mut buffer = String::new();
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
		}
	}

	pub fn peek(&mut self) -> Option<Token> {
		if self.next >= self.source.len() {
			return None;
		}

		let old_current = self.current;
		let old_next = self.next;
		let old_char = self.char_;

		self.char_ = self.source[self.next];

		let token = self.match_token();

		self.current = old_current;
		self.next = old_next;
		self.char_ = old_char;

		Some(token)
	}
}

impl Iterator for Lexer {
	type Item = Token;

	fn next(&mut self) -> Option<Token> {
		if self.next >= self.source.len() {
			return None;
		}

		let token = self.match_token();

		Some(token)
	}
}
