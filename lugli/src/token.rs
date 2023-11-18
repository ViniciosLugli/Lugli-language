use std::clone;
use std::fmt;

use logos::{Lexer, Logos};

mod sanitizers {
	use super::*;

	pub fn to_string(lex: &mut Lexer<Token>) -> Option<String> {
		let slice = lex.slice();
		if slice.starts_with('"') && slice.ends_with('"') {
			Some(slice[1..slice.len() - 1].to_string())
		} else {
			Some(slice.to_string())
		}
	}

	pub fn to_float(lex: &Lexer<Token>) -> Option<f64> {
		lex.slice().parse().ok()
	}
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"#[^\n]*")]
pub enum Token {
	#[token("fn")]
	Function,
	#[token("create")]
	Create,
	#[token("constant")]
	Constant,
	#[token("if")]
	If,
	#[token("elif")]
	ElseIf,
	#[token("else")]
	Else,
	#[token("class")]
	Class,
	#[token("while")]
	While,
	#[token("loop")]
	Loop,
	#[token("return")]
	Return,
	#[token("break")]
	Break,
	#[token("continue")]
	Continue,
	#[token("for")]
	For,
	#[token("in")]
	In,
	#[token("not in")]
	NotIn,

	#[token("+=")]
	PlusAssign,
	#[token("-=")]
	MinusAssign,
	#[token("*=")]
	MultiplyAssign,
	#[token("/=")]
	DivideAssign,

	#[token("++")]
	Increment,
	#[token("--")]
	Decrement,

	#[token("+")]
	Plus,
	#[token("-")]
	Minus,
	#[token("*")]
	Asterisk,
	#[token("/")]
	Slash,
	#[token("%")]
	Percent,
	#[token("**")]
	Pow,

	#[token("=")]
	Assign,
	#[token("==")]
	Equals,
	#[token("!=")]
	NotEquals,
	#[token("<")]
	LessThan,
	#[token(">")]
	GreaterThan,
	#[token("<=")]
	LessThanOrEquals,
	#[token(">=")]
	GreaterThanOrEquals,

	#[token("&&")]
	And,
	#[token("||")]
	Or,

	#[regex(r"[a-zA-Z_?!]+", sanitizers::to_string)]
	Identifier(String),
	#[regex(r##""(?:[^"\\]|\\.)*""##, sanitizers::to_string)]
	String(String),
	#[regex(r"([0-9]+[.])?[0-9]+", sanitizers::to_float)]
	Number(f64),

	#[token("(")]
	LeftParen,
	#[token(")")]
	RightParen,
	#[token("{")]
	LeftBrace,
	#[token("}")]
	RightBrace,
	#[token("[")]
	LeftBracket,
	#[token("]")]
	RightBracket,

	#[token("false", |_| false)]
	#[token("true", |_| true)]
	Bool(bool),
	#[token("null")]
	Null,

	#[token(",")]
	Comma,
	#[token(":")]
	Colon,
	#[token("!")]
	Bang,
	#[token(".")]
	Dot,

	Eof,
}

impl Into<String> for Token {
	fn into(self) -> String {
		match self {
			Token::Identifier(s) => s,
			Token::String(s) => s,
			_ => unreachable!(),
		}
	}
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position(usize, usize);

impl fmt::Display for Position {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}:{}", self.0, self.1)
	}
}

pub struct LexerWrapper<'a> {
	lexer: Lexer<'a, Token>,
}

impl<'a> LexerWrapper<'a> {
	pub fn new(input: &'a str) -> Self {
		Self { lexer: Token::lexer(input) }
	}

	pub fn next(&mut self) -> Option<Token> {
		self.lexer.next().map(|t| t.unwrap())
	}

	pub fn peek(&mut self) -> Option<Token> {
		self.lexer.clone().next().map(|t| t.unwrap())
	}

	pub fn slice(&self) -> &str {
		self.lexer.slice()
	}

	pub fn span(&self) -> std::ops::Range<usize> {
		self.lexer.span()
	}

	pub fn source(&self) -> &str {
		self.lexer.source()
	}

	pub fn generate_all_tokens(&mut self) -> Vec<Token> {
		let mut tokens = Vec::new();

		while let Some(token) = self.next() {
			tokens.push(token);
		}

		tokens
	}

	pub fn position(&self) -> Position {
		let mut line = 1;
		let mut column = 1;

		for c in self.lexer.source()[..self.lexer.span().start].chars() {
			if c == '\n' {
				line += 1;
				column = 1;
			} else {
				column += 1;
			}
		}

		Position(line, column)
	}
}

#[cfg(test)]
mod token_tests {
	use super::*;

	fn test_tokenizer(input: &str, expected_tokens: &[Token]) {
		let mut lexer = LexerWrapper::new(input);

		for expected_token in expected_tokens {
			match lexer.next() {
				Some(token) => assert_eq!(&token, expected_token),
				None => panic!("Unexpected end of tokens"),
			}
		}

		assert!(lexer.next().is_none(), "More tokens than expected");
	}

	#[test]
	fn test_get_line_column() {
		let mut lexer = LexerWrapper::new(
			"if false {
				create number = 3
			} elif true{
				create number = 6
			}else {
				create number = 9
			}",
		);

		let token = lexer.next().unwrap();
		let token = lexer.next().unwrap();
		let token = lexer.next().unwrap();
		let token = lexer.next().unwrap();
		let token = lexer.next().unwrap();

		assert_eq!(lexer.position(), Position(2, 12));
	}

	#[test]
	fn it_can_skip_comments() {
		test_tokenizer("# foo", &[]);
	}

	#[test]
	fn it_can_recognise_keywords() {
		let keywords = [
			Token::Function,
			Token::Constant,
			Token::Create,
			Token::Bool(true),
			Token::Bool(false),
			Token::If,
			Token::Else,
			Token::While,
			Token::For,
			Token::Class,
			Token::ElseIf,
			Token::Loop,
		];

		test_tokenizer("fn constant create true false if else while for class elif loop", &keywords);
	}

	#[test]
	fn it_can_recognise_symbols() {
		let symbols = [
			Token::LeftParen,
			Token::RightParen,
			Token::LeftBrace,
			Token::RightBrace,
			Token::Plus,
			Token::Minus,
			Token::Asterisk,
			Token::Slash,
			Token::Assign,
			Token::Equals,
			Token::NotEquals,
			Token::Colon,
			Token::Dot,
		];

		test_tokenizer("( ) { } +-*/ = == != : .", &symbols);
	}

	#[test]
	fn it_can_recognise_symbol_equals() {
		let symbol_equals = [Token::PlusAssign, Token::MinusAssign, Token::MultiplyAssign, Token::DivideAssign];

		test_tokenizer("+= -= *= /=", &symbol_equals);
	}

	#[test]
	fn it_can_recognise_identifiers() {
		let identifiers = [
			Token::Identifier("hello_world".to_owned()),
			Token::Identifier("HelloWorld".to_owned()),
			Token::Identifier("hello_world?".to_owned()),
			Token::Identifier("helloWorld".to_owned()),
		];

		test_tokenizer("hello_world HelloWorld hello_world? helloWorld", &identifiers);
	}

	#[test]
	fn it_can_recognise_numbers() {
		let numbers = [Token::Number(12345.0), Token::Number(6789.01), Token::Number(12345.6789), Token::Number(0.0)];

		test_tokenizer("12345 6789.01 12345.6789 0", &numbers);
	}

	#[test]
	fn it_can_recognise_strings() {
		let strings =
			[Token::String(r##"testing"##.to_owned()), Token::String(r##"testing with \""##.to_owned()), Token::String(r##"testing \n"##.to_owned())];

		test_tokenizer(r##""testing" "testing with \"" "testing \n""##, &strings);
	}

	#[test]
	fn it_can_recognise_full_code() {
		let code = r##"
			fn main() {
				constant a = 1
				create b = 2
				create c = a + b
				print(c)
			}
		"##;

		let tokens = [
			Token::Function,
			Token::Identifier("main".to_owned()),
			Token::LeftParen,
			Token::RightParen,
			Token::LeftBrace,
			Token::Constant,
			Token::Identifier("a".to_owned()),
			Token::Assign,
			Token::Number(1.0),
			Token::Create,
			Token::Identifier("b".to_owned()),
			Token::Assign,
			Token::Number(2.0),
			Token::Create,
			Token::Identifier("c".to_owned()),
			Token::Assign,
			Token::Identifier("a".to_owned()),
			Token::Plus,
			Token::Identifier("b".to_owned()),
			Token::Identifier("print".to_owned()),
			Token::LeftParen,
			Token::Identifier("c".to_owned()),
			Token::RightParen,
			Token::RightBrace,
		];

		test_tokenizer(code, &tokens);
	}
}
