use logos::{Lexer, Logos};

mod sanitizers {
	use super::*;

	pub fn to_string(lex: &mut Lexer<Token>) -> Option<String> {
		let slice = lex.slice();
		if slice.starts_with('"') && slice.ends_with('"') {
			Some(slice[1..slice.len() - 1].replace("\\\"", "\""))
		} else {
			Some(slice.to_string())
		}
	}

	pub fn to_float(lex: &Lexer<Token>) -> Option<f64> {
		lex.slice().parse().ok()
	}
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"#[^\n]*")]
pub enum Token {
	#[token("fn")]
	Fn,
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
	#[token("struct")]
	Struct,
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

	#[token("true")]
	True,
	#[token("false")]
	False,
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

pub fn generate(input: &str) -> Vec<Token> {
	let lexer = Token::lexer(input);
	lexer.filter_map(Result::ok).collect()
}

#[cfg(test)]
mod token_tests {
	use super::*;

	fn test_lexer(input: &str, expected_tokens: &[Token]) {
		let mut lexer = Token::lexer(input);

		for expected_token in expected_tokens {
			match lexer.next() {
				Some(Ok(token)) => assert_eq!(&token, expected_token),
				Some(Err(e)) => panic!("Lexer error: {:?}", e),
				None => panic!("Unexpected end of tokens"),
			}
		}

		assert!(lexer.next().is_none(), "More tokens than expected");
	}

	#[test]
	fn it_can_skip_comments() {
		test_lexer("# foo", &[]);
	}

	#[test]
	fn it_can_recognise_keywords() {
		let keywords = [
			Token::Fn,
			Token::Constant,
			Token::Create,
			Token::True,
			Token::False,
			Token::If,
			Token::Else,
			Token::While,
			Token::For,
			Token::Struct,
			Token::ElseIf,
			Token::Loop,
		];

		test_lexer("fn constant create true false if else while for struct elif loop", &keywords);
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

		test_lexer("( ) { } +-*/ = == != : .", &symbols);
	}

	#[test]
	fn it_can_recognise_symbol_equals() {
		let symbol_equals = [Token::PlusAssign, Token::MinusAssign, Token::MultiplyAssign, Token::DivideAssign];

		test_lexer("+= -= *= /=", &symbol_equals);
	}

	#[test]
	fn it_can_recognise_identifiers() {
		let identifiers = [
			Token::Identifier("hello_world".to_owned()),
			Token::Identifier("HelloWorld".to_owned()),
			Token::Identifier("hello_world?".to_owned()),
			Token::Identifier("helloWorld".to_owned()),
		];

		test_lexer("hello_world HelloWorld hello_world? helloWorld", &identifiers);
	}

	#[test]
	fn it_can_recognise_numbers() {
		let numbers = [Token::Number(12345.0), Token::Number(6789.01), Token::Number(12345.6789), Token::Number(0.0)];

		test_lexer("12345 6789.01 12345.6789 0", &numbers);
	}

	#[test]
	fn it_can_recognise_strings() {
		let strings = [
			Token::String(r##"testing"##.to_owned()),
			Token::String(r##"testing with \""##.to_owned()),
			Token::String(r##"testing \n"##.to_owned()),
		];

		test_lexer(r##""testing" "testing with \"" "testing \n""##, &strings);
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
			Token::Fn,
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

		test_lexer(code, &tokens);
	}
}
