#[derive(Debug)]
pub enum Statement {
	Variable { name: String, initial: Expression }
}

#[derive(Debug)]
pub enum Expression {
	Number(f64),

	Binary(Box<Expression>, BinaryOperator, Box<Expression>)
}

#[derive(Debug)]
pub enum BinaryOperator {
	Plus,
	Minus,
	Multiply,
	Divide,
	Modulo,
	Power
}
