#[derive(Debug)]
pub enum Statement {
	Variable { name: String, initial: Expression }
}

#[derive(Debug)]
pub enum Expression {
	Number(f64)
}
