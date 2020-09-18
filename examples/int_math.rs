use prec::{Assoc, Climber, Expression, Rule};

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum Operator {
	Add,
	Sub,
	Mul,
	Div,
	DivUp,
	Exp,
}

#[derive(Clone)]
pub enum Token {
	Paren(Box<Expression<Operator, Token>>),
	Num(i64),
}

fn main() {}
