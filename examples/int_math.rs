use prec::{Assoc, Climber, Expression, Rule, Token as PrecToken};
use std::fmt;

/*

	This example uses the `prec` crate to perform integer operations.
	It supports parentheses, addition, subtraction, division, multiplication, exponents, and an additional operator for rounded-up division.

*/

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
pub enum Operator {
	// Addition
	Add,
	// Subtraction
	Sub,
	// Multiplication
	Mul,
	// Division, rounding down (standard behavior)
	Div,
	// Division, rounding up
	DivUp,
	// Exponent
	Exp,
}

impl fmt::Display for Operator {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Operator::Add => "+",
				Operator::Sub => "-",
				Operator::Mul => "*",
				Operator::Div => "/",
				Operator::DivUp => "/u",
				Operator::Exp => "^",
			}
		)
	}
}

#[derive(Clone)]
pub enum Token {
	Paren(Box<Expression<Operator, Token>>),
	Num(i64),
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Token::Paren(expr) => write!(f, "( {} )", expr),
			Token::Num(num) => write!(f, "{}", num),
		}
	}
}

impl PrecToken<i64, ()> for Token {
	fn convert(self, _: &()) -> Result<i64, ()> {
		Ok(match self {
			Token::Paren(expr) => CLIMBER.process(expr.as_ref(), &())?,
			Token::Num(n) => n,
		})
	}
}

fn handler(lhs: Token, op: Operator, rhs: Token, _: &()) -> Result<Token, ()> {
	let lhs: i64 = lhs.convert(&())?;
	let rhs: i64 = rhs.convert(&())?;
	Ok(match op {
		Operator::Add => Token::Num(lhs + rhs),
		Operator::Sub => Token::Num(lhs - rhs),
		Operator::Mul => Token::Num(lhs * rhs),
		Operator::Div => Token::Num(lhs / rhs),
		Operator::DivUp => Token::Num((lhs as f64 / rhs as f64 + 0.5) as i64),
		Operator::Exp => Token::Num(lhs.overflowing_pow(rhs as u32).0),
	})
}

lazy_static::lazy_static! {
	pub static ref CLIMBER: Climber<Operator, Token, i64, ()> = Climber::new(
		vec![
			Rule::new(Operator::Add, Assoc::Left)
				| Rule::new(Operator::Sub, Assoc::Left),
			Rule::new(Operator::Mul, Assoc::Left)
				| Rule::new(Operator::Div, Assoc::Left)
				| Rule::new(Operator::DivUp, Assoc::Left),
			Rule::new(Operator::Exp, Assoc::Right),
		],
		handler,
	);
}

fn main() {
	use Operator::*;
	use Token::*;

	// 2 + 2
	// 4
	let expression = Expression::new(Num(2i64), vec![(Add, Num(2))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 8 * 2 + 1
	// 16 + 1
	// 17
	let expression =
		Expression::new(Num(8i64), vec![(Mul, Num(2)), (Add, Num(1))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 8 * ( 2 + 1 )
	// 8 * 3
	// 24
	let expression = Expression::new(
		Num(8i64),
		vec![(
			Mul,
			Paren(Box::new(Expression::new(Num(2), vec![(Add, Num(1))]))),
		)],
	);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 9 / 2
	// 4
	let expression = Expression::new(Num(9), vec![(Div, Num(2))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 9 /u 2
	// 5
	let expression = Expression::new(Num(9), vec![(DivUp, Num(2))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 5 ^ 2
	// 25
	let expression = Expression::new(Num(5), vec![(Exp, Num(2))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 1 + 5 ^ 3 + 1
	// 1 + 125 + 1
	// 127
	let expression =
		Expression::new(Num(1), vec![(Add, Num(5)), (Exp, Num(3)), (Add, Num(1))]);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);

	// 5 + 3 ^ ( 1 + 1) - 2 * (8-1) / 3
	// 5 + 3 ^ 2 - 2 * 7 / 3
	// 5 + 9 - 2 * 7 / 3
	// 5 + 9 - 14 / 3
	// 5 + 9 - 4
	// 10
	let expression = Expression::new(
		Num(5),
		vec![
			(Add, Num(3)),
			(
				Exp,
				Paren(Box::new(Expression::new(Num(1), vec![(Add, Num(1))]))),
			),
			(Sub, Num(2)),
			(
				Mul,
				Paren(Box::new(Expression::new(Num(8), vec![(Sub, Num(1))]))),
			),
			(Div, Num(3)),
		],
	);
	println!(
		"{} = {}",
		expression,
		CLIMBER.process(&expression, &()).unwrap()
	);
}
