use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Climber<Ru: Hash + Eq + Copy, To: Into<Re> + Clone, Re> {
	rules: HashMap<Ru, (usize, Assoc)>,
	p_rule_value: PhantomData<Ru>,
	p_token: PhantomData<To>,
	p_result: PhantomData<Re>,
}

impl<Ru: Hash + Eq + Copy, To: Into<Re> + Clone, Re> Climber<Ru, To, Re> {
	pub fn new(rules: Vec<Rule<Ru>>) -> Self {
		let rules = rules.into_iter().zip(1..).fold(
			HashMap::new(),
			|mut map, (val, prec)| {
				let mut next = Some(val);
				while let Some(val) = next.take() {
					match val {
						Rule {
							val,
							assoc,
							next: val_next,
						} => {
							map.insert(val, (prec, assoc));
							next = val_next.map(|val| *val);
						}
					}
				}
				map
			},
		);
		Self {
			rules,
			p_rule_value: PhantomData,
			p_token: PhantomData,
			p_result: PhantomData,
		}
	}

	pub fn process<H: FnMut(To, Ru, To) -> To>(
		&self,
		expr: &Expression<Ru, To>,
		mut handler: H,
	) -> Re {
		let mut primary = expr.first_token.clone().into();
		let lhs = expr.first_token.clone();
		let mut tokens = expr.pairs.iter().peekable();
		self
			.process_rec(
				lhs, //
				0,
				&mut primary,
				&mut tokens,
				&mut handler,
			)
			.into()
	}

	fn process_rec<H: FnMut(To, Ru, To) -> To>(
		&self,
		mut lhs: To,
		min_prec: usize,
		primary: &mut Re,
		tokens: &mut std::iter::Peekable<std::slice::Iter<(Ru, To)>>,
		handler: &mut H,
	) -> To {
		while let Some((rule, token)) = tokens.peek() {
			if let Some(&(prec, _)) = self.rules.get(rule) {
				if prec >= min_prec {
					let (op, rhs_ref) = tokens.next().unwrap();
					let mut rhs = rhs_ref.clone();

					while let Some((peek_rule, peek_token)) = tokens.peek() {
						if let Some(&(peek_prec, peek_assoc)) = self.rules.get(peek_rule) {
							if peek_prec > prec
								|| peek_assoc == Assoc::Right && peek_prec == prec
							{
								rhs =
									self.process_rec(rhs, peek_prec, primary, tokens, handler);
							} else {
								break;
							}
						} else {
							break;
						}
					}
					lhs = handler(lhs, *rule, rhs);
				} else {
					break;
				}
			} else {
				break;
			}
		}
		lhs
	}
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Assoc {
	Left,
	Right,
}

#[derive(Debug)]
pub struct Rule<Ru> {
	val: Ru,
	assoc: Assoc,
	next: Option<Box<Rule<Ru>>>,
}

impl<Ru> Rule<Ru> {
	pub fn new(val: Ru, assoc: Assoc) -> Self {
		Self {
			val,
			assoc,
			next: None,
		}
	}
}

impl<Ru> std::ops::BitOr for Rule<Ru> {
	type Output = Self;
	fn bitor(mut self, rhs: Self) -> Self {
		fn assign_next<Ru>(op: &mut Rule<Ru>, next: Rule<Ru>) {
			if let Some(ref mut child) = op.next {
				assign_next(child, next);
			} else {
				op.next = Some(Box::new(next));
			}
		}
		assign_next(&mut self, rhs);
		self
	}
}

#[derive(Debug, Clone)]
pub struct Expression<Ru: Copy, To: Clone> {
	pub first_token: To,
	pub pairs: Vec<(Ru, To)>,
}

impl<Ru: Copy, To: Clone> Expression<Ru, To> {
	pub fn new(first_token: To, pairs: Vec<(Ru, To)>) -> Self {
		Self { first_token, pairs }
	}
}

impl<Ru: Copy + fmt::Display, To: Clone + fmt::Display> fmt::Display
	for Expression<Ru, To>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut s = format!("{}", self.first_token);
		for (o, t) in &self.pairs {
			s = format!("{} {} {}", s, o, t);
		}
		write!(f, "{}", s)
	}
}

#[cfg(test)]
mod test {
	use super::*;

	fn c(expression: &Expression<MathOperator, MathToken>) -> f32 {
		use MathOperator::*;
		let climber = Climber::new(vec![
			Rule::new(Add, Assoc::Left) | Rule::new(Sub, Assoc::Left),
			Rule::new(Mul, Assoc::Left) | Rule::new(Div, Assoc::Left),
		]);
		climber.process(&expression, |lhs, op, rhs| {
			let lhs: f32 = lhs.into();
			let rhs: f32 = rhs.into();
			match op {
				MathOperator::Add => MathToken::Num(lhs + rhs),
				MathOperator::Sub => MathToken::Num(lhs - rhs),
				MathOperator::Mul => MathToken::Num(lhs * rhs),
				MathOperator::Div => MathToken::Num(lhs / rhs),
			}
		})
	}

	#[derive(Hash, Eq, PartialEq, Copy, Clone)]
	pub enum MathOperator {
		Add,
		Sub,
		Mul,
		Div,
	}

	#[derive(Clone)]
	pub enum MathToken {
		Paren(Box<Expression<MathOperator, MathToken>>),
		Num(f32),
	}

	impl Into<f32> for MathToken {
		fn into(self) -> f32 {
			match self {
				MathToken::Paren(expr) => c(expr.as_ref()),
				MathToken::Num(n) => n,
			}
		}
	}

	#[test]
	fn process() {
		let res = c(&Expression::new(
			MathToken::Num(7.0),
			vec![(MathOperator::Add, MathToken::Num(3.0))],
		));

		assert_eq!(res, 10.0);
	}
	#[test]
	fn proces_complex() {
		use MathOperator::*;
		use MathToken::*;
		let res = c(&Expression::new(
			Num(10.0),
			vec![(Add, Num(5.0)), (Mul, Num(3.0)), (Add, Num(1.0))],
		));
		println!("{}", res);
	}
}
