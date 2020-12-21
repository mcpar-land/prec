//! A crate to enable easy use of the [precedence climbing][1] algorithm.
//! You plug your own handler function, token struct, and operator enum,
//! and this crate provides the algorithm.
//!
//! Because the crate is sufficiently generic, It's possible to add
//! very complex behavior without having to roll your own algorith.
//! the [`int_math`][2] example demonstrates adding parenteses that respect ordering.
//!
//! The relationships between the required structures you provide are enforced
//! by std library traits, which will let your structures play well with
//! your existing code base.
//!
//! [1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
//! [2]: https://github.com/mcpar-land/prec/blob/master/examples/int_math.rs

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::marker::PhantomData;

pub trait Token<Re, Err, Ctx = ()> {
	fn convert(self, ctx: &Ctx) -> Result<Re, Err>;
}

/// A struct containing the order of operations rules and a pointer to a handler function.
///
/// # Generics
/// Has three generics:
/// ## `Op`
/// An enum or other value that is used to specify different rules.
///
/// Required implementations: [Hash], [Eq], [Copy]
///
/// ## `To`
/// A token used in expressions between operators.
///
/// Required implementations: [Into<Re>], [Clone]
///
/// ## `Re`
/// A result value returned from the `compute` function.
///
/// ## `Err`
/// The error type returned in results.
///
/// ## `Ctx`
/// A context value made available across an entire expression while evaluating.
/// Entirely optional, defaults to `()`
///
///
/// [Hash]: https://doc.rust-lang.org/std/hash/index.html
/// [Eq]: https://doc.rust-lang.org/std/cmp/trait.Eq.ht
/// [Copy]: https://doc.rust-lang.org/std/marker/trait.Copy.html
/// [Clone]: https://doc.rust-lang.org/std/clone/trait.Clone.html
pub struct Climber<
	Op: Hash + Eq + Copy,
	To: Token<Re, Err, Ctx> + Clone,
	Re,
	Err,
	Ctx = (),
> {
	/// A map of [Rule](struct.Rule.html) s.
	///
	/// [1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
	pub rules: HashMap<Op, (usize, Assoc)>,
	/// Function to handle the result of an operator between two tokens.
	///
	/// Arguments are:
	/// - Left-hand side token
	/// - Operator
	/// - Right-hand side token
	pub handler: fn(To, Op, To, &Ctx) -> Result<To, Err>,
	p_rule_value: PhantomData<Op>,
	p_token: PhantomData<To>,
	p_result: PhantomData<Re>,
	p_ctx: PhantomData<Ctx>,
}

impl<Op: Hash + Eq + Copy, To: Token<Re, Err, Ctx> + Clone, Re, Err, Ctx>
	Climber<Op, To, Re, Err, Ctx>
{
	/// Construtor for a new climber.
	/// Rules with the same [precedence level][1] are separated by a `|` character.
	/// ```ignore
	/// fn handler(lhs: f64, op: Op, rhs: f64, _:&()) -> Result<f64, ()> {
	/// 	Ok(match op {
	/// 		Op::Add => lhs + rhs,
	/// 		Op::Sub => lhs - rhs,
	/// 		Op::Mul => lhs * rhs,
	/// 		Op::Div => lhs / rhs,
	///			Op::Exp => lhs.powf(rhs)
	/// 	})
	/// }
	///
	/// let climber = Climber::new(
	/// 	vec![
	/// 		Rule::new(Op::Add, Assoc::Left) | Rule::new(Op::Sub, Assoc::Right),
	/// 		Rule::new(Op::Mul, Assoc::Left) | Rule::new(Op::Div, Assoc::Right),
	/// 		Rule::new(Op::Exp, Assoc::Right)
	/// 	],
	///		handler
	/// );
	/// ```
	pub fn new(
		rules: Vec<Rule<Op>>,
		handler: fn(To, Op, To, &Ctx) -> Result<To, Err>,
	) -> Self {
		let rules =
			rules
				.into_iter()
				.zip(1..)
				.fold(HashMap::new(), |mut map, (op, prec)| {
					let mut next = Some(op);
					while let Some(op) = next.take() {
						match op {
							Rule {
								op,
								assoc,
								next: val_next,
							} => {
								map.insert(op, (prec, assoc));
								next = val_next.map(|val| *val);
							}
						}
					}
					map
				});
		Self {
			rules,
			handler,
			p_rule_value: PhantomData,
			p_token: PhantomData,
			p_result: PhantomData,
			p_ctx: PhantomData,
		}
	}

	/// Process an [Expression](struct.Expression.html) and return the resulting value.
	/// ```ignore
	/// // 2 + 2 * 3
	/// // 2 + 6
	/// // 8
	/// let expression = Expression::new(
	/// 	2.0f64,
	/// 	vec![
	/// 		(Op::Add, 2.0f64),
	/// 		(Op::Mul, 3.0f64)
	/// 	]
	/// );
	/// assert_eq!(climber.process(&expression, &()).unwrap(), 8.0f64);
	/// ```
	pub fn process(
		&self,
		expr: &Expression<Op, To>,
		ctx: &Ctx,
	) -> Result<Re, Err> {
		let mut primary = expr.first_token.clone().convert(ctx)?;
		let lhs = expr.first_token.clone();
		let mut tokens = expr.pairs.iter().peekable();
		self
			.process_rec(
				lhs, //
				0,
				&mut primary,
				&mut tokens,
				ctx,
			)?
			.convert(ctx)
	}

	fn process_rec(
		&self,
		mut lhs: To,
		min_prec: usize,
		primary: &mut Re,
		tokens: &mut std::iter::Peekable<std::slice::Iter<(Op, To)>>,
		ctx: &Ctx,
	) -> Result<To, Err> {
		while let Some((rule, _)) = tokens.peek() {
			if let Some(&(prec, _)) = self.rules.get(rule) {
				if prec >= min_prec {
					let (_, rhs_ref) = tokens.next().unwrap();
					let mut rhs = rhs_ref.clone();

					while let Some((peek_rule, _)) = tokens.peek() {
						if let Some(&(peek_prec, peek_assoc)) = self.rules.get(peek_rule) {
							if peek_prec > prec
								|| peek_assoc == Assoc::Right && peek_prec == prec
							{
								rhs = self.process_rec(rhs, peek_prec, primary, tokens, ctx)?;
							} else {
								break;
							}
						} else {
							break;
						}
					}
					lhs = (self.handler)(lhs, *rule, rhs, ctx)?;
				} else {
					break;
				}
			} else {
				break;
			}
		}
		Ok(lhs)
	}
}

/// Used within a [Rule](struct.Rule.html) to indicate the left/right association of an operator.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Assoc {
	Left,
	Right,
}

/// A single operator and an [Assoc](enum.Assoc.html)
#[derive(Debug)]
pub struct Rule<Op> {
	op: Op,
	assoc: Assoc,
	next: Option<Box<Rule<Op>>>,
}

impl<Op> Rule<Op> {
	pub fn new(op: Op, assoc: Assoc) -> Self {
		Self {
			op,
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

/// A faithful, always-valid representation of an expression.
///
/// It's impossible to throw an error due to the order of `token, operator, token` not being respected.
#[derive(Debug, Clone)]
pub struct Expression<Op: Copy, To: Clone> {
	pub first_token: To,
	pub pairs: Vec<(Op, To)>,
}

impl<Op: Copy, To: Clone> Expression<Op, To> {
	/// ```ignore
	/// // 5 * 6 + 3 / 2 ^ 4
	/// let expression = Expression::new(
	/// 	5.0f64,
	/// 	vec![
	/// 		(Op::Mul, 6.0),
	/// 		(Op::Add, 3.0),
	/// 		(Op::Div, 2.0),
	/// 		(Op::Exp, 4.0)
	/// 	]
	/// );
	/// ```
	pub fn new(first_token: To, pairs: Vec<(Op, To)>) -> Self {
		Self { first_token, pairs }
	}
}

impl<Op: Copy + fmt::Display, To: Clone + fmt::Display> fmt::Display
	for Expression<Op, To>
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut s = format!("{}", self.first_token);
		for (o, t) in &self.pairs {
			s = format!("{} {} {}", s, o, t);
		}
		write!(f, "{}", s)
	}
}

impl<Op: Copy + PartialEq, To: Clone + PartialEq> PartialEq
	for Expression<Op, To>
{
	fn eq(&self, other: &Self) -> bool {
		self.first_token == other.first_token && self.pairs == other.pairs
	}
}

impl<Op: Copy + Eq, To: Clone + Eq> Eq for Expression<Op, To> {}

#[cfg(test)]
mod test {
	use super::*;

	fn c(
		expression: &Expression<MathOperator, MathToken>,
		ctx: &f32,
	) -> Result<f32, &'static str> {
		use MathOperator::*;
		let climber = Climber::new(
			vec![
				Rule::new(Add, Assoc::Left) | Rule::new(Sub, Assoc::Left),
				Rule::new(Mul, Assoc::Left) | Rule::new(Div, Assoc::Left),
			],
			|lhs: MathToken, op: MathOperator, rhs: MathToken, ctx: &f32| {
				let lhs: f32 = lhs.convert(ctx)?;
				let rhs: f32 = rhs.convert(ctx)?;
				Ok(match op {
					MathOperator::Add => MathToken::Num(lhs + rhs),
					MathOperator::Sub => MathToken::Num(lhs - rhs),
					MathOperator::Mul => MathToken::Num(lhs * rhs),
					MathOperator::Div => MathToken::Num(lhs / rhs),
				})
			},
		);
		climber.process(&expression, ctx)
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
		X,
	}

	impl Token<f32, &'static str, f32> for MathToken {
		fn convert(self, ctx: &f32) -> Result<f32, &'static str> {
			Ok(match self {
				MathToken::Paren(expr) => c(expr.as_ref(), ctx)?,
				MathToken::Num(n) => n,
				MathToken::X => *ctx,
			})
		}
	}

	#[test]
	fn process() {
		let res = c(
			&Expression::new(
				MathToken::Num(7.0),
				vec![(MathOperator::Add, MathToken::X)],
			),
			&8.0,
		)
		.unwrap();

		assert_eq!(res, 15.0);
	}
	#[test]
	fn proces_complex() {
		use MathOperator::*;
		use MathToken::*;
		let res = c(
			&Expression::new(
				Num(10.0),
				vec![(Add, Num(5.0)), (Mul, Num(3.0)), (Add, Num(1.0))],
			),
			&8.0,
		)
		.unwrap();
		println!("{}", res);
	}
}
