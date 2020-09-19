A generic [operator-precedence parser][1] for Rust. You plug your own handler function, token struct, and operator enum, and this crate provides the algorithm.

Simple example is available in [int_math.rs](examples/int_math.rs)

```sh
cargo run --example int_math
```

# Example

```rust

fn handler(lhs: f64, op: Op, rhs: f64) {
	match op {
		Op::Add => lhs + rhs,
		Op::Sub => lhs - rhs,
		Op::Mul => lhs * rhs,
		Op::Div => lhs / rhs,
		Op::Exp => lhs.powf(rhs)
	}
}

let climber = Climber::new(
	vec![
		Rule::new(Op::Add, Assoc::Left) | Rule::new(Op::Sub, Assoc::Right),
		Rule::new(Op::Mul, Assoc::Left) | Rule::new(Op::Div, Assoc::Right),
		Rule::new(Op::Exp, Assoc::Right)
	],
	handler
);

// 2 + 2 * 3
// 2 + 6
// 8
let expression = Expression::new(
	2.0f64,
	vec![
		(Op::Add, 2.0f64),
		(Op::Mul, 3.0f64)
	]
);

assert_eq!(climber.process(&expression), 8.0f64);
```

---

This crate is heavily based on the [Pest][2] parser's [PrecClimber][3], but is a more generic implementation for non-Pest use.

[1]: https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method
[2]: https://pest.rs/
[3]: https://docs.rs/pest/2.1.3/pest/prec_climber/struct.PrecClimber.html
