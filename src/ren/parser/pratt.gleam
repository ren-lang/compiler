// IMPORTS ---------------------------------------------------------------------

import gleam/list
import ren/parser.{Break, Continue, Parser, do}

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Parsers(a) {
  Parsers(
    one_of: List(fn(Parsers(a)) -> Parser(a)),
    then: List(fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a))),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn expr(
  one_of one_of: List(fn(Parsers(a)) -> Parser(a)),
  then then: List(fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a))),
) -> Parser(a) {
  subexpr(0, Parsers(one_of, then))
}

///
///
pub fn subexpr(precedence: Int, parsers: Parsers(a)) -> Parser(a) {
  let parse_expr = fn() {
    parsers.one_of
    |> list.map(fn(parser) { parser(parsers) })
    |> parser.one_of
  }

  use expr <- do(parser.lazy(parse_expr))
  use expr <- parser.loop(expr)
  parser.one_of([
    operator(precedence, expr, parsers)
    |> parser.map(Continue),
    parser.return(expr)
    |> parser.map(Break),
  ])
}

fn operator(precedence: Int, expr: a, parsers: Parsers(a)) -> Parser(a) {
  let parse_operator = fn(operator: #(Int, fn(a) -> Parser(a))) {
    case operator.0 > precedence {
      True -> Ok(operator.1(expr))
      False -> Error(Nil)
    }
  }

  parsers.then
  |> list.filter_map(fn(to_operator) { parse_operator(to_operator(parsers)) })
  |> parser.one_of
}

//

///
///
pub fn literal(parser: Parser(a)) -> fn(Parsers(a)) -> Parser(a) {
  fn(_) { parser }
}

///
///
pub fn constant(parser: Parser(a), val: b) -> fn(Parsers(a)) -> Parser(b) {
  fn(_) { parser.map(parser, fn(_) { val }) }
}

///
///
pub fn prefix(
  precedence: Int,
  operator: Parser(b),
  apply: fn(a) -> Parser(a),
  parsers: Parsers(a),
) -> Parser(a) {
  use _ <- do(operator)
  use expr <- do(subexpr(precedence, parsers))

  apply(expr)
}

//

///
///
pub fn infixl(
  precedence: Int,
  operator: Parser(b),
  apply: fn(a, a) -> Parser(a),
) -> fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a)) {
  make_infix(#(precedence, precedence), operator, apply)
}

///
///
pub fn infixr(
  precedence: Int,
  operator: Parser(b),
  apply: fn(a, a) -> Parser(a),
) -> fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a)) {
  make_infix(#(precedence, precedence - 1), operator, apply)
}

///
///
pub fn postfix(
  precedence: Int,
  operator: Parser(b),
  apply: fn(a) -> Parser(a),
) -> fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a)) {
  fn(_) {
    #(
      precedence,
      fn(lhs) {
        use _ <- do(operator)

        apply(lhs)
      },
    )
  }
}

// UTILS -----------------------------------------------------------------------

fn make_infix(
  precedence: #(Int, Int),
  operator: Parser(b),
  apply: fn(a, a) -> Parser(a),
) -> fn(Parsers(a)) -> #(Int, fn(a) -> Parser(a)) {
  fn(parsers: Parsers(a)) {
    #(
      precedence.0,
      fn(lhs) {
        use _ <- do(operator)
        use rhs <- do(subexpr(precedence.1, parsers))

        apply(lhs, rhs)
      },
    )
  }
}
