// IMPORTS ---------------------------------------------------------------------

import gleam/list
import control/parser.{Break, Continue, Parser, do}

// TYPES -----------------------------------------------------------------------

pub opaque type Parsers(a, e) {
  Parsers(
    one_of: List(fn(Parsers(a, e)) -> Parser(a, e)),
    then: List(fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e))),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn expr(
  one_of one_of: List(fn(Parsers(a, e)) -> Parser(a, e)),
  then then: List(fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e))),
) -> Parser(a, e) {
  subexpr(0, Parsers(one_of, then))
}

pub fn subexpr(precedence: Int, parsers: Parsers(a, e)) -> Parser(a, e) {
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

fn operator(precedence: Int, expr: a, parsers: Parsers(a, e)) -> Parser(a, e) {
  let parse_operator = fn(operator: #(Int, fn(a) -> Parser(a, e))) {
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

pub fn literal(parser: Parser(a, e), _: Parsers(a, e)) -> Parser(a, e) {
  parser
}

pub fn constant(parser: Parser(a, e), val: b, _: Parsers(b, e)) -> Parser(b, e) {
  parser.map(parser, fn(_) { val })
}

pub fn prefix(
  precedence: Int,
  operator: Parser(b, e),
  apply: fn(a) -> a,
  parsers: Parsers(a, e),
) -> Parser(a, e) {
  use _ <- do(operator)
  use expr <- do(subexpr(precedence, parsers))
  parser.return(apply(expr))
}

//

pub fn infixl(
  precedence: Int,
  operator: Parser(b, e),
  apply: fn(a, a) -> a,
) -> fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e)) {
  make_infix(#(precedence, precedence), operator, apply)
}

pub fn infixr(
  precedence: Int,
  operator: Parser(b, e),
  apply: fn(a, a) -> a,
) -> fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e)) {
  make_infix(#(precedence, precedence - 1), operator, apply)
}

pub fn postfix(
  precedence: Int,
  operator: Parser(b, e),
  apply: fn(a) -> a,
) -> fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e)) {
  fn(_) {
    #(
      precedence,
      fn(lhs) {
        use _ <- do(operator)
        parser.return(apply(lhs))
      },
    )
  }
}

// UTILS -----------------------------------------------------------------------

fn make_infix(
  precedence: #(Int, Int),
  operator: Parser(b, e),
  apply: fn(a, a) -> a,
) -> fn(Parsers(a, e)) -> #(Int, fn(a) -> Parser(a, e)) {
  fn(parsers: Parsers(a, e)) {
    #(
      precedence.0,
      fn(lhs) {
        use _ <- do(operator)
        use rhs <- do(subexpr(precedence.1, parsers))
        parser.return(apply(lhs, rhs))
      },
    )
  }
}
