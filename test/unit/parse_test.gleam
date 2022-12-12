// IMPORTS ---------------------------------------------------------------------

import gleeunit/should
import ren/ast/expr
import ren/ast/mod
import ren/data/token
import ren/query
import ren/query/parse

// TESTS -----------------------------------------------------------------------

pub fn parse_test() {
  todo
}

pub fn parse_mod_test() {
  todo
}

pub fn parse_dec_test() {
  todo
}

pub fn parse_expr_test() {
  let input = [
    token.num(1.0),
    token.add,
    token.num(2.0),
    token.mul,
    token.num(3.0),
    token.EOF,
  ]
  let expected = expr.add(expr.num(1.0), expr.mul(expr.num(2.0), expr.num(3.0)))

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))

  let input = [
    token.fun,
    token.lower("x"),
    token.lower("y"),
    token.arrow,
    token.lower("x"),
    token.add,
    token.lower("y"),
  ]
  let expected = expr.lam(["x", "y"], expr.add(expr.var("x"), expr.var("y")))

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))
}

// UTILS -----------------------------------------------------------------------

// This util executes a parser query with a mocked IO provider. By passing the
// parser in we can use the same util to run expr/dec/mod parsers without changing
// anything.
//
fn run_parser(input, parser) {
  let test_provider =
    query.Provider(
      read: fn(_) { Ok("") },
      write: fn(_, _) { Ok(Nil) },
      stdout: fn(_) { Nil },
      stderr: fn(_) { Nil },
      log: fn(_) { Nil },
    )

  parser(input)
  |> query.run(test_provider, Nil)
}
