// IMPORTS ---------------------------------------------------------------------

import gleeunit/should
import ren/ast/expr
import ren/ast/pat
import ren/ast/lit
import ren/query
import ren/query/parse
import ren/query/lex

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
  let input = "x y z"
  let expected = expr.call(expr.Var("x"), [expr.Var("y"), expr.Var("z")])

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))

  let input = "let x = 1 + 2"
  let expected = expr.Let(pat.Bind("x"), expr.add(expr.num(1.0), expr.num(2.0)))

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))

  let input = "fun x y -> x + y"
  let expected =
    expr.Fun(
      [pat.Bind("x"), pat.Bind("y")],
      expr.add(expr.Var("x"), expr.Var("y")),
    )

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))

  let input = "fun [x, y] z -> x + y"
  let expected =
    expr.Fun(
      [pat.Value(lit.Array([pat.Bind("x"), pat.Bind("y")])), pat.Bind("z")],
      expr.add(expr.Var("x"), expr.Var("y")),
    )

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))

  let input = "fun (:ok [x]) as result -> x + y"
  let expected =
    expr.Fun(
      [
        pat.Alias(
          pat.Value(lit.Enum("ok", [pat.Value(lit.Array([pat.Bind("x")]))])),
          "result",
        ),
      ],
      expr.add(expr.Var("x"), expr.Var("y")),
    )

  run_parser(input, parse.expr)
  |> should.equal(Ok(expected))
}

// UTILS -----------------------------------------------------------------------

// This util executes a parser query with a mocked IO provider. By passing the
// parser in we can use the same util to run expr/dec/mod parsers without changing
// anything.
//
fn run_parser(input, parse) {
  let test_provider =
    query.Provider(
      read: fn(_) { Ok("") },
      write: fn(_, _) { Ok(Nil) },
      stdout: fn(_) { Nil },
      stderr: fn(_) { Nil },
      log: fn(_) { Nil },
    )

  lex.run(input)
  |> query.then(parse)
  |> query.run(test_provider, Nil)
}
