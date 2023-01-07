// IMPORTS ---------------------------------------------------------------------

import gleeunit/should
import ren/data/token
import ren/query
import ren/query/lex

// TESTS -----------------------------------------------------------------------

pub fn lex_test() {
  let input = "fun x y -> x + y"
  let expected = [
    token.fun,
    token.lower("x"),
    token.lower("y"),
    token.arrow,
    token.lower("x"),
    token.add,
    token.lower("y"),
    token.EOF,
  ]

  run_lexer(input)
  |> should.equal(Ok(expected))

  let input = "Array.map (_ * 2) [1, ..xs]"
  let expected = [
    token.upper("Array"),
    token.dot,
    token.lower("map"),
    token.lparen,
    token.underscore,
    token.mul,
    token.num(2.0),
    token.rparen,
    token.lbracket,
    token.num(1.0),
    token.comma,
    token.double_dot,
    token.lower("xs"),
    token.rbracket,
    token.EOF,
  ]

  run_lexer(input)
  |> should.equal(Ok(expected))

  let input = "import \"ren/std/array\" as Array"
  let expected = [
    token.import_,
    token.str("ren/std/array"),
    token.as_,
    token.upper("Array"),
    token.EOF,
  ]

  run_lexer(input)
  |> should.equal(Ok(expected))
}

pub fn lex_eof_test() {
  run_lexer("")
  |> should.equal(Ok([token.EOF]))
}

pub fn lex_identifier_test() {
  run_lexer("foo")
  |> should.equal(Ok([token.lower("foo"), token.EOF]))

  run_lexer("Foo")
  |> should.equal(Ok([token.upper("Foo"), token.EOF]))

  run_lexer("fooBar")
  |> should.equal(Ok([token.lower("fooBar"), token.EOF]))

  run_lexer("foo42")
  |> should.equal(Ok([token.lower("foo42"), token.EOF]))

  // Underscores are allowed in identifiers...
  run_lexer("foo_bar")
  |> should.equal(Ok([token.lower("foo_bar"), token.EOF]))
  // ...but not at the start.
  run_lexer("_foo")
  |> should.equal(Ok([token.underscore, token.lower("foo"), token.EOF]))

  // Spaces should separate identifiers from each other, obviously.
  run_lexer("foo bar")
  |> should.equal(Ok([token.lower("foo"), token.lower("bar"), token.EOF]))

  // The lexer should not eagerly consume keywords as identifiers.
  run_lexer("cassette")
  |> should.equal(Ok([token.lower("cassette"), token.EOF]))
}

/// ...but not at the start
pub fn lex_keyword_test() {
  run_lexer("as")
  |> should.equal(Ok([token.as_, token.EOF]))

  run_lexer("assert")
  |> should.equal(Ok([token.assert_, token.EOF]))

  run_lexer("case")
  |> should.equal(Ok([token.case_, token.EOF]))

  run_lexer("else")
  |> should.equal(Ok([token.else, token.EOF]))

  run_lexer("expect")
  |> should.equal(Ok([token.expect, token.EOF]))

  run_lexer("ext")
  |> should.equal(Ok([token.ext, token.EOF]))

  run_lexer("fun")
  |> should.equal(Ok([token.fun, token.EOF]))

  run_lexer("λ")
  |> should.equal(Ok([token.fun, token.EOF]))

  run_lexer("if")
  |> should.equal(Ok([token.if_, token.EOF]))

  run_lexer("import")
  |> should.equal(Ok([token.import_, token.EOF]))

  run_lexer("let")
  |> should.equal(Ok([token.let_, token.EOF]))

  run_lexer("on")
  |> should.equal(Ok([token.on, token.EOF]))

  run_lexer("pkg")
  |> should.equal(Ok([token.pkg, token.EOF]))

  run_lexer("pub")
  |> should.equal(Ok([token.pub_, token.EOF]))

  run_lexer("switch")
  |> should.equal(Ok([token.switch, token.EOF]))

  run_lexer("then")
  |> should.equal(Ok([token.then, token.EOF]))

  run_lexer("type")
  |> should.equal(Ok([token.type_, token.EOF]))
}

pub fn lex_literal_test() {
  run_lexer("1")
  |> should.equal(Ok([token.num(1.0), token.EOF]))

  run_lexer("1.23")
  |> should.equal(Ok([token.num(1.23), token.EOF]))

  run_lexer("-1")
  |> should.equal(Ok([token.num(-1.0), token.EOF]))

  run_lexer("\"hello world\"")
  |> should.equal(Ok([token.str("hello world"), token.EOF]))
}

pub fn lex_operator_test() {
  run_lexer("+")
  |> should.equal(Ok([token.add, token.EOF]))

  run_lexer("&")
  |> should.equal(Ok([token.and, token.EOF]))

  run_lexer("/")
  |> should.equal(Ok([token.div, token.EOF]))

  run_lexer("==")
  |> should.equal(Ok([token.eq, token.EOF]))

  run_lexer(">")
  |> should.equal(Ok([token.gt, token.EOF]))

  run_lexer(">=")
  |> should.equal(Ok([token.gte, token.EOF]))

  run_lexer("<")
  |> should.equal(Ok([token.lt, token.EOF]))

  run_lexer("<=")
  |> should.equal(Ok([token.lte, token.EOF]))

  run_lexer("%")
  |> should.equal(Ok([token.mod, token.EOF]))

  run_lexer("*")
  |> should.equal(Ok([token.mul, token.EOF]))

  run_lexer("!=")
  |> should.equal(Ok([token.neq, token.EOF]))

  run_lexer("|")
  |> should.equal(Ok([token.or, token.EOF]))

  run_lexer("|>")
  |> should.equal(Ok([token.pipe, token.EOF]))

  run_lexer("^")
  |> should.equal(Ok([token.pow, token.EOF]))

  run_lexer("-")
  |> should.equal(Ok([token.sub, token.EOF]))
}

pub fn lex_symbol_test() {
  run_lexer("->")
  |> should.equal(Ok([token.arrow, token.EOF]))

  run_lexer("→")
  |> should.equal(Ok([token.arrow, token.EOF]))

  run_lexer(":")
  |> should.equal(Ok([token.colon, token.EOF]))

  run_lexer(",")
  |> should.equal(Ok([token.comma, token.EOF]))

  run_lexer(".")
  |> should.equal(Ok([token.dot, token.EOF]))

  run_lexer("..")
  |> should.equal(Ok([token.double_dot, token.EOF]))

  run_lexer("//")
  |> should.equal(Ok([token.double_slash, token.EOF]))

  run_lexer("=")
  |> should.equal(Ok([token.equals, token.EOF]))

  run_lexer("{")
  |> should.equal(Ok([token.lbrace, token.EOF]))

  run_lexer("[")
  |> should.equal(Ok([token.lbracket, token.EOF]))

  run_lexer("(")
  |> should.equal(Ok([token.lparen, token.EOF]))

  run_lexer("?")
  |> should.equal(Ok([token.question, token.EOF]))

  run_lexer("}")
  |> should.equal(Ok([token.rbrace, token.EOF]))

  run_lexer("]")
  |> should.equal(Ok([token.rbracket, token.EOF]))

  run_lexer(")")
  |> should.equal(Ok([token.rparen, token.EOF]))

  run_lexer(";")
  |> should.equal(Ok([token.seq, token.EOF]))

  run_lexer("_")
  |> should.equal(Ok([token.underscore, token.EOF]))
}

// UTILS -----------------------------------------------------------------------

fn run_lexer(input: String) {
  // Our lexer, like most parts of the compiler, is run as a `Query`. What that
  // means is explained in more detail in the query module itself, but here it is
  // enough to know that we need to provide a query with some functions it can use
  // to read and write data.
  //
  // We don't need any of that functionality for the lexer, so we can just stub
  // out a bunch of dummy methods that do nothing.
  let test_provider =
    query.Provider(
      read: fn(_) { Ok("") },
      write: fn(_, _) { Ok(Nil) },
      stdout: fn(_) { Nil },
      stderr: fn(_) { Nil },
      log: fn(_) { Nil },
    )

  lex.run(input)
  |> query.run(test_provider, Nil)
}
