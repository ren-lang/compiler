// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleeunit/should
import ren/data/error.{Error}
import ren/data/token.{Token}
import ren/query
import ren/query/lex

// TESTS: SNIPPETS -------------------------------------------------------------

pub fn maths_expr_test() {
  use result <- should("lex a simple maths expression", "1 + 2 * 3")
  let tokens = [
    Token(1, 1, 1, 2, "1", token.int(1)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "2", token.int(2)),
    Token(1, 7, 1, 8, "*", token.mul),
    Token(1, 9, 1, 10, "3", token.int(3)),
  ]

  result
  |> should.equal(Ok(tokens))
}

pub fn import_decl_test() {
  use result <- should("lex an import declaration", "import Wibble.Wobble")
  let tokens = [
    Token(1, 1, 1, 7, "import", token.import_),
    Token(1, 8, 1, 14, "Wibble", token.upper("Wibble")),
    Token(1, 14, 1, 15, ".", token.dot),
    Token(1, 15, 1, 21, "Wobble", token.upper("Wobble")),
  ]

  result
  |> should.equal(Ok(tokens))
}

pub fn pub_decl_test() {
  use result <- should("lex a declaration", "pub let wibble = \"wobble\"")
  let tokens = [
    Token(1, 1, 1, 4, "pub", token.pub_),
    Token(1, 5, 1, 8, "let", token.let_),
    Token(1, 9, 1, 15, "wibble", token.lower("wibble")),
    Token(1, 16, 1, 17, "=", token.equals),
    Token(1, 18, 1, 26, "\"wobble\"", token.str("wobble")),
  ]

  result
  |> should.equal(Ok(tokens))
}

// TESTS: IDENTIFIERS ----------------------------------------------------------

pub fn lower_identifier_test() {
  use result <- should("lex a lower identifier", "wibble")
  let token = Token(1, 1, 1, 7, "wibble", token.lower("wibble"))

  result
  |> should.equal(Ok([token]))
}

pub fn lower_identifier_complex_test() {
  use result <- should("lex a complex lower identifier", "wibble_123")
  let token = Token(1, 1, 1, 11, "wibble_123", token.lower("wibble_123"))

  result
  |> should.equal(Ok([token]))
}

pub fn upper_identifier_test() {
  use result <- should("lex an upper identifier", "Wibble")
  let token = Token(1, 1, 1, 7, "Wibble", token.upper("Wibble"))

  result
  |> should.equal(Ok([token]))
}

pub fn upper_identifier_complex_test() {
  use result <- should("lex a complex upper identifier", "Wibble_123")
  let token = Token(1, 1, 1, 11, "Wibble_123", token.upper("Wibble_123"))

  result
  |> should.equal(Ok([token]))
}

// TESTS: LITERALS -------------------------------------------------------------

pub fn int_test() {
  use result <- should("lex an integer", "123")
  let token = Token(1, 1, 1, 4, "123", token.int(123))

  result
  |> should.equal(Ok([token]))
}

pub fn negative_int_test() {
  use result <- should("lex an integer", "-123")
  let token = Token(1, 1, 1, 5, "-123", token.int(-123))

  result
  |> should.equal(Ok([token]))
}

pub fn float_test() {
  use result <- should("lex an integer", "123.456")
  let token = Token(1, 1, 1, 8, "123.456", token.num(123.456))

  result
  |> should.equal(Ok([token]))
}

pub fn negative_float_test() {
  use result <- should("lex an integer", "-123.456")
  let token = Token(1, 1, 1, 9, "-123.456", token.num(-123.456))

  result
  |> should.equal(Ok([token]))
}

pub fn string_test() {
  use result <- should("lex a string", "\"wibble\"")
  let token = Token(1, 1, 1, 9, "\"wibble\"", token.str("wibble"))

  result
  |> should.equal(Ok([token]))
}

pub fn string_with_escapes_test() {
  use result <- should("lex an escaped string", "\"wibble\\n\\\"wobble\\\"\"")
  let token =
    Token(
      1,
      1,
      1,
      21,
      "\"wibble\\n\\\"wobble\\\"\"",
      token.str("wibble\\n\\\"wobble\\\""),
    )

  result
  |> should.equal(Ok([token]))
}

// TESTS: KEYWORDS -------------------------------------------------------------

pub fn as_test() {
  use result <- should("lex the `as` keyword", "as")
  let token = Token(1, 1, 1, 3, "as", token.as_)

  result
  |> should.equal(Ok([token]))
}

pub fn assert_test() {
  use result <- should("lex the `assert` keyword", "assert")
  let token = Token(1, 1, 1, 7, "assert", token.assert_)

  result
  |> should.equal(Ok([token]))
}

pub fn case_test() {
  use result <- should("lex the `case` keyword", "case")
  let token = Token(1, 1, 1, 5, "case", token.case_)

  result
  |> should.equal(Ok([token]))
}

pub fn else_test() {
  use result <- should("lex the `else` keyword", "else")
  let token = Token(1, 1, 1, 5, "else", token.else)

  result
  |> should.equal(Ok([token]))
}

pub fn expect_test() {
  use result <- should("lex the `expect` keyword", "expect")
  let token = Token(1, 1, 1, 7, "expect", token.expect)

  result
  |> should.equal(Ok([token]))
}

pub fn ext_test() {
  use result <- should("lex the `ext` keyword", "ext")
  let token = Token(1, 1, 1, 4, "ext", token.ext)

  result
  |> should.equal(Ok([token]))
}

pub fn forall_test() {
  use result <- should("lex the `forall` keyword", "forall")
  let token = Token(1, 1, 1, 7, "forall", token.forall)

  result
  |> should.equal(Ok([token]))
}

pub fn unicode_forall_test() {
  use result <- should("lex the `unicode forall` keyword", "∀")
  let token = Token(1, 1, 1, 2, "∀", token.forall)

  result
  |> should.equal(Ok([token]))
}

pub fn fun_test() {
  use result <- should("lex the `fun` keyword", "fun")
  let token = Token(1, 1, 1, 4, "fun", token.fun)

  result
  |> should.equal(Ok([token]))
}

pub fn unicode_fun_test() {
  use result <- should("lex the `unicode fun` keyword", "λ")
  let token = Token(1, 1, 1, 2, "λ", token.fun)

  result
  |> should.equal(Ok([token]))
}

pub fn if_test() {
  use result <- should("lex the `if` keyword", "if")
  let token = Token(1, 1, 1, 3, "if", token.if_)

  result
  |> should.equal(Ok([token]))
}

pub fn import_test() {
  use result <- should("lex the `import` keyword", "import")
  let token = Token(1, 1, 1, 7, "import", token.import_)

  result
  |> should.equal(Ok([token]))
}

pub fn let_test() {
  use result <- should("lex the `let` keyword", "let")
  let token = Token(1, 1, 1, 4, "let", token.let_)

  result
  |> should.equal(Ok([token]))
}

pub fn on_test() {
  use result <- should("lex the `on` keyword", "on")
  let token = Token(1, 1, 1, 3, "on", token.on)

  result
  |> should.equal(Ok([token]))
}

pub fn pkg_test() {
  use result <- should("lex the `pkg` keyword", "pkg")
  let token = Token(1, 1, 1, 4, "pkg", token.pkg)

  result
  |> should.equal(Ok([token]))
}

pub fn pub_test() {
  use result <- should("lex the `pub` keyword", "pub")
  let token = Token(1, 1, 1, 4, "pub", token.pub_)

  result
  |> should.equal(Ok([token]))
}

pub fn switch_test() {
  use result <- should("lex the `switch` keyword", "switch")
  let token = Token(1, 1, 1, 7, "switch", token.switch)

  result
  |> should.equal(Ok([token]))
}

pub fn then_test() {
  use result <- should("lex the `then` keyword", "then")
  let token = Token(1, 1, 1, 5, "then", token.then)

  result
  |> should.equal(Ok([token]))
}

pub fn type_test() {
  use result <- should("lex the `type` keyword", "type")
  let token = Token(1, 1, 1, 5, "type", token.type_)

  result
  |> should.equal(Ok([token]))
}

// TESTS: SYMBOLS --------------------------------------------------------------

pub fn arrow_test() {
  use result <- should("lex an `arrow` symbol", "->")
  let token = Token(1, 1, 1, 3, "->", token.arrow)

  result
  |> should.equal(Ok([token]))
}

pub fn unicode_arrow_test() {
  use result <- should("lex a `unicode arrow` symbol", "→")
  let token = Token(1, 1, 1, 2, "→", token.arrow)

  result
  |> should.equal(Ok([token]))
}

pub fn at_test() {
  use result <- should("lex an `at` symbol", "@")
  let token = Token(1, 1, 1, 2, "@", token.at)

  result
  |> should.equal(Ok([token]))
}

pub fn colon_test() {
  use result <- should("lex a `colon` symbol", ":")
  let token = Token(1, 1, 1, 2, ":", token.colon)

  result
  |> should.equal(Ok([token]))
}

pub fn dot_test() {
  use result <- should("lex a `dot` symbol", ".")
  let token = Token(1, 1, 1, 2, ".", token.dot)

  result
  |> should.equal(Ok([token]))
}

pub fn double_dot_test() {
  use result <- should("lex a `double dot` symbol", "..")
  let token = Token(1, 1, 1, 3, "..", token.double_dot)

  result
  |> should.equal(Ok([token]))
}

pub fn double_slash_test() {
  use result <- should("lex a `double slash` symbol", "//")
  let token = Token(1, 1, 1, 3, "//", token.double_slash)

  result
  |> should.equal(Ok([token]))
}

pub fn equals_test() {
  use result <- should("lex an `equals` symbol", "=")
  let token = Token(1, 1, 1, 2, "=", token.equals)

  result
  |> should.equal(Ok([token]))
}

pub fn hash_test() {
  use result <- should("lex a `hash` symbol", "#")
  let token = Token(1, 1, 1, 2, "#", token.hash)

  result
  |> should.equal(Ok([token]))
}

pub fn lbrace_test() {
  use result <- should("lex a `left brace` symbol", "{")
  let token = Token(1, 1, 1, 2, "{", token.lbrace)

  result
  |> should.equal(Ok([token]))
}

pub fn lbracket_test() {
  use result <- should("lex a `left bracket` symbol", "[")
  let token = Token(1, 1, 1, 2, "[", token.lbracket)

  result
  |> should.equal(Ok([token]))
}

pub fn lparen_test() {
  use result <- should("lex a `left paren` symbol", "(")
  let token = Token(1, 1, 1, 2, "(", token.lparen)

  result
  |> should.equal(Ok([token]))
}

pub fn question_test() {
  use result <- should("lex a `question` symbol", "?")
  let token = Token(1, 1, 1, 2, "?", token.question)

  result
  |> should.equal(Ok([token]))
}

pub fn rbrace_test() {
  use result <- should("lex a `right brace` symbol", "}")
  let token = Token(1, 1, 1, 2, "}", token.rbrace)

  result
  |> should.equal(Ok([token]))
}

pub fn rbracket_test() {
  use result <- should("lex a `right bracket` symbol", "]")
  let token = Token(1, 1, 1, 2, "]", token.rbracket)

  result
  |> should.equal(Ok([token]))
}

pub fn rparen_test() {
  use result <- should("lex a `right paren` symbol", ")")
  let token = Token(1, 1, 1, 2, ")", token.rparen)

  result
  |> should.equal(Ok([token]))
}

pub fn undercore_test() {
  use result <- should("lex an `underscore` symbol", "_")
  let token = Token(1, 1, 1, 2, "_", token.underscore)

  result
  |> should.equal(Ok([token]))
}

// TESTS: OPERATORS ------------------------------------------------------------

pub fn add_test() {
  use result <- should("lex the `add` operator", "+")
  let token = Token(1, 1, 1, 2, "+", token.add)

  result
  |> should.equal(Ok([token]))
}

pub fn and_test() {
  use result <- should("lex the `and` operator", "&")
  let token = Token(1, 1, 1, 2, "&", token.and)

  result
  |> should.equal(Ok([token]))
}

pub fn concat_test() {
  use result <- should("lex the `concat` operator", "++")
  let token = Token(1, 1, 1, 3, "++", token.concat)

  result
  |> should.equal(Ok([token]))
}

pub fn div_test() {
  use result <- should("lex the `div` operator", "/")
  let token = Token(1, 1, 1, 2, "/", token.div)

  result
  |> should.equal(Ok([token]))
}

pub fn eq_test() {
  use result <- should("lex the `eq` operator", "==")
  let token = Token(1, 1, 1, 3, "==", token.eq)

  result
  |> should.equal(Ok([token]))
}

pub fn gt_test() {
  use result <- should("lex the `gt` operator", ">")
  let token = Token(1, 1, 1, 2, ">", token.gt)

  result
  |> should.equal(Ok([token]))
}

pub fn gte_test() {
  use result <- should("lex the `gte` operator", ">=")
  let token = Token(1, 1, 1, 3, ">=", token.gte)

  result
  |> should.equal(Ok([token]))
}

pub fn lt_test() {
  use result <- should("lex the `lt` operator", "<")
  let token = Token(1, 1, 1, 2, "<", token.lt)

  result
  |> should.equal(Ok([token]))
}

pub fn lte_test() {
  use result <- should("lex the `lte` operator", "<=")
  let token = Token(1, 1, 1, 3, "<=", token.lte)

  result
  |> should.equal(Ok([token]))
}

pub fn mod_test() {
  use result <- should("lex the `mod` operator", "%")
  let token = Token(1, 1, 1, 2, "%", token.mod)

  result
  |> should.equal(Ok([token]))
}

pub fn mul_test() {
  use result <- should("lex the `mul` operator", "*")
  let token = Token(1, 1, 1, 2, "*", token.mul)

  result
  |> should.equal(Ok([token]))
}

pub fn neq_test() {
  use result <- should("lex the `neq` operator", "!=")
  let token = Token(1, 1, 1, 3, "!=", token.neq)

  result
  |> should.equal(Ok([token]))
}

pub fn or_test() {
  use result <- should("lex the `or` operator", "|")
  let token = Token(1, 1, 1, 2, "|", token.or)

  result
  |> should.equal(Ok([token]))
}

pub fn pipe_test() {
  use result <- should("lex the `pipe` operator", "|>")
  let token = Token(1, 1, 1, 3, "|>", token.pipe)

  result
  |> should.equal(Ok([token]))
}

pub fn pow_test() {
  use result <- should("lex the `pow` operator", "^")
  let token = Token(1, 1, 1, 2, "^", token.pow)

  result
  |> should.equal(Ok([token]))
}

pub fn seq_test() {
  use result <- should("lex the `seq` operator", ";")
  let token = Token(1, 1, 1, 2, ";", token.seq)

  result
  |> should.equal(Ok([token]))
}

pub fn sub_test() {
  use result <- should("lex the `sub` operator", "-")
  let token = Token(1, 1, 1, 2, "-", token.sub)

  result
  |> should.equal(Ok([token]))
}

// UTILS -----------------------------------------------------------------------

fn should(desc: String, src: String, k: fn(Result(List(Token), Error)) -> a) {
  // Stub out a dummy provider for our query runner. We're not testing that
  // here so we just return empty values.
  let provider =
    query.Provider(
      read: fn(_) { Ok("") },
      read_dir: fn(_) { Ok([]) },
      write: fn(_, _) { Ok(Nil) },
      write_dir: fn(_) { Ok(Nil) },
      stdout: fn(_) { Nil },
      stderr: fn(_) { Nil },
      log: fn(_) { Nil },
    )

  // This only gets printed when the test fails (using the Erlang target).
  io.println("should " <> desc)

  lex.run(src)
  |> query.run(provider, Nil)
  |> k
}
