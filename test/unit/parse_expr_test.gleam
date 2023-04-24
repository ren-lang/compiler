// IMPORTS ---------------------------------------------------------------------

import gleam/option.{None, Some}
import gleam/io
import gleeunit/should
import ren/ast/expr.{Expr}
import ren/ast/lit.{Field}
import ren/ast/pat
import ren/data/error.{Error}
import ren/data/token.{Token}
import ren/query
import ren/query/parse

// TESTS: BINOPS ---------------------------------------------------------------

pub fn add_test() {
  // 1 + 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse an `add` binop", source)

  should.equal(result, Ok(expr.add(expr.num(1.0), expr.num(2.0))))
}

pub fn and_test() {
  // x & y
  let source = [
    Token(1, 1, 1, 2, "x", token.lower("x")),
    Token(1, 3, 1, 4, "&", token.and),
    Token(1, 5, 1, 6, "y", token.lower("y")),
  ]
  use result <- should("parse an `and` binop", source)

  should.equal(result, Ok(expr.and(expr.Var("x"), expr.Var("y"))))
}

pub fn concat_test() {
  // "a" ++ "b"
  let source = [
    Token(1, 1, 1, 4, "\"a\"", token.str("a")),
    Token(1, 5, 1, 7, "++", token.concat),
    Token(1, 8, 1, 11, "\"b\"", token.str("b")),
  ]
  use result <- should("parse a `concat` binop", source)

  should.equal(result, Ok(expr.concat(expr.str("a"), expr.str("b"))))
}

pub fn div_test() {
  // 1 / 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "/", token.div),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `div` binop", source)

  should.equal(result, Ok(expr.div(expr.num(1.0), expr.num(2.0))))
}

pub fn eq_test() {
  // 1 == 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 5, "==", token.eq),
    Token(1, 6, 1, 7, "2", token.num(2.0)),
  ]
  use result <- should("parse an `eq` binop", source)

  should.equal(result, Ok(expr.eq(expr.num(1.0), expr.num(2.0))))
}

pub fn gt_test() {
  // 1 > 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, ">", token.gt),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `gt` binop", source)

  should.equal(result, Ok(expr.gt(expr.num(1.0), expr.num(2.0))))
}

pub fn gte_test() {
  // 1 >= 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 5, ">=", token.gte),
    Token(1, 6, 1, 7, "2", token.num(2.0)),
  ]
  use result <- should("parse a `gte` binop", source)

  should.equal(result, Ok(expr.gte(expr.num(1.0), expr.num(2.0))))
}

pub fn lt_test() {
  // 1 < 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "<", token.lt),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `lt` binop", source)

  should.equal(result, Ok(expr.lt(expr.num(1.0), expr.num(2.0))))
}

pub fn lte_test() {
  // 1 <= 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 5, "<=", token.lte),
    Token(1, 6, 1, 7, "2", token.num(2.0)),
  ]
  use result <- should("parse a `lte` binop", source)

  should.equal(result, Ok(expr.lte(expr.num(1.0), expr.num(2.0))))
}

pub fn mod_test() {
  // 1 % 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "%", token.mod),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `mod` binop", source)

  should.equal(result, Ok(expr.mod(expr.num(1.0), expr.num(2.0))))
}

pub fn mul_test() {
  // 1 * 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "*", token.mul),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `mul` binop", source)

  should.equal(result, Ok(expr.mul(expr.num(1.0), expr.num(2.0))))
}

pub fn neq_test() {
  // 1 != 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 5, "!=", token.neq),
    Token(1, 6, 1, 7, "2", token.num(2.0)),
  ]
  use result <- should("parse a `neq` binop", source)

  should.equal(result, Ok(expr.neq(expr.num(1.0), expr.num(2.0))))
}

pub fn or_test() {
  // a | b
  let source = [
    Token(1, 1, 1, 2, "a", token.lower("a")),
    Token(1, 3, 1, 4, "|", token.or),
    Token(1, 5, 1, 6, "b", token.lower("b")),
  ]
  use result <- should("parse an `or` binop", source)

  should.equal(result, Ok(expr.or(expr.Var("a"), expr.Var("b"))))
}

pub fn pipe_test() {
  // x |> f
  let source = [
    Token(1, 1, 1, 2, "x", token.lower("x")),
    Token(1, 3, 1, 5, "|>", token.pipe),
    Token(1, 6, 1, 7, "f", token.lower("f")),
  ]
  use result <- should("parse a `pipe` binop", source)

  should.equal(result, Ok(expr.pipe(expr.Var("x"), expr.Var("f"))))
}

pub fn pow_test() {
  // 1 ^ 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "^", token.pow),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `pow` binop", source)

  should.equal(result, Ok(expr.pow(expr.num(1.0), expr.num(2.0))))
}

pub fn sub_test() {
  // 1 - 2
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "-", token.sub),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
  ]
  use result <- should("parse a `sub` binop", source)

  should.equal(result, Ok(expr.sub(expr.num(1.0), expr.num(2.0))))
}

pub fn seq_test() {
  // { x; y }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, ";", token.seq),
    Token(1, 7, 1, 8, "y", token.lower("y")),
    Token(1, 9, 1, 10, "}", token.rbrace),
  ]
  use result <- should("parse a `seq` binop", source)

  should.equal(result, Ok(expr.seq(expr.Var("x"), expr.Var("y"))))
}

pub fn dangling_seq_test() {
  // { x; }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, ";", token.seq),
    Token(1, 9, 1, 10, "}", token.rbrace),
  ]
  use result <- should("parse a `seq` binop", source)

  should.equal(result, Ok(expr.seq(expr.Var("x"), expr.enum("undefined", []))))
}

// TESTS: BINOP PRECEDENCE -----------------------------------------------------

pub fn add_mul_test() {
  // 1 + 2 * 3
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
    Token(1, 7, 1, 8, "*", token.mul),
    Token(1, 9, 1, 10, "3", token.num(3.0)),
  ]
  use result <- should("parse `add` and `mul` binops", source)

  should.equal(
    result,
    Ok(expr.add(expr.num(1.0), expr.mul(expr.num(2.0), expr.num(3.0)))),
  )
}

pub fn add_sub_test() {
  // 1 + 2 - 3
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
    Token(1, 7, 1, 8, "-", token.sub),
    Token(1, 9, 1, 10, "3", token.num(3.0)),
  ]
  use result <- should("parse `add` and `sub` binops", source)

  should.equal(
    result,
    Ok(expr.sub(expr.add(expr.num(1.0), expr.num(2.0)), expr.num(3.0))),
  )
}

pub fn add_div_test() {
  // 1 + 2 / 3
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "2", token.num(2.0)),
    Token(1, 7, 1, 8, "/", token.div),
    Token(1, 9, 1, 10, "3", token.num(3.0)),
  ]
  use result <- should("parse `add` and `div` binops", source)

  should.equal(
    result,
    Ok(expr.add(expr.num(1.0), expr.div(expr.num(2.0), expr.num(3.0)))),
  )
}

pub fn chain_pipe_test() {
  // x |> f |> g
  let source = [
    Token(1, 1, 1, 2, "x", token.lower("x")),
    Token(1, 3, 1, 5, "|>", token.pipe),
    Token(1, 6, 1, 7, "f", token.lower("f")),
    Token(1, 8, 1, 10, "|>", token.pipe),
    Token(1, 11, 1, 12, "g", token.lower("g")),
  ]
  use result <- should("parse a chain of `pipe` binops", source)

  should.equal(
    result,
    Ok(expr.pipe(expr.pipe(expr.Var("x"), expr.Var("f")), expr.Var("g"))),
  )
}

pub fn chain_seq_test() {
  // { x; y; z }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, ";", token.seq),
    Token(1, 7, 1, 8, "y", token.lower("y")),
    Token(1, 5, 1, 6, ";", token.seq),
    Token(1, 7, 1, 8, "z", token.lower("z")),
    Token(1, 9, 1, 10, "}", token.rbrace),
  ]
  use result <- should("parse a `seq` binop", source)

  should.equal(
    result,
    Ok(expr.seq(expr.Var("x"), expr.seq(expr.Var("y"), expr.Var("z")))),
  )
}

pub fn chain_dangling_seq_test() {
  // { x; y; }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, ";", token.seq),
    Token(1, 7, 1, 8, "y", token.lower("y")),
    Token(1, 9, 1, 10, ";", token.seq),
    Token(1, 11, 1, 12, "}", token.rbrace),
  ]
  use result <- should("parse a `seq` binop", source)

  should.equal(
    result,
    Ok(expr.seq(
      expr.Var("x"),
      expr.seq(expr.Var("y"), expr.enum("undefined", [])),
    )),
  )
}

// TESTS: FUNCTION CALLS -------------------------------------------------------

pub fn call_test() {
  // f x y z
  let source = [
    Token(1, 1, 1, 2, "f", token.lower("f")),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, "y", token.lower("y")),
    Token(1, 7, 1, 8, "z", token.lower("z")),
  ]
  use result <- should("parse a function call", source)

  should.equal(
    result,
    Ok(expr.Call(expr.Var("f"), [expr.Var("x"), expr.Var("y"), expr.Var("z")])),
  )
}

pub fn nested_call_test() {
  // f x (g y z)
  let source = [
    Token(1, 1, 1, 2, "f", token.lower("f")),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, "(", token.lparen),
    Token(1, 7, 1, 8, "g", token.lower("g")),
    Token(1, 9, 1, 10, "y", token.lower("y")),
    Token(1, 11, 1, 12, "z", token.lower("z")),
    Token(1, 13, 1, 14, ")", token.rparen),
  ]
  use result <- should("parse a nested function call", source)

  should.equal(
    result,
    Ok(expr.Call(
      expr.Var("f"),
      [expr.Var("x"), expr.Call(expr.Var("g"), [expr.Var("y"), expr.Var("z")])],
    )),
  )
}

// TESTS: LAMBDAS --------------------------------------------------------------

pub fn lambda_test() {
  // fun x y z -> x + y + z
  let source = [
    Token(1, 1, 1, 2, "fun", token.fun),
    Token(1, 3, 1, 4, "x", token.lower("x")),
    Token(1, 5, 1, 6, "y", token.lower("y")),
    Token(1, 7, 1, 8, "z", token.lower("z")),
    Token(1, 9, 1, 11, "->", token.arrow),
    Token(1, 12, 1, 13, "x", token.lower("x")),
    Token(1, 14, 1, 15, "+", token.add),
    Token(1, 16, 1, 17, "y", token.lower("y")),
    Token(1, 18, 1, 19, "+", token.add),
    Token(1, 20, 1, 21, "z", token.lower("z")),
  ]
  use result <- should("parse a lambda", source)

  should.equal(
    result,
    Ok(expr.Fun(
      [pat.Bind("x"), pat.Bind("y"), pat.Bind("z")],
      expr.add(expr.add(expr.Var("x"), expr.Var("y")), expr.Var("z")),
    )),
  )
}

pub fn empty_lambda_test() {
  // fun -> 1
  let source = [
    Token(1, 1, 1, 2, "fun", token.fun),
    Token(1, 3, 1, 5, "->", token.arrow),
    Token(1, 6, 1, 7, "1", token.num(1.0)),
  ]
  use result <- should("not parse a lambda with no args", source)

  should.not_equal(result, Ok(expr.Fun([], expr.num(1.0))))
}

pub fn iife_test() {
  // (fun x -> x + 1) 2
  let source = [
    Token(1, 1, 1, 2, "(", token.lparen),
    Token(1, 2, 1, 3, "fun", token.fun),
    Token(1, 4, 1, 5, "x", token.lower("x")),
    Token(1, 6, 1, 8, "->", token.arrow),
    Token(1, 9, 1, 10, "x", token.lower("x")),
    Token(1, 11, 1, 12, "+", token.add),
    Token(1, 13, 1, 14, "1", token.num(1.0)),
    Token(1, 15, 1, 16, ")", token.rparen),
    Token(1, 17, 1, 18, "2", token.num(2.0)),
  ]
  use result <- should("parse an IIFE", source)

  should.equal(
    result,
    Ok(expr.Call(
      expr.Fun([pat.Bind("x")], expr.add(expr.Var("x"), expr.num(1.0))),
      [expr.num(2.0)],
    )),
  )
}

// TESTS: IF EXPRESSIONS -------------------------------------------------------

pub fn if_test() {
  // if x then y else z
  let source = [
    Token(1, 1, 1, 3, "if", token.if_),
    Token(1, 4, 1, 5, "x", token.lower("x")),
    Token(1, 6, 1, 10, "then", token.then),
    Token(1, 11, 1, 12, "y", token.lower("y")),
    Token(1, 13, 1, 17, "else", token.else),
    Token(1, 18, 1, 19, "z", token.lower("z")),
  ]
  use result <- should("parse an if expression", source)

  should.equal(result, Ok(expr.If(expr.Var("x"), expr.Var("y"), expr.Var("z"))))
}

pub fn chain_if_test() {
  // if x then y else if z then w else v
  let source = [
    Token(1, 1, 1, 3, "if", token.if_),
    Token(1, 4, 1, 5, "x", token.lower("x")),
    Token(1, 6, 1, 10, "then", token.then),
    Token(1, 11, 1, 12, "y", token.lower("y")),
    Token(1, 13, 1, 17, "else", token.else),
    Token(1, 18, 1, 20, "if", token.if_),
    Token(1, 21, 1, 22, "z", token.lower("z")),
    Token(1, 23, 1, 27, "then", token.then),
    Token(1, 28, 1, 29, "w", token.lower("w")),
    Token(1, 30, 1, 34, "else", token.else),
    Token(1, 35, 1, 36, "v", token.lower("v")),
  ]
  use result <- should("parse a chain of if expressions", source)

  should.equal(
    result,
    Ok(expr.If(
      expr.Var("x"),
      expr.Var("y"),
      expr.If(expr.Var("z"), expr.Var("w"), expr.Var("v")),
    )),
  )
}

// TESTS: LET EXPRESSIONS ------------------------------------------------------

pub fn let_test() {
  // { let x = 1; x }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 3, 1, 6, "let", token.let_),
    Token(1, 7, 1, 8, "x", token.lower("x")),
    Token(1, 9, 1, 10, "=", token.equals),
    Token(1, 11, 1, 12, "1", token.num(1.0)),
    Token(1, 12, 1, 13, ";", token.seq),
    Token(1, 14, 1, 15, "x", token.lower("x")),
    Token(1, 16, 1, 17, "}", token.rbrace),
  ]
  use result <- should("parse a let expression", source)

  should.equal(
    result,
    Ok(expr.seq(expr.Let(pat.Bind("x"), expr.num(1.0)), expr.Var("x"))),
  )
}

// TESTS: LITERALS -------------------------------------------------------------

pub fn empty_arr_test() {
  // []
  let source = [
    Token(1, 1, 1, 2, "[", token.lbracket),
    Token(1, 2, 1, 3, "]", token.rbracket),
  ]
  use result <- should("parse an empty array", source)

  should.equal(result, Ok(expr.arr([])))
}

pub fn single_arr_test() {
  // [1]
  let source = [
    Token(1, 1, 1, 2, "[", token.lbracket),
    Token(1, 2, 1, 3, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "]", token.rbracket),
  ]
  use result <- should("parse a single-element array", source)

  should.equal(result, Ok(expr.arr([expr.num(1.0)])))
}

pub fn multi_arr_test() {
  // [1, 2]
  let source = [
    Token(1, 1, 1, 2, "[", token.lbracket),
    Token(1, 2, 1, 3, "1", token.num(1.0)),
    Token(1, 3, 1, 4, ",", token.comma),
    Token(1, 4, 1, 5, "2", token.num(2.0)),
    Token(1, 5, 1, 6, "]", token.rbracket),
  ]
  use result <- should("parse a multi-element array", source)

  should.equal(result, Ok(expr.arr([expr.num(1.0), expr.num(2.0)])))
}

pub fn enum_test() {
  // #wibble
  let source = [
    Token(1, 1, 1, 2, "#", token.hash),
    Token(1, 2, 1, 7, "wibble", token.lower("wibble")),
  ]
  use result <- should("parse a constructor", source)

  should.equal(result, Ok(expr.enum("wibble", [])))
}

pub fn enum_with_args_test() {
  // #wibble 1 2
  let source = [
    Token(1, 1, 1, 2, "#", token.hash),
    Token(1, 2, 1, 7, "wibble", token.lower("wibble")),
    Token(1, 8, 1, 9, "1", token.num(1.0)),
    Token(1, 10, 1, 11, "2", token.num(2.0)),
  ]
  use result <- should("parse a constructor with arguments", source)

  should.equal(result, Ok(expr.enum("wibble", [expr.num(1.0), expr.num(2.0)])))
}

pub fn num_test() {
  // 123.456
  let source = [Token(1, 1, 1, 8, "123.456", token.num(123.456))]
  use result <- should("parse a number", source)

  should.equal(result, Ok(expr.num(123.456)))
}

pub fn empty_obj_test() {
  // {}
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 2, 1, 3, "}", token.rbrace),
  ]
  use result <- should("parse an empty object", source)

  should.equal(result, Ok(expr.rec([])))
}

pub fn single_obj_test() {
  // { a: 1 }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 2, 1, 3, "a", token.lower("a")),
    Token(1, 3, 1, 4, ":", token.colon),
    Token(1, 4, 1, 5, "1", token.num(1.0)),
    Token(1, 5, 1, 6, "}", token.rbrace),
  ]
  use result <- should("parse a single-element object", source)

  should.equal(result, Ok(expr.rec([Field("a", expr.num(1.0))])))
}

pub fn multi_obj_test() {
  // { a: 1, b: 2 }
  let source = [
    Token(1, 1, 1, 2, "{", token.lbrace),
    Token(1, 2, 1, 3, "a", token.lower("a")),
    Token(1, 3, 1, 4, ":", token.colon),
    Token(1, 4, 1, 5, "1", token.num(1.0)),
    Token(1, 5, 1, 6, ",", token.comma),
    Token(1, 6, 1, 7, "b", token.lower("b")),
    Token(1, 7, 1, 8, ":", token.colon),
    Token(1, 8, 1, 9, "2", token.num(2.0)),
    Token(1, 9, 1, 10, "}", token.rbrace),
  ]
  use result <- should("parse a multi-element object", source)

  should.equal(
    result,
    Ok(expr.rec([Field("a", expr.num(1.0)), Field("b", expr.num(2.0))])),
  )
}

pub fn str_test() {
  // "hello"
  let source = [Token(1, 1, 1, 8, "\"hello\"", token.str("hello"))]
  use result <- should("parse a string", source)

  should.equal(result, Ok(expr.str("hello")))
}

// TESTS: PLACEHOLDERS ---------------------------------------------------------

pub fn placeholder_test() {
  // _
  let source = [Token(1, 1, 1, 2, "_", token.underscore)]
  use result <- should("parse a placeholder", source)

  should.equal(result, Ok(expr.Placeholder))
}

pub fn placeholder_in_expr_test() {
  // 1 + _
  let source = [
    Token(1, 1, 1, 2, "1", token.num(1.0)),
    Token(1, 3, 1, 4, "+", token.add),
    Token(1, 5, 1, 6, "_", token.underscore),
  ]
  use result <- should("parse a placeholder in an expression", source)

  should.equal(result, Ok(expr.add(expr.num(1.0), expr.Placeholder)))
}

// TESTS: PATTERN MATCHING -----------------------------------------------------

pub fn num_switch_test() {
  // switch x on
  //   case 1 -> 2
  //   case 2 -> 3
  //   case _ -> 4
  let source = [
    Token(1, 1, 1, 7, "switch", token.switch),
    Token(1, 8, 1, 9, "x", token.lower("x")),
    Token(1, 10, 1, 12, "on", token.on),
    Token(1, 13, 1, 17, "case", token.case_),
    Token(1, 18, 1, 19, "1", token.num(1.0)),
    Token(1, 20, 1, 22, "->", token.arrow),
    Token(1, 23, 1, 24, "2", token.num(2.0)),
    Token(1, 25, 1, 29, "case", token.case_),
    Token(1, 30, 1, 31, "2", token.num(2.0)),
    Token(1, 32, 1, 34, "->", token.arrow),
    Token(1, 35, 1, 36, "3", token.num(3.0)),
    Token(1, 37, 1, 41, "case", token.case_),
    Token(1, 42, 1, 43, "_", token.underscore),
    Token(1, 44, 1, 46, "->", token.arrow),
    Token(1, 47, 1, 48, "4", token.num(4.0)),
  ]
  use result <- should("parse a number switch", source)

  should.equal(
    result,
    Ok(expr.Switch(
      expr.Var("x"),
      [
        pat.Case(pat.Value(lit.Num(1.0)), None, expr.num(2.0)),
        pat.Case(pat.Value(lit.Num(2.0)), None, expr.num(3.0)),
        pat.Case(pat.Wildcard, None, expr.num(4.0)),
      ],
    )),
  )
}

pub fn arr_switch_test() {
  // switch arr on
  //   case [] -> 0
  //   case [x, y as z] -> x + z
  //   case _ -> 1
  let source = [
    Token(1, 1, 1, 7, "switch", token.switch),
    Token(1, 8, 1, 11, "arr", token.lower("arr")),
    Token(1, 12, 1, 14, "on", token.on),
    Token(1, 15, 1, 19, "case", token.case_),
    Token(1, 20, 1, 22, "[]", token.lbracket),
    Token(1, 22, 1, 23, "]", token.rbracket),
    Token(1, 24, 1, 26, "->", token.arrow),
    Token(1, 27, 1, 28, "0", token.num(0.0)),
    Token(1, 29, 1, 33, "case", token.case_),
    Token(1, 34, 1, 35, "[", token.lbracket),
    Token(1, 35, 1, 36, "x", token.lower("x")),
    Token(1, 36, 1, 37, ",", token.comma),
    Token(1, 38, 1, 39, "y", token.lower("y")),
    Token(1, 40, 1, 42, "as", token.as_),
    Token(1, 43, 1, 44, "z", token.lower("z")),
    Token(1, 44, 1, 45, "]", token.rbracket),
    Token(1, 46, 1, 48, "->", token.arrow),
    Token(1, 49, 1, 50, "x", token.lower("x")),
    Token(1, 51, 1, 53, "+", token.add),
    Token(1, 54, 1, 55, "z", token.lower("z")),
    Token(1, 56, 1, 60, "case", token.case_),
    Token(1, 61, 1, 62, "_", token.underscore),
    Token(1, 63, 1, 65, "->", token.arrow),
    Token(1, 66, 1, 67, "1", token.num(1.0)),
  ]
  use result <- should("parse an array switch", source)

  should.equal(
    result,
    Ok(expr.Switch(
      expr.Var("arr"),
      [
        pat.Case(pat.Value(lit.Arr([])), None, expr.num(0.0)),
        pat.Case(
          pat.Value(lit.Arr([pat.Bind("x"), pat.Alias(pat.Bind("y"), "z")])),
          None,
          expr.add(expr.Var("x"), expr.Var("z")),
        ),
        pat.Case(pat.Wildcard, None, expr.num(1.0)),
      ],
    )),
  )
}

pub fn obj_switch_test() {
  // switch obj on
  //   {} -> 0
  //   { x: @Number x } -> x
  //   { x, y } -> x + y
  //   _ -> 1 
  let source = [
    Token(1, 1, 1, 7, "switch", token.switch),
    Token(1, 8, 1, 11, "obj", token.lower("obj")),
    Token(1, 12, 1, 14, "on", token.on),
    Token(1, 15, 1, 17, "case", token.case_),
    Token(1, 18, 1, 19, "{", token.lbrace),
    Token(1, 19, 1, 20, "}", token.rbrace),
    Token(1, 21, 1, 23, "->", token.arrow),
    Token(1, 24, 1, 25, "0", token.num(0.0)),
    Token(1, 26, 1, 30, "case", token.case_),
    Token(1, 31, 1, 32, "{", token.lbrace),
    Token(1, 33, 1, 34, "x", token.lower("x")),
    Token(1, 34, 1, 35, ":", token.colon),
    Token(1, 36, 1, 38, "@", token.at),
    Token(1, 38, 1, 44, "Number", token.upper("Number")),
    Token(1, 45, 1, 46, "x", token.lower("x")),
    Token(1, 46, 1, 47, "}", token.rbrace),
    Token(1, 48, 1, 50, "->", token.arrow),
    Token(1, 51, 1, 52, "x", token.lower("x")),
    Token(1, 53, 1, 57, "case", token.case_),
    Token(1, 58, 1, 59, "{", token.lbrace),
    Token(1, 60, 1, 61, "x", token.lower("x")),
    Token(1, 61, 1, 62, ",", token.comma),
    Token(1, 63, 1, 64, "y", token.lower("y")),
    Token(1, 64, 1, 65, "}", token.rbrace),
    Token(1, 66, 1, 68, "->", token.arrow),
    Token(1, 69, 1, 70, "x", token.lower("x")),
    Token(1, 71, 1, 73, "+", token.add),
    Token(1, 74, 1, 75, "y", token.lower("y")),
    Token(1, 76, 1, 80, "case", token.case_),
    Token(1, 81, 1, 82, "_", token.underscore),
    Token(1, 83, 1, 85, "->", token.arrow),
    Token(1, 86, 1, 87, "1", token.num(1.0)),
  ]
  use result <- should("parse an object switch", source)

  should.equal(
    result,
    Ok(expr.Switch(
      expr.Var("obj"),
      [
        pat.Case(pat.Value(lit.Obj([])), None, expr.num(0.0)),
        pat.Case(
          pat.Value(lit.Obj([
            lit.Field("x", pat.Typeof("Number", pat.Bind("x"))),
          ])),
          None,
          expr.Var("x"),
        ),
        pat.Case(
          pat.Value(lit.Obj([
            lit.Field("x", pat.Bind("x")),
            lit.Field("y", pat.Bind("y")),
          ])),
          None,
          expr.add(expr.Var("x"), expr.Var("y")),
        ),
        pat.Case(pat.Wildcard, None, expr.num(1.0)),
      ],
    )),
  )
}

pub fn switch_with_guard_test() {
  // switch arr on
  //   [x, y] if x < y -> 0
  //   _ -> 1
  let source = [
    Token(1, 1, 1, 7, "switch", token.switch),
    Token(1, 8, 1, 11, "arr", token.lower("arr")),
    Token(1, 12, 1, 14, "on", token.on),
    Token(1, 15, 1, 17, "case", token.case_),
    Token(1, 18, 1, 19, "[", token.lbracket),
    Token(1, 19, 1, 20, "x", token.lower("x")),
    Token(1, 20, 1, 21, ",", token.comma),
    Token(1, 22, 1, 23, "y", token.lower("y")),
    Token(1, 23, 1, 24, "]", token.rbracket),
    Token(1, 25, 1, 27, "if", token.if_),
    Token(1, 28, 1, 29, "x", token.lower("x")),
    Token(1, 30, 1, 31, "<", token.lt),
    Token(1, 32, 1, 33, "y", token.lower("y")),
    Token(1, 34, 1, 36, "->", token.arrow),
    Token(1, 37, 1, 38, "0", token.num(0.0)),
    Token(1, 39, 1, 43, "case", token.case_),
    Token(1, 44, 1, 45, "_", token.underscore),
    Token(1, 46, 1, 48, "->", token.arrow),
    Token(1, 49, 1, 50, "1", token.num(1.0)),
  ]
  use result <- should("parse a switch with a guard", source)

  should.equal(
    result,
    Ok(expr.Switch(
      expr.Var("arr"),
      [
        pat.Case(
          pat.Value(lit.Arr([pat.Bind("x"), pat.Bind("y")])),
          Some(expr.lt(expr.Var("x"), expr.Var("y"))),
          expr.num(0.0),
        ),
        pat.Case(pat.Wildcard, None, expr.num(1.0)),
      ],
    )),
  )
}

// UTILS -----------------------------------------------------------------------

fn should(d: String, s: List(Token), k: fn(Result(Expr, Error)) -> b) {
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
  io.println("should " <> d)

  parse.expr(s)
  |> query.run(provider, Nil)
  |> k
}
