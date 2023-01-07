// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/string
import util

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  App(Expr, Expr)
  Lam(String, Expr)
  Let(String, Expr, Expr)
  Lit(Lit)
  Var(String)
}

///
///
pub type Lit {
  Array(List(Expr))
  Num(Float)
  Record(List(#(String, Expr)))
  Str(String)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn app(fun: Expr, args: List(Expr)) -> Expr {
  case args {
    [] -> fun
    [arg, ..rest] -> app(App(fun, arg), rest)
  }
}

pub fn lam(args: List(String), body: Expr) -> Expr {
  case args {
    [] -> body
    [arg, ..rest] -> Lam(arg, lam(rest, body))
  }
}

pub fn var(name: String) -> Expr {
  assert Ok(first) = string.first(name)

  case <<first:utf8>> {
    <<code:int>> if code > 96 && code < 123 -> Var(name)
    _ -> util.crash("Cannot construct variable from string `" <> name <> "`")
  }
}

// CONSTRUCTORS: OPERATORS -----------------------------------------------------

pub fn add(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("+"), [lhs, rhs])
}

pub fn and(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("&"), [lhs, rhs])
}

pub fn div(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("/"), [lhs, rhs])
}

pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("=="), [lhs, rhs])
}

pub fn gt(lhs: Expr, rhs: Expr) -> Expr {
  app(Var(">"), [lhs, rhs])
}

pub fn gte(lhs: Expr, rhs: Expr) -> Expr {
  app(Var(">="), [lhs, rhs])
}

pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("<"), [lhs, rhs])
}

pub fn lte(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("<="), [lhs, rhs])
}

pub fn mul(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("*"), [lhs, rhs])
}

pub fn neq(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("!="), [lhs, rhs])
}

pub fn or(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("|"), [lhs, rhs])
}

pub fn pipe(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("|>"), [lhs, rhs])
}

pub fn seq(lhs: Expr, rhs: Expr) -> Expr {
  app(Var(";"), [lhs, rhs])
}

pub fn sub(lhs: Expr, rhs: Expr) -> Expr {
  app(Var("-"), [lhs, rhs])
}

// CONSTRUCTORS: LITERALS ------------------------------------------------------

pub fn arr(items: List(Expr)) -> Expr {
  Lit(Array(items))
}

pub fn num(value: Float) -> Expr {
  Lit(Num(value))
}

pub fn int(value: Int) -> Expr {
  num(int.to_float(value))
}

pub fn rec(items: List(#(String, Expr))) -> Expr {
  Lit(Record(items))
}

pub fn str(value: String) -> Expr {
  Lit(Str(value))
}
