// IMPORTS ---------------------------------------------------------------------

import gleam/option.{None, Option, Some}

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  Binop(Operator, Expr, Expr)
  Call(Expr, Expr)
  Fun(List(Pattern), Expr)
  If(Expr, Expr, Expr)
  Let(Pattern, Expr)
  Lit(Literal(Expr))
  Placeholder
  Switch(Expr, List(Case))
  Var(String)
}

///
///
pub type Operator {
  Add
  And
  Div
  Eq
  Gt
  Gte
  Lt
  Lte
  Mod
  Mul
  Neq
  Or
  Pipe
  Pow
  Sub
  Seq
}

///
///
pub type Literal(e) {
  Array(List(e))
  Enum(String, List(e))
  Number(Float)
  Record(List(#(String, e)))
  String(String)
}

///
///
pub type Pattern {
  Alias(Pattern, String)
  Bind(String)
  Value(Literal(Pattern))
  Wildcard
  Typeof(String, Pattern)
}

///
///
pub type Case =
  #(Pattern, Option(Expr), Expr)

// CONSTANTS -------------------------------------------------------------------
// CONSTRUCTORS ----------------------------------------------------------------

pub fn call(fun: Expr, args: List(Expr)) -> Expr {
  case args {
    [] -> fun
    [arg, ..rest] -> Call(fun, call(arg, rest))
  }
}

// CONSTRUCTORS: OPERATORS -----------------------------------------------------

///
///
pub fn add(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Add, lhs, rhs)
}

///
///
pub fn and(lhs: Expr, rhs: Expr) -> Expr {
  Binop(And, lhs, rhs)
}

///
///
pub fn div(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Div, lhs, rhs)
}

///
///
pub fn eq(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Eq, lhs, rhs)
}

///
///
pub fn gt(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Gt, lhs, rhs)
}

///
///
pub fn gte(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Gte, lhs, rhs)
}

///
///
pub fn lt(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Lt, lhs, rhs)
}

///
///
pub fn lte(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Lte, lhs, rhs)
}

///
///
pub fn mod(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Mod, lhs, rhs)
}

///
///
pub fn mul(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Mul, lhs, rhs)
}

///
///
pub fn neq(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Neq, lhs, rhs)
}

///
///
pub fn or(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Or, lhs, rhs)
}

///
///
pub fn pipe(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Pipe, lhs, rhs)
}

///
///
pub fn pow(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Pow, lhs, rhs)
}

///
///
pub fn sub(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Sub, lhs, rhs)
}

///
///
pub fn seq(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Seq, lhs, rhs)
}

// CONSTRUCTORS: LITERALS ------------------------------------------------------

///
///
pub fn arr(elements: List(Expr)) -> Expr {
  Lit(Array(elements))
}

///
///
pub fn enum(name: String, args: List(Expr)) -> Expr {
  Lit(Enum(name, args))
}

///
///
pub fn num(value: Float) -> Expr {
  Lit(Number(value))
}

///
///
pub fn rec(fields: List(#(String, Expr))) -> Expr {
  Lit(Record(fields))
}

///
///
pub fn str(value: String) -> Expr {
  Lit(String(value))
}
// QUERIES ---------------------------------------------------------------------
// MANIPULATIONS ---------------------------------------------------------------
// CONVERSIONS -----------------------------------------------------------------
// UTILS -----------------------------------------------------------------------
