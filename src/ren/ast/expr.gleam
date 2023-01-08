// IMPORTS ---------------------------------------------------------------------

import gleam/option.{None, Option, Some}
import ren/ast/pat.{Pat}
import ren/ast/lit.{Array, Enum, Lit, Number, Record, String}

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  Binop(Operator, Expr, Expr)
  Call(Expr, Expr)
  Fun(List(Pat), Expr)
  If(Expr, Expr, Expr)
  Let(Pat, Expr)
  Literal(Lit(Expr))
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
pub type Case =
  #(Pat, Option(Expr), Expr)

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
  Literal(Array(elements))
}

///
///
pub fn enum(name: String, args: List(Expr)) -> Expr {
  Literal(Enum(name, args))
}

///
///
pub fn num(value: Float) -> Expr {
  Literal(Number(value))
}

///
///
pub fn rec(fields: List(#(String, Expr))) -> Expr {
  Literal(Record(fields))
}

///
///
pub fn str(value: String) -> Expr {
  Literal(String(value))
}
// QUERIES ---------------------------------------------------------------------
// MANIPULATIONS ---------------------------------------------------------------
// CONVERSIONS -----------------------------------------------------------------
// UTILS -----------------------------------------------------------------------
