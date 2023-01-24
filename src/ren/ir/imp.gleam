// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

pub type Statement {
  Block(List(Statement))
  Break
  Comment(String)
  Const(String, Expression)
  Continue
  Export(Statement)
  Expr(Expression)
  ForIn(String, Expression, List(Statement))
  Function(String, List(String), List(Statement))
  If(Expression, List(Statement))
  IfElse(Expression, List(Statement), List(Statement))
  Return(Expression)
  Throw(String)
  While(Expression, List(Statement))
}

pub type Expression {
  Access(Expression, List(String))
  Array(List(Expression))
  Arrow(List(String), List(Statement))
  Assign(Expression, Expression)
  Binop(Expression, Binop, Expression)
  Call(Expression, List(Expression))
  IIFE(List(Statement))
  Index(Expression, Expression)
  JSFalse
  JSTrue
  Null
  Number(Float)
  Object(List(#(String, Expression)))
  Spread(Expression)
  String(String)
  Ternary(Expression, Expression, Expression)
  Undefined
  Unop(Unop, Expression)
  Var(String)
}

pub type Unop {
  Neg
  New
  Not
  Pos
  Typeof
}

pub type Binop {
  Add
  And
  Comma
  Div
  Eq
  Gt
  Gte
  In
  Instanceof
  Lt
  Lte
  Mod
  Mul
  Neq
  Or
  Pow
  Sub
}

// CONSTANTS -------------------------------------------------------------------

// CONSTRUCTORS ----------------------------------------------------------------
// QUERIES ---------------------------------------------------------------------

/// Used for determining when to wrap subexpressions in parentheses. Uses precedence
/// numbers from:
///  <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence>
///
pub fn precedence_of(expr: Expression) -> Int {
  case expr {
    Access(_, _) -> 17
    Index(_, _) -> 17
    Unop(New, _) -> 17
    Call(_, _) -> 17
    Unop(Pos, _) -> 14
    Unop(Neg, _) -> 14
    Unop(Typeof, _) -> 14
    Binop(_, Mul, _) -> 12
    Binop(_, Div, _) -> 12
    Binop(_, Mod, _) -> 12
    Binop(_, Add, _) -> 11
    Binop(_, Sub, _) -> 11
    Binop(_, Lt, _) -> 9
    Binop(_, Lte, _) -> 9
    Binop(_, Gt, _) -> 9
    Binop(_, Gte, _) -> 9
    Binop(_, In, _) -> 9
    Binop(_, Instanceof, _) -> 9
    Binop(_, Eq, _) -> 8
    Binop(_, Neq, _) -> 8
    Binop(_, And, _) -> 4
    Binop(_, Or, _) -> 3
    Binop(_, Comma, _) -> 1

    // The way we check if a current expression should be wrapped in parentheses,
    // we look at the expression's precedence and compare it to the current
    // precedence level. If it is lower, it get's wrapped.
    //
    // Besides operators, other kinds of expression don't really need to deal
    // with this so we say they have a massive precedence and never wrap.
    _ -> 999
  }
}

pub fn statements(stmt: Statement) -> List(Statement) {
  case stmt {
    Block(stmts) -> stmts
    _ -> [stmt]
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn flatten(stmt: Statement) -> Statement {
  case stmt {
    Block(stmts) -> Block(list.flat_map(stmts, do_flatten))
    _ -> stmt
  }
}

fn do_flatten(stmt: Statement) -> List(Statement) {
  case stmt {
    Block(stmts) -> list.flat_map(stmts, do_flatten)
    _ -> [stmt]
  }
}

///
///
pub fn return(stmt: Statement) -> Statement {
  case stmt {
    Block([]) -> Return(Undefined)
    Block([stmt]) -> return(stmt)
    Block(stmts) -> Block(return_last(stmts))
    Break -> stmt
    Comment(_) -> stmt
    Const(_, expr) -> Return(expr)
    Continue -> stmt
    Export(stmt) -> return(stmt)
    Expr(expr) -> Return(expr)
    ForIn(_, _, _) -> Block([stmt, Return(Undefined)])
    Function(_, args, body) -> Return(Arrow(args, body))
    If(cond, then) -> If(cond, return_last(then))
    IfElse(cond, then, else) ->
      IfElse(cond, return_last(then), return_last(else))
    Return(_) -> stmt
    Throw(_) -> stmt
    While(_, _) -> Block([stmt, Return(Undefined)])
  }
}

fn return_last(stmts: List(Statement)) -> List(Statement) {
  case list.reverse(stmts) {
    [last, ..rest] -> list.reverse([return(last), ..rest])
    [] -> []
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn as_expression(stmt: Statement) -> Expression {
  case stmt {
    Block(stmts) -> IIFE(stmts)
    Const(_, expr) -> expr
    Expr(expr) -> expr
    Function(_, args, body) -> Arrow(args, body)
    If(cond, [Expr(then)]) -> Ternary(cond, then, Undefined)
    IfElse(cond, [Expr(then)], [Expr(else)]) -> Ternary(cond, then, else)
    Return(expr) -> expr
    _ -> IIFE(do_flatten(stmt))
  }
}
// UTILS -----------------------------------------------------------------------
