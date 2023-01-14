// IMPORTS ---------------------------------------------------------------------

// TYPES -----------------------------------------------------------------------

pub type Statement {
  Block(List(Statement))
  Break
  Comment(String)
  Const(String, Expression)
  Continue
  Export(Statement)
  Expr(Expression)
  ForIn(String, Expression, Statement)
  Function(String, List(String), List(Statement))
  If(Expression, Statement)
  IfElse(Expression, Statement, Statement)
  Return(Expression)
  Throw(String)
  While(Expression, Statement)
}

pub type Expression {
  Access(Expression, List(String))
  Array(List(Expression))
  Arrow(List(String), List(Statement))
  Assign(Expression, Expression)
  Binop(Expression, Binop, Expression)
  Call(Expression, List(Expression))
  IIFE(Statement)
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
// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn as_expression(stmt: Statement) -> Expression {
  case stmt {
    Const(_, expr) -> expr
    Expr(expr) -> expr
    Function(_, args, body) -> Arrow(args, body)
    Return(expr) -> expr
    _ -> IIFE(stmt)
  }
}
// CONVERSIONS -----------------------------------------------------------------
// UTILS -----------------------------------------------------------------------
