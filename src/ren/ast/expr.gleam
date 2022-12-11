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
