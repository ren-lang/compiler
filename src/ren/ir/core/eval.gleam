// IMPORTS ---------------------------------------------------------------------

import gleam/list
import ren/ast/lit.{Lit}
import ren/ir/core.{Expr}

// EVAL ------------------------------------------------------------------------

///
///
pub fn run(expr: Expr) -> Expr {
  core.fold(
    expr,
    on_app,
    core.Lam,
    // NOTE: This lambda is currently necessary because of a bug in Gleam's code
    // generation for Erlang. If we use the `Let` variant directly it will spit
    // out a syntax error.
    fn(name, value, body) { core.Let(name, value, body) },
    core.Lit,
    core.Pat,
    core.Var,
  )
}

// HANDLERS --------------------------------------------------------------------

///
///
fn on_app(fun: Expr, args: List(Expr)) -> Expr {
  let expr = core.App(fun, args)

  case fun, args {
    // RECORD ACCESS -----------------------------------------------------------
    core.Var("$access"), [core.Lit(lit.Str(key)), core.Lit(lit.Obj(fields))] ->
      case list.find(fields, fn(field) { field.key == key }) {
        Ok(lit.Field(_, val)) -> val
        Error(_) -> expr
      }

    // BINOPS ------------------------------------------------------------------
    core.Var("$add"), _ -> {
      use x, y <- num_binop(expr, "$add")
      core.Lit(lit.Num(x +. y))
    }

    core.Var("$and"), _ -> {
      use a, b <- bool_binop(expr, "$and")
      core.Lit(lit.bool(a && b))
    }

    core.Var("$concat"), _ -> {
      use x, y <- str_binop(expr, "$concat")
      core.Lit(lit.Str(x <> y))
    }

    core.Var("$div"), _ -> {
      use x, y <- num_binop(expr, "$div")
      core.Lit(lit.Num(x /. y))
    }

    core.Var("$eq"), _ -> {
      use x, y <- binop(expr, "$eq")

      case is_simple_expr(x), is_simple_expr(y) {
        True, True -> core.Lit(lit.bool(x == y))
        _, _ -> expr
      }
    }

    core.Var("$gt"), _ -> {
      use x, y <- num_binop(expr, "$gt")
      core.Lit(lit.bool(x >. y))
    }

    core.Var("$gte"), _ -> {
      use x, y <- num_binop(expr, "$gte")
      core.Lit(lit.bool(x >=. y))
    }

    core.Var("$lt"), _ -> {
      use x, y <- num_binop(expr, "$lt")
      core.Lit(lit.bool(x <. y))
    }

    core.Var("$lte"), _ -> {
      use x, y <- num_binop(expr, "$lte")
      core.Lit(lit.bool(x <=. y))
    }

    core.Var("$mul"), _ -> {
      use x, y <- num_binop(expr, "$mul")
      core.Lit(lit.Num(x *. y))
    }

    core.Var("$neq"), _ -> {
      use x, y <- binop(expr, "$neq")

      case is_simple_expr(x), is_simple_expr(y) {
        True, True -> core.Lit(lit.bool(x != y))
        _, _ -> expr
      }
    }

    core.Var("$or"), _ -> {
      use a, b <- bool_binop(expr, "$or")
      core.Lit(lit.bool(a || b))
    }

    core.Var("$sub"), _ -> {
      use x, y <- num_binop(expr, "$sub")
      core.Lit(lit.Num(x -. y))
    }

    // IF EXPRESSIONS ----------------------------------------------------------
    core.Var("$if"), [core.Lit(lit.Con("true", [])), then, _] -> then
    core.Var("$if"), [core.Lit(lit.Con("false", [])), _, else] -> else

    _, _ -> expr
  }
}

// BINOP UTILS -----------------------------------------------------------------

fn binop(expr: Expr, op: String, f: fn(Expr, Expr) -> Expr) -> Expr {
  case expr {
    core.App(core.Var(fun), [x, y]) if fun == op -> f(x, y)
    _ -> expr
  }
}

fn num_binop(expr: Expr, op: String, f: fn(Float, Float) -> Expr) -> Expr {
  use lhs, rhs <- binop(expr, op)

  case lhs, rhs {
    core.Lit(lit.Num(x)), core.Lit(lit.Num(y)) -> f(x, y)
    _, _ -> expr
  }
}

fn str_binop(expr: Expr, op: String, f: fn(String, String) -> Expr) -> Expr {
  use lhs, rhs <- binop(expr, op)

  case lhs, rhs {
    core.Lit(lit.Str(x)), core.Lit(lit.Str(y)) -> f(x, y)
    _, _ -> expr
  }
}

fn bool_binop(expr: Expr, op: String, f: fn(Bool, Bool) -> Expr) -> Expr {
  use lhs, rhs <- binop(expr, op)

  case lhs, rhs {
    core.Lit(lit.Con("true", [])), core.Lit(lit.Con("true", [])) ->
      f(True, True)
    core.Lit(lit.Con("true", [])), core.Lit(lit.Con("false", [])) ->
      f(True, False)
    core.Lit(lit.Con("false", [])), core.Lit(lit.Con("true", [])) ->
      f(False, True)
    core.Lit(lit.Con("false", [])), core.Lit(lit.Con("false", [])) ->
      f(False, False)
    _, _ -> expr
  }
}

// UTILS -----------------------------------------------------------------------

fn is_simple_expr(expr: Expr) -> Bool {
  case expr {
    core.Lit(lit.Arr([])) -> True
    core.Lit(lit.Con(_, [])) -> True
    core.Lit(lit.Num(_)) -> True
    core.Lit(lit.Obj([])) -> True
    core.Lit(lit.Str(_)) -> True
    core.Var(_) -> True
    _ -> False
  }
}
