// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import gleam/option.{None, Option, Some}
import ren/ast/lit.{Arr, Con, Int, Lit, Num, Obj, Str}
import ren/ast/pat.{Pat}
import ren/data/token.{
  Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Neq, Operator, Or, Pipe, Pow,
  Seq, Sub,
}
import ren/util/rec

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  Binop(Operator, Expr, Expr)
  Call(Expr, List(Expr))
  Fun(List(Pat), Expr)
  If(Expr, Expr, Expr)
  Let(Pat, Expr)
  Literal(Lit(Expr))
  Placeholder
  Switch(Expr, List(#(Pat, Option(Expr), Expr)))
  Var(String)
}

// CONSTANTS -------------------------------------------------------------------
// CONSTRUCTORS ----------------------------------------------------------------
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
  Literal(Arr(elements))
}

///
///
pub fn enum(name: String, args: List(Expr)) -> Expr {
  Literal(Con(name, args))
}

///
///
pub fn int(value: Int) -> Expr {
  Literal(Int(value))
}

///
///
pub fn num(value: Float) -> Expr {
  Literal(Num(value))
}

///
///
pub fn rec(fields: List(#(String, Expr))) -> Expr {
  Literal(Obj(fields))
}

///
///
pub fn str(value: String) -> Expr {
  Literal(Str(value))
}

// QUERIES ---------------------------------------------------------------------
// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn desugar(expr: Expr) -> Expr {
  transform(expr, replace_placeholders)
}

fn replace_placeholders(expr: Expr) -> Expr {
  let name = fn(idx) { "_" <> int.to_string(idx) }

  let names = fn(exprs) {
    list.reverse({
      use names, expr, idx <- list.index_fold(exprs, [])
      case expr {
        Placeholder -> [name(idx), ..names]
        _ -> names
      }
    })
  }

  let replace = fn(expr, idx) {
    case expr {
      Placeholder -> Var(name(idx))
      _ -> expr
    }
  }

  let replace_all = fn(exprs, offset) {
    list.index_map(exprs, fn(i, e) { replace(e, i + offset) })
  }

  let as_lambda = fn(body, names) {
    case names {
      [] -> expr
      _ -> Fun(list.map(names, pat.Bind), body)
    }
  }

  case expr {
    Binop(op, lhs, rhs) ->
      Binop(op, replace(lhs, 0), replace(rhs, 1))
      |> as_lambda(names([lhs, rhs]))

    Call(fun, args) ->
      Call(replace(fun, 0), replace_all(args, 1))
      |> as_lambda(names([fun, ..args]))

    Fun(_, _) -> expr

    If(cond, then, else) ->
      If(replace(cond, 0), replace(then, 1), replace(else, 2))
      |> as_lambda(names([cond, then, else]))

    Let(_, _) -> expr

    Literal(Arr(elements)) ->
      Literal(Arr(replace_all(elements, 0)))
      |> as_lambda(names(elements))

    Literal(Con(name, args)) ->
      Literal(Con(name, replace_all(args, 0)))
      |> as_lambda(names(args))

    Literal(Int(_)) -> expr

    Literal(Num(_)) -> expr

    Literal(Obj(fields)) -> {
      let #(keys, values) = list.unzip(fields)
      values
      |> replace_all(1)
      |> list.zip(keys, _)
      |> fn(fields) { Literal(Obj(fields)) }
      |> as_lambda(names(values))
    }

    Literal(Str(_)) -> expr

    Placeholder -> expr

    Switch(expr, cases) ->
      Switch(replace(expr, 0), cases)
      |> as_lambda(names([expr]))

    Var(_) -> expr
  }
}

///
///
pub fn transform(expr: Expr, f: fn(Expr) -> Expr) -> Expr {
  fold(
    expr,
    on_binop: fn(op, lhs, rhs) { f(Binop(op, lhs, rhs)) },
    on_call: fn(fun, args) { f(Call(fun, args)) },
    on_fun: fn(params, body) { f(Fun(params, body)) },
    on_if: fn(cond, then, else) { f(If(cond, then, else)) },
    on_let: fn(pat, expr) { f(Let(pat, expr)) },
    on_literal: fn(lit) { f(Literal(lit)) },
    on_placeholder: fn() { f(Placeholder) },
    on_switch: fn(expr, cases) { f(Switch(expr, cases)) },
    on_var: fn(name) { f(Var(name)) },
  )
}

///
///
pub fn fold(
  over expr: Expr,
  on_binop do_binop: fn(Operator, a, a) -> a,
  on_call do_call: fn(a, List(a)) -> a,
  on_fun do_fun: fn(List(Pat), a) -> a,
  on_if do_if: fn(a, a, a) -> a,
  on_let do_let: fn(Pat, a) -> a,
  on_literal do_literal: fn(Lit(a)) -> a,
  on_placeholder do_placeholder: fn() -> a,
  on_switch do_switch: fn(a, List(#(Pat, Option(a), a))) -> a,
  on_var do_var: fn(String) -> a,
) -> a {
  fold_with_expr(
    expr,
    on_binop: fn(_, op, lhs, rhs) { do_binop(op, lhs, rhs) },
    on_call: fn(_, fun, args) { do_call(fun, args) },
    on_fun: fn(_, params, body) { do_fun(params, body) },
    on_if: fn(_, cond, then, else) { do_if(cond, then, else) },
    on_let: fn(_, pat, expr) { do_let(pat, expr) },
    on_literal: fn(_, lit) { do_literal(lit) },
    on_placeholder: fn(_) { do_placeholder() },
    on_switch: fn(_, expr, cases) { do_switch(expr, cases) },
    on_var: fn(_, name) { do_var(name) },
  )
}

///
///
pub fn fold_with_expr(
  over expr: Expr,
  on_binop do_binop: fn(Expr, Operator, a, a) -> a,
  on_call do_call: fn(Expr, a, List(a)) -> a,
  on_fun do_fun: fn(Expr, List(Pat), a) -> a,
  on_if do_if: fn(Expr, a, a, a) -> a,
  on_let do_let: fn(Expr, Pat, a) -> a,
  on_literal do_literal: fn(Expr, Lit(a)) -> a,
  on_placeholder do_placeholder: fn(Expr) -> a,
  on_switch do_switch: fn(Expr, a, List(#(Pat, Option(a), a))) -> a,
  on_var do_var: fn(Expr, String) -> a,
) -> a {
  use expr <- rec.run(expr)

  case expr {
    Binop(op, lhs, rhs) -> {
      use x <- rec.step(lhs)
      use y <- rec.step(rhs)
      rec.base(do_binop(expr, op, x, y))
    }

    Call(fun, arg) -> {
      use x <- rec.step(fun)
      use y <- rec.list(arg)
      rec.base(do_call(expr, x, y))
    }

    Fun(pats, body) -> {
      use x <- rec.step(body)
      rec.base(do_fun(expr, pats, x))
    }

    If(cond, then, else) -> {
      use x <- rec.step(cond)
      use y <- rec.step(then)
      use z <- rec.step(else)
      rec.base(do_if(expr, x, y, z))
    }

    Let(pat, value) -> {
      use x <- rec.step(value)
      rec.base(do_let(expr, pat, x))
    }

    Literal(Arr(elements)) -> {
      use x <- rec.list(elements)
      rec.base(do_literal(expr, Arr(x)))
    }

    Literal(Con(tag, args)) -> {
      use x <- rec.list(args)
      rec.base(do_literal(expr, Con(tag, x)))
    }

    Literal(Int(n)) -> rec.base(do_literal(expr, Int(n)))

    Literal(Num(n)) -> rec.base(do_literal(expr, Num(n)))

    Literal(Obj(fields)) -> {
      let do_field = fn(field: #(String, r)) {
        use x <- rec.step(field.1)
        rec.base(#(field.0, x))
      }
      use x <- rec.traverse(fields, do_field)
      rec.base(do_literal(expr, Obj(x)))
    }

    Literal(Str(s)) -> rec.base(do_literal(expr, Str(s)))
    Placeholder -> rec.base(do_placeholder(expr))

    Switch(cond, cases) -> {
      let do_case = fn(case_: #(Pat, Option(r), r)) {
        let #(pat, guard, body) = case_
        case guard {
          Some(guard) -> {
            use x <- rec.step(guard)
            use y <- rec.step(body)
            rec.base(#(pat, Some(x), y))
          }
          None -> {
            use x <- rec.step(body)
            rec.base(#(pat, None, x))
          }
        }
      }
      use x <- rec.step(cond)
      use y <- rec.traverse(cases, do_case)
      rec.base(do_switch(expr, x, y))
    }

    Var(name) -> rec.base(do_var(expr, name))
  }
}
// CONVERSIONS -----------------------------------------------------------------

// UTILS -----------------------------------------------------------------------
