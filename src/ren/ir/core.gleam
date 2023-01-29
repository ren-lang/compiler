// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import gleam/option.{None, Some}
import ren/ast/expr
import ren/ast/lit.{Arr, Con, Field, Lit, Num, Obj, Str}
import ren/ast/pat.{Bind, Case, Pat, Value, Wildcard}
import ren/data/token
import ren/util/debug
import ren/util/rec

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  App(Expr, List(Expr))
  Lam(List(String), Expr)
  Let(String, Expr, Expr)
  Lit(Lit(Expr))
  Pat(Expr, List(Case(Expr)))
  Var(String)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn from_expr(expr: expr.Expr) -> Expr {
  expr.fold(
    expr,
    on_binop: from_binop_expr,
    on_call: App,
    on_fun: from_fun_expr,
    on_if: from_if_expr,
    on_let: from_let_expr,
    on_literal: Lit,
    on_placeholder: fn() {
      debug.crash("core.gleam", 40, "Placeholders should have been removed")
    },
    on_switch: Pat,
    on_var: Var,
  )
}

fn from_binop_expr(op: token.Operator, lhs: Expr, rhs: Expr) -> Expr {
  case lhs, op, rhs {
    _, token.Add, _ -> App(Var("$add"), [lhs, rhs])
    _, token.And, _ -> App(Var("$and"), [lhs, rhs])
    _, token.Concat, _ -> App(Var("$concat"), [lhs, rhs])
    _, token.Div, _ -> App(Var("$div"), [lhs, rhs])
    _, token.Eq, _ -> App(Var("$eq"), [lhs, rhs])
    _, token.Gt, _ -> App(Var("$gt"), [lhs, rhs])
    _, token.Gte, _ -> App(Var("$gte"), [lhs, rhs])
    _, token.Lt, _ -> App(Var("$lt"), [lhs, rhs])
    _, token.Lte, _ -> App(Var("$lte"), [lhs, rhs])
    _, token.Mod, _ -> App(Var("$mod"), [lhs, rhs])
    _, token.Mul, _ -> App(Var("$mul"), [lhs, rhs])
    _, token.Neq, _ -> App(Var("$neq"), [lhs, rhs])
    _, token.Or, _ -> App(Var("$or"), [lhs, rhs])
    _, token.Pipe, _ -> App(Var("$pipe"), [lhs, rhs])
    _, token.Pow, _ -> App(Var("$pow"), [lhs, rhs])
    Let(name, value, Lit(Con("undefined", []))), token.Seq, rhs ->
      Let(name, value, rhs)
    Let(
      "$pat",
      value,
      Pat(Var("$pat"), [Case(pat, None, Lit(Con("undefined", [])))]),
    ), token.Seq, rhs ->
      Let("$pat", value, Pat(Var("$pat"), [Case(pat, None, rhs)]))
    _, token.Seq, _ -> App(Var("$seq"), [lhs, rhs])
    _, token.Sub, _ -> App(Var("$sub"), [lhs, rhs])
  }
}

fn from_fun_expr(pats: List(Pat), body: Expr) -> Expr {
  // Pair each pattern with a temporary positional name for that argument.
  let pats_and_names =
    list.index_map(
      pats,
      fn(i, p) {
        let name = "$arg" <> int.to_string(i)
        case p {
          Wildcard -> #(Bind(name), name)
          Bind(name) -> #(p, name)
          _ -> #(p, name)
        }
      },
    )
  // Split the patterns into simple ones that don't require any logic to match
  // and complex ones that do.
  let #(_, complex_pats) =
    list.partition(pats_and_names, fn(p) { pat.is_simple(p.0) })
  let #(complex_pats, complex_names) = list.unzip(complex_pats)

  case complex_pats {
    [] -> {
      let args = list.map(pats_and_names, fn(p) { p.1 })
      Lam(args, body)
    }

    [pat] -> {
      assert [name] = complex_names
      let args = list.map(pats_and_names, fn(p) { p.1 })
      let expr = Var(name)
      let body = Pat(expr, [Case(pat, None, body)])
      Lam(args, body)
    }

    _ -> {
      let args = list.map(pats_and_names, fn(p) { p.1 })
      let expr = Lit(Arr(list.map(complex_names, Var)))
      let pat = Value(Arr(complex_pats))
      let body = Pat(expr, [Case(pat, None, body)])
      Lam(args, body)
    }
  }
}

fn from_if_expr(cond: Expr, then: Expr, else: Expr) -> Expr {
  App(Var("$if"), [cond, then, else])
}

fn from_let_expr(pat: Pat, value: Expr) -> Expr {
  let body = Lit(Con("undefined", []))
  case pat {
    Bind(name) -> Let(name, value, body)
    Wildcard -> App(Var("$seq"), [value, body])
    _ -> Let("$pat", value, Pat(Var("$pat"), [Case(pat, None, body)]))
  }
}

// QUERIES ---------------------------------------------------------------------
// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn transform(expr: Expr, f: fn(Expr) -> Expr) -> Expr {
  fold(
    expr,
    on_app: fn(fun, args) { f(App(fun, args)) },
    on_lam: fn(args, body) { f(Lam(args, body)) },
    on_let: fn(name, value, body) { f(Let(name, value, body)) },
    on_lit: fn(lit) { f(Lit(lit)) },
    on_pat: fn(expr, cases) { f(Pat(expr, cases)) },
    on_var: fn(var) { f(Var(var)) },
  )
}

///
///
pub fn transform_many(expr: Expr, steps: List(fn(Expr) -> Expr)) -> Expr {
  transform(expr, do_transform_many(_, steps))
}

fn do_transform_many(expr: Expr, steps: List(fn(Expr) -> Expr)) -> Expr {
  let next = list.fold(steps, expr, fn(expr, step) { step(expr) })

  case next == expr {
    True -> expr
    False -> transform_many(next, steps)
  }
}

///
///
pub fn fold(
  over expr: Expr,
  on_app do_app: fn(a, List(a)) -> a,
  on_lam do_lam: fn(List(String), a) -> a,
  on_let do_let: fn(String, a, a) -> a,
  on_lit do_lit: fn(Lit(a)) -> a,
  on_pat do_pat: fn(a, List(Case(a))) -> a,
  on_var do_var: fn(String) -> a,
) -> a {
  use expr <- rec.run(expr)

  case expr {
    App(fun, args) -> {
      use x <- rec.step(fun)
      use y <- rec.list(args)
      rec.base(do_app(x, y))
    }

    Lam(arg, body) -> {
      use x <- rec.step(body)
      rec.base(do_lam(arg, x))
    }

    Let(name, value, body) -> {
      use x <- rec.step(value)
      use y <- rec.step(body)
      rec.base(do_let(name, x, y))
    }

    Lit(Arr(elements)) -> {
      use x <- rec.list(elements)
      rec.base(do_lit(Arr(x)))
    }

    Lit(Con(tag, args)) -> {
      use x <- rec.list(args)
      rec.base(do_lit(Con(tag, x)))
    }

    Lit(Num(value)) -> {
      let x = do_lit(Num(value))
      rec.base(x)
    }

    Lit(Obj(fields)) -> {
      let do_field = fn(field: Field(r)) {
        use x <- rec.step(field.value)
        rec.base(Field(field.key, x))
      }
      use x <- rec.traverse(fields, do_field)
      rec.base(do_lit(Obj(x)))
    }

    Lit(Str(value)) -> {
      let x = do_lit(Str(value))
      rec.base(x)
    }

    Pat(expr, cases) -> {
      use x <- rec.step(expr)
      let do_case = fn(case_: Case(r)) {
        let Case(pat, guard, body) = case_
        case guard {
          Some(guard) -> {
            use x <- rec.step(guard)
            use y <- rec.step(body)
            rec.base(Case(pat, Some(x), y))
          }
          None -> {
            use x <- rec.step(body)
            rec.base(Case(pat, None, x))
          }
        }
      }
      use y <- rec.traverse(cases, do_case)
      rec.base(do_pat(x, y))
    }

    Var(name) -> {
      let x = do_var(name)
      rec.base(x)
    }
  }
}
