// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{None, Some}
import ren/ast/lit.{Arr, Con, Field, Lit, Num, Obj, Str}
import ren/ast/pat.{Case, Pat}
import ren/data/token.{
  Add, And, Concat, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Neq, Operator, Or, Pipe,
  Pow, Seq, Sub,
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
  Switch(Expr, List(Case(Expr)))
  Var(String)
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
pub fn concat(lhs: Expr, rhs: Expr) -> Expr {
  Binop(Concat, lhs, rhs)
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
pub fn num(value: Float) -> Expr {
  Literal(Num(value))
}

///
///
pub fn rec(fields: List(Field(Expr))) -> Expr {
  Literal(Obj(fields))
}

///
///
pub fn str(value: String) -> Expr {
  Literal(Str(value))
}

// MANIPULATIONS ---------------------------------------------------------------

/// Given a function to transform an expression, `transform` will apply that
/// function recurisvely bottom-up to every expression in the tree. This is a
/// simplified case of the `fold` function below.
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
pub fn transform_many(expr: Expr, fns: List(fn(Expr) -> Expr)) -> Expr {
  case list.fold(fns, expr, fn(expr, f) { f(expr) }) {
    next if next == expr -> expr
    next -> transform_many(next, fns)
  }
}

/// In the same way `list.fold` reduces a list down to a single value, this 
/// function is used to reduce an expression down to one. We pass in a callback
/// for every variant of our `Expr` type that works to fold that variant down.
/// The callbacks are applied recursively, bottom up, so we only have to think
/// about one "level" in the tree at a time.
///
/// Note that in places where we would expect to see a child `Expr` and a recursive
/// call to fold that the result has already been computed and replaced with `a`.
/// For example our `Binop` variant looks like `Binop(Operator, Expr, Expr)` but
/// the callback we pass in takes `fn(Operator, a, a) -> a` instead!
///
/// ❗️ This function is implemented in a way that makes arbitrarily deep traversal
/// *stack safe* at the cost of some performace. 
///
/// 💡 For those familiar with tree traversals in OOP languages, you can think of
/// this function as an example of the visitor pattern.
///
/// 💡 For those curious about more arcane functional programming things, this
/// function is typically called `cata` - meaning "catamorphism".
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
  on_switch do_switch: fn(a, List(Case(a))) -> a,
  on_var do_var: fn(String) -> a,
) -> a {
  use expr <- rec.run(expr)

  case expr {
    Binop(op, lhs, rhs) -> {
      use x <- rec.step(lhs)
      use y <- rec.step(rhs)
      rec.base(do_binop(op, x, y))
    }

    Call(fun, arg) -> {
      use x <- rec.step(fun)
      use y <- rec.list(arg)
      rec.base(do_call(x, y))
    }

    Fun(pats, body) -> {
      use x <- rec.step(body)
      rec.base(do_fun(pats, x))
    }

    If(cond, then, else) -> {
      use x <- rec.step(cond)
      use y <- rec.step(then)
      use z <- rec.step(else)
      rec.base(do_if(x, y, z))
    }

    Let(pat, value) -> {
      use x <- rec.step(value)
      rec.base(do_let(pat, x))
    }

    Literal(Arr(elements)) -> {
      use x <- rec.list(elements)
      rec.base(do_literal(Arr(x)))
    }

    Literal(Con(tag, args)) -> {
      use x <- rec.list(args)
      rec.base(do_literal(Con(tag, x)))
    }

    Literal(Num(n)) -> {
      let x = do_literal(Num(n))
      rec.base(x)
    }

    Literal(Obj(fields)) -> {
      let do_field = fn(field: Field(r)) {
        use x <- rec.step(field.value)
        rec.base(Field(field.key, x))
      }
      use x <- rec.traverse(fields, do_field)
      rec.base(do_literal(Obj(x)))
    }

    Literal(Str(s)) -> {
      let x = do_literal(Str(s))
      rec.base(x)
    }

    Placeholder -> {
      let x = do_placeholder()
      rec.base(x)
    }

    Switch(cond, cases) -> {
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
      use x <- rec.step(cond)
      use y <- rec.traverse(cases, do_case)
      rec.base(do_switch(x, y))
    }

    Var(name) -> {
      let x = do_var(name)
      rec.base(x)
    }
  }
}
