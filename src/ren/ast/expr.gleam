// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/string
import ren/ast/lit.{Arr, Con, Int, Lit, Num, Obj, Str}
import ren/ast/pat.{Alias, Bind, Pat, Typeof, Value, Wildcard}
import ren/data/token.{
  Add, And, Div, Eq, Gt, Gte, Lt, Lte, Mod, Mul, Neq, Operator, Or, Pipe, Pow,
  Seq, Sub,
}
import ren/ir/imp
import ren/util/debug
import ren/util/rec

// TYPES -----------------------------------------------------------------------

///
///
pub type Expr {
  Binop(Operator, Expr, Expr)
  Call(Expr, List(Expr))
  Fun(List(Pat), Expr)
  If(Expr, Expr, Expr)
  Let(Pat, Expr, Expr)
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

    Let(_, _, _) -> expr

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
    on_let: fn(pat, expr, body) { f(Let(pat, expr, body)) },
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
  on_let do_let: fn(Pat, a, a) -> a,
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
    on_let: fn(_, pat, expr, body) { do_let(pat, expr, body) },
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
  on_let do_let: fn(Expr, Pat, a, a) -> a,
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

    Let(pat, value, body) -> {
      use x <- rec.step(value)
      use y <- rec.step(body)
      rec.base(do_let(expr, pat, x, y))
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

pub fn to_imp(expr: Expr) -> imp.Statement {
  let to_branch_from_case = fn(name, case_) {
    let #(pat, guard, body) = case_

    imp.If(
      to_imp_checks_from_pat(pat, imp.Var(name)),
      case guard {
        Some(imp.Expr(guard)) ->
          list.flatten([
            to_imp_assignments_from_pat(pat, imp.Var(name)),
            [imp.If(guard, [body])],
          ])
        None ->
          list.flatten([to_imp_assignments_from_pat(pat, imp.Var(name)), [body]])
      },
    )
  }

  fold(
    expr,
    //
    on_binop: to_imp_from_binop,
    //
    on_call: fn(fun, args) {
      let fun_expr = imp.as_expression(fun)
      let arg_exprs = list.map(args, imp.as_expression)

      imp.Expr(imp.Call(fun_expr, arg_exprs))
    },
    //
    on_fun: fn(args, body) {
      let indexed_args = list.index_map(args, fn(i, arg) { #(i, arg) })
      let #(names, checks, assignments) = {
        use acc, arg <- list.fold_right(indexed_args, #([], [], []))
        let #(names, checks, assignments) = acc
        let #(index, pat) = arg

        case pat {
          pat.Bind(name) -> #([name, ..names], checks, assignments)
          _ -> {
            let name = "$" <> int.to_string(index)
            let check = to_imp_checks_from_pat(pat, imp.Var(name))
            let assignment = to_imp_assignments_from_pat(pat, imp.Var(name))
            #(
              [name, ..names],
              [check, ..checks],
              list.append(assignment, assignments),
            )
          }
        }
      }

      imp.Expr(imp.Call(
        imp.Var("$fn"),
        [
          imp.Arrow(
            names,
            case checks {
              [check, ..rest] -> [
                imp.return(imp.If(
                  list.fold_right(
                    rest,
                    check,
                    fn(a, b) { imp.Binop(a, imp.And, b) },
                  ),
                  list.append(assignments, [body]),
                )),
                imp.Throw("[MatchError] Non-exhaustive pattern match"),
              ]
              [] -> [imp.return(imp.Block(list.append(assignments, [body])))]
            },
          ),
        ],
      ))
    },
    on_if: fn(cond, then, else) {
      imp.IfElse(imp.as_expression(cond), [then], [else])
    },
    on_let: fn(pat, expr, body) {
      case pat, imp.as_expression(expr) {
        Bind(name), expr -> imp.Block([imp.Const(name, expr), body])
      }
    },
    on_literal: fn(lit) {
      case lit {
        Arr(elements) ->
          imp.Expr(imp.Array(list.map(elements, imp.as_expression)))
        Con(tag, args) ->
          imp.Expr(imp.Array([
            imp.String(tag),
            ..list.map(args, imp.as_expression)
          ]))
        Int(n) -> imp.Expr(imp.Number(int.to_float(n)))
        Num(n) -> imp.Expr(imp.Number(n))
        Obj(fields) ->
          imp.Expr(imp.Object(list.map(
            fields,
            fn(field) { #(field.0, imp.as_expression(field.1)) },
          )))
        Str(s) -> imp.Expr(imp.String(s))
      }
    },
    on_placeholder: fn() {
      debug.crash(
        "expr.gleam",
        509,
        "Placeholder should not be present in the AST",
      )
    },
    on_switch: fn(expr, cases) {
      imp.Block(list.flatten([
        [imp.Const("$switch", imp.as_expression(expr))],
        list.map(
          cases,
          fn(case_) { imp.return(to_branch_from_case("$switch", case_)) },
        ),
        [imp.Throw("[MatchError] Non-exhaustive pattern match")],
      ]))
    },
    on_var: fn(name) { imp.Expr(imp.Var(name)) },
  )
}

///
///
fn to_imp_from_binop(
  op: Operator,
  lhs: imp.Statement,
  rhs: imp.Statement,
) -> imp.Statement {
  let lhs_expr = imp.as_expression(lhs)
  let rhs_expr = imp.as_expression(rhs)

  case op {
    Add -> imp.Expr(imp.Binop(lhs_expr, imp.Add, rhs_expr))
    And -> imp.Expr(imp.Binop(lhs_expr, imp.And, rhs_expr))
    Div -> imp.Expr(imp.Binop(lhs_expr, imp.Div, rhs_expr))
    Eq -> imp.Expr(imp.Binop(lhs_expr, imp.Eq, rhs_expr))
    Gt -> imp.Expr(imp.Binop(lhs_expr, imp.Gt, rhs_expr))
    Gte -> imp.Expr(imp.Binop(lhs_expr, imp.Gte, rhs_expr))
    Lt -> imp.Expr(imp.Binop(lhs_expr, imp.Lt, rhs_expr))
    Lte -> imp.Expr(imp.Binop(lhs_expr, imp.Lte, rhs_expr))
    Mod -> imp.Expr(imp.Binop(lhs_expr, imp.Mod, rhs_expr))
    Mul -> imp.Expr(imp.Binop(lhs_expr, imp.Mul, rhs_expr))
    Neq -> imp.Expr(imp.Binop(lhs_expr, imp.Neq, rhs_expr))
    Or -> imp.Expr(imp.Binop(lhs_expr, imp.Or, rhs_expr))
    Pipe -> imp.Expr(imp.Call(rhs_expr, [lhs_expr]))
    Pow -> imp.Expr(imp.Binop(lhs_expr, imp.Pow, rhs_expr))
    Seq -> imp.Block([lhs, rhs])
    Sub -> imp.Expr(imp.Binop(lhs_expr, imp.Sub, rhs_expr))
  }
}

fn to_imp_checks_from_pat(pat: Pat, expr: imp.Expression) -> imp.Expression {
  // Patterns like the wildcard `_` are always just `true`. This is necessary
  // to handle top-level patterns that match on anything but once we're
  // doing some sort of compound check we can safely eliminate them.
  let and = fn(lhs, rhs) {
    case lhs, rhs {
      imp.JSTrue, _ -> rhs
      _, imp.JSTrue -> lhs
      _, _ -> imp.Binop(lhs, imp.And, rhs)
    }
  }

  let to_imp_checks_from_arr = fn(elements) {
    // We'll want to first check that the expression is actually an array, so
    // we don't have to waste time checking the length or indexing into it if
    // it doesn't make sense to.
    let is_array =
      imp.Call(imp.Access(imp.Var("globalThis"), ["Array", "isArray"]), [expr])
    // Similarly, we'll want to check that the length of the array is correct
    // so we don't have to worry about out-of-bounds errors.
    let length =
      imp.Binop(
        imp.Access(expr, ["length"]),
        imp.Gte,
        imp.Number(int.to_float(list.length(elements))),
      )
    let init = and(is_array, length)
    // Now we can fold over the patterns and generate the checks for each
    // element. This ignores constant `true` checks because they don't change
    // the result.
    use checks, pat, i <- list.index_fold(elements, init)
    let index = imp.Number(int.to_float(i))
    let expr = imp.Index(expr, index)
    and(checks, to_imp_checks_from_pat(pat, expr))
  }

  let to_imp_checks_from_obj = fn(fields) {
    // As with the `is_array` check, we'll first want to make sure what we're
    // dealing with is actually an object.
    let is_object =
      imp.Binop(imp.Unop(imp.Typeof, expr), imp.Eq, imp.String("object"))
    use pats, field <- list.fold(fields, is_object)
    let #(key, pat) = field
    // For each key, before we bother checking the pattnern we first always
    // want to verify that the key exists in the object.
    let key_in = imp.Binop(imp.String(key), imp.In, expr)
    let pats = and(pats, key_in)
    let expr = imp.Access(expr, [key])
    and(pats, to_imp_checks_from_pat(pat, expr))
  }

  case pat {
    pat.Alias(_, _) ->
      debug.crash("expr.gleam", 511, "Alias patterns unsupported")
    pat.Bind(_) -> imp.JSTrue
    pat.Value(Arr(elements)) -> to_imp_checks_from_arr(elements)
    pat.Value(Con(tag, args)) ->
      to_imp_checks_from_arr([pat.Value(Str(tag)), ..args])
    pat.Value(Int(n)) -> imp.Binop(expr, imp.Eq, imp.Number(int.to_float(n)))
    pat.Value(Num(n)) -> imp.Binop(expr, imp.Eq, imp.Number(n))
    pat.Value(Obj(fields)) -> to_imp_checks_from_obj(fields)
    pat.Value(Str(s)) -> imp.Binop(expr, imp.Eq, imp.String(s))
    pat.Wildcard -> imp.JSTrue
    // 
    pat.Typeof("Array", pat.Value(Arr(elements))) ->
      to_imp_checks_from_arr(elements)
    pat.Typeof("Array", _) ->
      imp.Call(imp.Access(imp.Var("globalThis"), ["Array", "isArray"]), [expr])
    // There is a catch-all case below that will perform the `typeof expr == "object"`
    // check below, but that is not sufficient for the `Object` primitive type
    // because that check will also check if the constructor name matches the
    // pattern type name.
    //
    // For `@Object` patterns we want to be able to match *any* JavaScript object,
    // so we *only* need to perform the `typeof` check and any checks from the
    // inner pattern.
    pat.Typeof("Object", pat.Value(Obj(fields))) ->
      to_imp_checks_from_obj(fields)
    pat.Typeof("Object", _) ->
      imp.Binop(imp.Unop(imp.Typeof, expr), imp.Eq, imp.String("object"))
    // For primitive types, we need to check either the `typeof` or the
    // object constructor name matches the primitive type. This is because when
    // performing `typeof` on primitives constructed using `new`, such as
    // `new Number(1)`, the operation will return `"object"` and *not* `"Number"`.
    pat.Typeof("Boolean" as t, pat) | pat.Typeof("Number" as t, pat) | pat.Typeof(
      "String" as t,
      pat,
    ) -> {
      let typeof =
        imp.Binop(
          imp.Unop(imp.Typeof, expr),
          imp.Eq,
          imp.String(string.lowercase(t)),
        )
      let constructor =
        imp.Binop(
          imp.Access(expr, ["constructor", "name"]),
          imp.Eq,
          imp.String(t),
        )
      let is_type = imp.Binop(typeof, imp.Or, constructor)
      // Now we can check the main pattern against the expression.
      and(is_type, to_imp_checks_from_pat(pat, expr))
    }
    pat.Typeof(t, pat) -> {
      let typeof =
        imp.Binop(imp.Unop(imp.Typeof, expr), imp.Eq, imp.String("object"))
      let constructor =
        imp.Binop(
          imp.Access(expr, ["constructor", "name"]),
          imp.Eq,
          imp.String(t),
        )
      // Note how for primitives this check was an *or* check, but for classes
      // and objects it's now an *and* check. 
      let is_type = and(typeof, constructor)
      // Now we can check the main pattern against the expression.
      and(is_type, to_imp_checks_from_pat(pat, expr))
    }
  }
}

fn to_imp_assignments_from_pat(
  pat: Pat,
  expr: imp.Expression,
) -> List(imp.Statement) {
  case pat {
    pat.Alias(_, _) ->
      debug.crash("expr.gleam", 689, "Alias patterns unsupported")
    pat.Bind(name) -> [imp.Const(name, expr)]
    pat.Value(Arr(elements)) -> {
      use assignments, pat, i <- list.index_fold(elements, [])
      let index = imp.Number(int.to_float(i))
      list.append(
        assignments,
        to_imp_assignments_from_pat(pat, imp.Index(expr, index)),
      )
    }
    pat.Value(Con(_, args)) -> {
      let arr = pat.Value(Arr([pat.Wildcard, ..args]))
      to_imp_assignments_from_pat(arr, expr)
    }
    pat.Value(Int(_)) -> []
    pat.Value(Num(_)) -> []
    pat.Value(Obj(fields)) -> {
      use assignments, field <- list.fold(fields, [])
      let #(key, pat) = field
      let expr = imp.Access(expr, [key])
      list.append(assignments, to_imp_assignments_from_pat(pat, expr))
    }
    pat.Value(Str(_)) -> []
    pat.Wildcard -> []
    pat.Typeof(_, pat) -> to_imp_assignments_from_pat(pat, expr)
  }
}
// UTILS -----------------------------------------------------------------------
