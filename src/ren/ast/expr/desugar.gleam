// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import ren/ast/expr.{Expr}
import ren/ast/lit
import ren/ast/pat

// DESUGAR ---------------------------------------------------------------------

pub fn all(expr: Expr) -> Expr {
  expr.transform(expr, expr.transform_many(_, [placeholders]))
}

// PLACEHOLDERS ----------------------------------------------------------------

pub fn placeholders(expr: Expr) -> Expr {
  case expr {
    expr.Binop(op, lhs, rhs) -> {
      let names = to_names([lhs, rhs])
      let lhs = replace_placeholder(lhs, 0)
      let rhs = replace_placeholder(rhs, 1)

      expr.Binop(op, lhs, rhs)
      |> to_lambda(names)
    }

    expr.Call(fun, args) -> {
      let names = to_names([fun, ..args])
      let fun = replace_placeholder(fun, 0)
      let args = replace_placeholders(args, 1)

      expr.Call(fun, args)
      |> to_lambda(names)
    }

    expr.If(cond, then, else) -> {
      let names = to_names([cond, then, else])
      let cond = replace_placeholder(cond, 0)
      let then = replace_placeholder(then, 1)
      let else = replace_placeholder(else, 2)

      expr.If(cond, then, else)
      |> to_lambda(names)
    }

    expr.Literal(lit.Arr(elements)) -> {
      let names = to_names(elements)
      let elements = replace_placeholders(elements, 0)

      expr.Literal(lit.Arr(elements))
      |> to_lambda(names)
    }

    expr.Literal(lit.Con(name, args)) -> {
      let args = replace_placeholders(args, 0)
      let names = to_names(args)

      expr.Literal(lit.Con(name, args))
      |> to_lambda(names)
    }

    expr.Literal(lit.Obj(fields)) -> {
      let #(keys, values) = list.unzip(lit.key_val_pairs(fields))
      let names = to_names(values)
      let fields =
        replace_placeholders(values, 0)
        |> list.zip(keys, _)
        |> list.map(fn(field) { lit.Field(field.0, field.1) })

      expr.Literal(lit.Obj(fields))
      |> to_lambda(names)
    }

    expr.Switch(expr, cases) -> {
      let names = to_names([expr])
      let expr = replace_placeholder(expr, 0)

      expr.Switch(expr, cases)
      |> to_lambda(names)
    }

    _ -> expr
  }
}

// UTILS -----------------------------------------------------------------------

fn to_name(idx: Int) {
  "_" <> int.to_string(idx)
}

fn to_names(exprs: List(Expr)) {
  list.reverse({
    use names, expr, idx <- list.index_fold(exprs, [])

    case expr {
      expr.Placeholder -> [to_name(idx), ..names]
      _ -> names
    }
  })
}

fn replace_placeholder(expr, idx) {
  case expr {
    expr.Placeholder -> expr.Var(to_name(idx))
    _ -> expr
  }
}

fn replace_placeholders(exprs, offset) {
  use idx, expr <- list.index_map(exprs)

  replace_placeholder(expr, idx + offset)
}

fn to_lambda(body, names) {
  let args = list.map(names, pat.Bind)

  case args {
    [] -> body
    _ -> expr.Fun(args, body)
  }
}
