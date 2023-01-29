// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/map.{Map}
import gleam/function
import ren/ast/lit.{Field}
import ren/ir/imp.{Binop, Expression, Statement, Unop}

// TYPES -----------------------------------------------------------------------

type Step(a) =
  fn(Map(String, Expression)) -> a

// INLINE ----------------------------------------------------------------------

///
///
pub fn run(stmt: Statement) -> Statement {
  let init = map.new()
  let run =
    imp.fold(
      stmt,
      on_access: on_access,
      on_array: on_array,
      on_arrow: on_arrow,
      on_assign: on_assign,
      on_binop: on_binop,
      on_block: on_block,
      on_break: function.constant(imp.Break),
      on_call: on_call,
      on_comment: on_comment,
      on_const: on_const,
      on_continue: function.constant(imp.Continue),
      on_export: on_export,
      on_expr: on_expr,
      on_false: function.constant(imp.JSFalse),
      on_for_in: on_for_in,
      on_function: on_function,
      on_if: on_if,
      on_if_else: on_if_else,
      on_iife: on_iife,
      on_index: on_index,
      on_let: on_let,
      on_null: function.constant(imp.Null),
      on_number: on_number,
      on_object: on_object,
      on_return: on_return,
      on_spread: on_spread,
      on_string: on_string,
      on_ternary: on_ternary,
      on_throw: on_throw,
      on_true: function.constant(imp.JSTrue),
      on_undefined: function.constant(imp.Undefined),
      on_unop: on_unop,
      on_var: on_var,
      on_while: on_while,
    )

  run(init)
}

// HANDLERS --------------------------------------------------------------------

fn on_access(expr: Step(Expression), keys: List(String)) -> Step(Expression) {
  fn(bindings) {
    let expr = expr(bindings)

    imp.Access(expr, keys)
  }
}

fn on_array(elements: List(Step(Expression))) -> Step(Expression) {
  fn(bindings) {
    let elements = list.map(elements, fn(el) { el(bindings) })

    imp.Array(elements)
  }
}

fn on_arrow(args: List(String), body: List(Step(Statement))) -> Step(Expression) {
  fn(bindings) {
    let bindings = list.fold(args, bindings, map.delete)
    let body = fold_statements(body, bindings)

    imp.Arrow(args, body)
  }
}

fn on_assign(name: String, value: Step(Expression)) -> Step(Expression) {
  fn(bindings) {
    let value = value(bindings)

    imp.Assign(name, value)
  }
}

fn on_binop(
  lhs: Step(Expression),
  op: Binop,
  rhs: Step(Expression),
) -> Step(Expression) {
  fn(bindings) {
    let lhs = lhs(bindings)
    let rhs = rhs(bindings)

    imp.Binop(lhs, op, rhs)
  }
}

fn on_block(stmts: List(Step(Statement))) -> Step(Statement) {
  fn(bindings) {
    let stmts = fold_statements(stmts, bindings)

    imp.Block(stmts)
  }
}

fn on_call(
  fun: Step(Expression),
  args: List(Step(Expression)),
) -> Step(Expression) {
  fn(bindings) {
    let fun = fun(bindings)
    let args = list.map(args, fn(arg) { arg(bindings) })

    imp.Call(fun, args)
  }
}

fn on_comment(text: String) -> Step(Statement) {
  fn(_) { imp.Comment(text) }
}

fn on_const(name: String, value: Step(Expression)) -> Step(Statement) {
  fn(bindings) {
    let value = value(bindings)

    imp.Const(name, value)
  }
}

fn on_export(stmt: Step(Statement)) -> Step(Statement) {
  fn(bindings) {
    let stmt = stmt(bindings)

    imp.Export(stmt)
  }
}

fn on_expr(expr: Step(Expression)) -> Step(Statement) {
  fn(bindings) {
    let expr = expr(bindings)

    imp.Expr(expr)
  }
}

fn on_for_in(
  name: String,
  cond: Step(Expression),
  body: List(Step(Statement)),
) -> Step(Statement) {
  fn(bindings) {
    let cond = cond(bindings)
    let bindings = map.delete(bindings, name)
    let body = list.map(body, fn(stmt) { stmt(bindings) })

    imp.ForIn(name, cond, body)
  }
}

fn on_function(
  name: String,
  args: List(String),
  body: List(Step(Statement)),
) -> Step(Statement) {
  fn(bindings) {
    let bindings = list.fold(args, bindings, map.delete)
    let bindings = map.delete(bindings, name)
    let body = fold_statements(body, bindings)

    imp.Function(name, args, body)
  }
}

fn on_if(cond: Step(Expression), then: List(Step(Statement))) -> Step(Statement) {
  fn(bindings) {
    let cond = cond(bindings)
    let then = fold_statements(then, bindings)

    imp.If(cond, then)
  }
}

fn on_if_else(
  cond: Step(Expression),
  then: List(Step(Statement)),
  else: List(Step(Statement)),
) -> Step(Statement) {
  fn(bindings) {
    let cond = cond(bindings)
    let then = fold_statements(then, bindings)
    let else = fold_statements(else, bindings)

    imp.IfElse(cond, then, else)
  }
}

fn on_iife(body: List(Step(Statement))) -> Step(Expression) {
  fn(bindings) {
    let body = fold_statements(body, bindings)

    imp.IIFE(body)
  }
}

fn on_index(expr: Step(Expression), idx: Step(Expression)) -> Step(Expression) {
  fn(bindings) {
    let expr = expr(bindings)
    let idx = idx(bindings)

    imp.Index(expr, idx)
  }
}

fn on_let(name: String, value: Step(Expression)) -> Step(Statement) {
  fn(bindings) {
    let value = value(bindings)

    imp.Let(name, value)
  }
}

fn on_number(num: Float) -> Step(Expression) {
  fn(_) { imp.Number(num) }
}

fn on_object(fields: List(Field(Step(Expression)))) -> Step(Expression) {
  fn(bindings) {
    let fields =
      list.map(fields, fn(field) { Field(field.key, field.value(bindings)) })

    imp.Object(fields)
  }
}

fn on_return(expr: Step(Expression)) -> Step(Statement) {
  fn(bindings) {
    let expr = expr(bindings)

    imp.Return(expr)
  }
}

fn on_spread(expr: Step(Expression)) -> Step(Expression) {
  fn(bindings) {
    let expr = expr(bindings)

    imp.Spread(expr)
  }
}

fn on_string(str: String) -> Step(Expression) {
  fn(_) { imp.String(str) }
}

fn on_ternary(
  cond: Step(Expression),
  then: Step(Expression),
  else: Step(Expression),
) -> Step(Expression) {
  fn(bindings) {
    let cond = cond(bindings)
    let then = then(bindings)
    let else = else(bindings)

    imp.Ternary(cond, then, else)
  }
}

fn on_throw(message: String) -> Step(Statement) {
  fn(_) { imp.Throw(message) }
}

fn on_unop(op: Unop, expr: Step(Expression)) -> Step(Expression) {
  fn(bindings) {
    let expr = expr(bindings)

    imp.Unop(op, expr)
  }
}

fn on_var(var: String) -> Step(Expression) {
  fn(bindings) { lookup(bindings, var) }
}

fn on_while(
  cond: Step(Expression),
  body: List(Step(Statement)),
) -> Step(Statement) {
  fn(bindings) {
    let cond = cond(bindings)
    let body = fold_statements(body, bindings)

    imp.While(cond, body)
  }
}

// UTILS -----------------------------------------------------------------------

fn fold_statements(
  stmts: List(Step(Statement)),
  bindings: Map(String, Expression),
) -> List(Statement) {
  let #(stmts, _) = {
    use acc, stmt <- list.fold(stmts, #([], bindings))
    let #(stmts, bindings) = acc

    case stmt(bindings) {
      imp.Const(name, value) as stmt -> {
        let bindings = map.insert(bindings, name, value)

        #([stmt, ..stmts], bindings)
      }

      imp.Let(name, value) as stmt -> {
        let bindings = map.insert(bindings, name, value)

        #([stmt, ..stmts], bindings)
      }

      imp.Expr(imp.Assign(name, value)) as stmt -> {
        let bindings = map.insert(bindings, name, value)

        #([stmt, ..stmts], bindings)
      }

      stmt -> #([stmt, ..stmts], bindings)
    }
  }

  list.reverse(stmts)
}

fn lookup(bindings: Map(String, Expression), name: String) -> Expression {
  case map.get(bindings, name) {
    Ok(imp.Var(var)) if var == name -> imp.Var(var)
    Ok(imp.Var(var)) -> lookup(bindings, var)
    Ok(expr) -> expr
    Error(_) -> imp.Var(name)
  }
}
