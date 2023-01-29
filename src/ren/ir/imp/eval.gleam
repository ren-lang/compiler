// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/result
import gleam/float
import ren/ir/imp.{Binop, Expression, Statement, Unop}
import ren/util/num

// EVAL ------------------------------------------------------------------------

///
///
pub fn run(stmt: Statement) -> Statement {
  imp.fold(
    stmt,
    on_access: on_access,
    on_array: imp.Array,
    on_arrow: imp.Arrow,
    on_assign: imp.Assign,
    on_binop: on_binop,
    on_block: on_block,
    on_break: imp.Break,
    on_call: imp.Call,
    on_comment: imp.Comment,
    on_const: imp.Const,
    on_continue: imp.Continue,
    on_export: imp.Export,
    on_expr: imp.Expr,
    on_false: imp.JSFalse,
    on_for_in: on_for_in,
    on_function: imp.Function,
    on_if: on_if,
    on_if_else: on_if_else,
    on_iife: on_iife,
    on_index: on_index,
    on_let: fn(name, value) { imp.Let(name, value) },
    on_null: imp.Null,
    on_number: imp.Number,
    on_object: imp.Object,
    on_return: imp.Return,
    on_spread: imp.Spread,
    on_string: imp.String,
    on_ternary: on_ternary,
    on_throw: imp.Throw,
    on_true: imp.JSTrue,
    on_undefined: imp.Undefined,
    on_unop: on_unop,
    on_var: imp.Var,
    on_while: on_while,
  )
}

// HANDLERS --------------------------------------------------------------------

fn on_access(expr: Expression, keys: List(String)) -> Expression {
  case expr, keys {
    imp.Object(fields), [key, ..rest] ->
      list.find(fields, fn(field) { field.key == key })
      |> result.map(fn(field) { field.value })
      |> result.map(fn(value) { on_access(value, rest) })
      |> result.unwrap(imp.Access(expr, keys))

    _, _ -> imp.Access(expr, keys)
  }
}

fn on_binop(lhs: Expression, op: Binop, rhs: Expression) -> Expression {
  case lhs, op, rhs {
    imp.Number(x), imp.Add, imp.Number(y) -> imp.Number(x +. y)
    imp.Number(x), imp.Sub, imp.Number(y) -> imp.Number(x -. y)
    imp.Number(x), imp.Div, imp.Number(y) -> imp.Number(x /. y)
    imp.Number(x), imp.Mul, imp.Number(y) -> imp.Number(x *. y)

    imp.Number(x), imp.Lt, imp.Number(y) if x <. y -> imp.JSTrue
    imp.Number(_), imp.Lt, imp.Number(_) -> imp.JSFalse
    imp.Number(x), imp.Lte, imp.Number(y) if x <=. y -> imp.JSTrue
    imp.Number(_), imp.Lte, imp.Number(_) -> imp.JSFalse
    imp.Number(x), imp.Gt, imp.Number(y) if x >. y -> imp.JSTrue
    imp.Number(_), imp.Gt, imp.Number(_) -> imp.JSFalse
    imp.Number(x), imp.Gte, imp.Number(y) if x >=. y -> imp.JSTrue
    imp.Number(_), imp.Gte, imp.Number(_) -> imp.JSFalse

    imp.JSTrue, imp.And, imp.JSTrue -> imp.JSTrue
    imp.JSTrue, imp.And, imp.JSFalse -> imp.JSFalse
    imp.JSFalse, imp.And, imp.JSTrue -> imp.JSFalse
    imp.JSFalse, imp.And, imp.JSFalse -> imp.JSFalse

    imp.JSTrue, imp.Or, imp.JSTrue -> imp.JSTrue
    imp.JSTrue, imp.Or, imp.JSFalse -> imp.JSTrue
    imp.JSFalse, imp.Or, imp.JSTrue -> imp.JSTrue
    imp.JSFalse, imp.Or, imp.JSFalse -> imp.JSFalse

    x, imp.Eq, y ->
      case can_static_eq(x) && can_static_eq(y) {
        True if x == y -> imp.JSTrue
        True -> imp.JSFalse
        False -> imp.Binop(x, op, y)
      }

    x, imp.Neq, y ->
      case can_static_eq(x) && can_static_eq(y) {
        True if x != y -> imp.JSTrue
        True -> imp.JSFalse
        False -> imp.Binop(x, op, y)
      }

    imp.String(k), imp.In, imp.Object(fields) ->
      case list.find(fields, fn(field) { field.key == k }) {
        Ok(_) -> imp.JSTrue
        Error(_) -> imp.JSFalse
      }

    imp.String(x), imp.Add, imp.String(y) -> imp.String(x <> y)

    _, _, _ -> imp.Binop(lhs, op, rhs)
  }
}

fn on_block(stmts: List(Statement)) -> Statement {
  case stmts {
    [imp.Const(_, _)] -> imp.Block([])
    [imp.Let(_, _)] -> imp.Block([])
    [stmt] -> stmt
    _ -> imp.Block(stmts)
  }
}

fn on_for_in(name: String, cond: Expression, body: List(Statement)) -> Statement {
  imp.ForIn(name, cond, body)
}

fn on_if(cond: Expression, then: List(Statement)) -> Statement {
  case cond {
    imp.JSTrue -> imp.Block(then)
    imp.JSFalse -> imp.Block([])
    _ -> imp.If(cond, then)
  }
}

fn on_if_else(
  cond: Expression,
  then: List(Statement),
  else: List(Statement),
) -> Statement {
  case cond {
    imp.JSTrue -> imp.Block(then)
    imp.JSFalse -> imp.Block(else)
    _ -> imp.IfElse(cond, then, else)
  }
}

fn on_iife(body: List(Statement)) -> Expression {
  case body {
    [imp.Return(expr)] -> expr
    _ -> imp.IIFE(body)
  }
}

fn on_index(expr: Expression, idx: Expression) -> Expression {
  case expr, idx {
    imp.Array(elements), imp.Number(num) ->
      case num.is_int(num) {
        True ->
          list.at(elements, float.round(num))
          |> result.unwrap(imp.Undefined)
        False -> imp.Undefined
      }

    imp.Object(fields), imp.String(key) ->
      list.find(fields, fn(field) { field.key == key })
      |> result.map(fn(field) { field.value })
      |> result.unwrap(imp.Undefined)
  }
}

fn on_ternary(
  cond: Expression,
  then: Expression,
  else: Expression,
) -> Expression {
  case cond {
    imp.JSTrue -> then
    imp.JSFalse -> else
    _ -> imp.Ternary(cond, then, else)
  }
}

fn on_unop(op: Unop, expr: Expression) -> Expression {
  case op, expr {
    imp.Neg, imp.Number(num) -> imp.Number(float.negate(num))

    imp.Not, imp.JSTrue -> imp.JSFalse
    imp.Not, imp.JSFalse -> imp.JSTrue

    imp.Typeof, imp.Array(_) -> imp.String("object")
    imp.Typeof, imp.JSFalse -> imp.String("boolean")
    imp.Typeof, imp.JSTrue -> imp.String("boolean")
    imp.Typeof, imp.Null -> imp.String("object")
    imp.Typeof, imp.Number(_) -> imp.String("number")
    imp.Typeof, imp.Object(_) -> imp.String("object")
    imp.Typeof, imp.String(_) -> imp.String("string")
    imp.Typeof, imp.Undefined -> imp.String("undefined")

    _, _ -> imp.Unop(op, expr)
  }
}

fn on_while(cond: Expression, body: List(Statement)) -> Statement {
  case cond {
    imp.JSFalse -> imp.Block([])
    _ -> imp.While(cond, body)
  }
}

// UTILS -----------------------------------------------------------------------

fn can_static_eq(expr: Expression) -> Bool {
  case expr {
    imp.Array(elements) -> list.all(elements, can_static_eq)
    imp.JSFalse -> True
    imp.JSTrue -> True
    imp.Null -> True
    imp.Number(_) -> True
    imp.Object(fields) ->
      list.all(fields, fn(field) { can_static_eq(field.value) })
    imp.String(_) -> True
    imp.Undefined -> True
    _ -> False
  }
}
