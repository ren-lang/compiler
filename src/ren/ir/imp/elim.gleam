// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/set.{Set}
import ren/ast/lit.{Field}
import ren/ir/imp.{Binop, Expression, Statement, Unop}

// TYPES -----------------------------------------------------------------------

type Step(a) =
  #(a, Set(String))

///
///
pub fn run(stmt: Statement) -> Statement {
  let #(stmt, _) =
    imp.fold(
      stmt,
      on_access: on_access,
      on_array: on_array,
      on_arrow: on_arrow,
      on_assign: on_assign,
      on_binop: on_binop,
      on_block: on_block,
      on_break: constant(imp.Break),
      on_call: on_call,
      on_comment: simple(imp.Comment),
      on_const: on_const,
      on_continue: constant(imp.Continue),
      on_export: on_export,
      on_expr: on_expr,
      on_false: constant(imp.JSFalse),
      on_for_in: on_for_in,
      on_function: on_function,
      on_if: on_if,
      on_if_else: on_if_else,
      on_iife: on_iife,
      on_index: on_index,
      on_let: on_let,
      on_null: constant(imp.Null),
      on_number: simple(imp.Number),
      on_object: on_object,
      on_return: on_return,
      on_spread: on_spread,
      on_string: simple(imp.String),
      on_ternary: on_ternary,
      on_throw: simple(imp.Throw),
      on_true: constant(imp.JSTrue),
      on_undefined: constant(imp.Undefined),
      on_unop: on_unop,
      on_var: on_var,
      on_while: on_while,
    )

  stmt
}

// HANDLERS --------------------------------------------------------------------

fn on_access(expr: Step(Expression), keys: List(String)) -> Step(Expression) {
  let #(expr, refs) = expr

  #(imp.Access(expr, keys), refs)
}

fn on_array(elements: List(Step(Expression))) -> Step(Expression) {
  let #(elements, refs) = list.unzip(elements)

  #(imp.Array(elements), list.fold(refs, set.new(), set.union))
}

fn on_arrow(args: List(String), body: List(Step(Statement))) -> Step(Expression) {
  let #(body, refs) = fold_statements(body)

  #(imp.Arrow(args, body), refs)
}

fn on_assign(name: String, value: Step(Expression)) -> Step(Expression) {
  let #(value, refs) = value

  #(imp.Assign(name, value), refs)
}

fn on_binop(
  lhs: Step(Expression),
  op: Binop,
  rhs: Step(Expression),
) -> Step(Expression) {
  let #(lhs, lhs_refs) = lhs
  let #(rhs, rhs_refs) = rhs

  #(imp.Binop(lhs, op, rhs), set.union(lhs_refs, rhs_refs))
}

fn on_block(stmts: List(Step(Statement))) -> Step(Statement) {
  let #(stmts, refs) = fold_statements(stmts)

  #(imp.Block(stmts), refs)
}

fn on_call(
  fun: Step(Expression),
  args: List(Step(Expression)),
) -> Step(Expression) {
  let #(fun, fun_refs) = fun
  let #(args, arg_refs) = list.unzip(args)
  let refs = list.fold(arg_refs, fun_refs, set.union)

  #(imp.Call(fun, args), refs)
}

fn on_const(name: String, value: Step(Expression)) -> Step(Statement) {
  let #(value, refs) = value

  #(imp.Const(name, value), refs)
}

fn on_export(stmt: Step(Statement)) -> Step(Statement) {
  let #(stmt, refs) = stmt

  #(imp.Export(stmt), refs)
}

fn on_expr(expr: Step(Expression)) -> Step(Statement) {
  let #(expr, refs) = expr

  #(imp.Expr(expr), refs)
}

fn on_for_in(
  name: String,
  cond: Step(Expression),
  body: List(Step(Statement)),
) -> Step(Statement) {
  let #(cond, cond_refs) = cond
  let #(body, body_refs) = fold_statements(body)

  #(imp.ForIn(name, cond, body), set.union(cond_refs, body_refs))
}

fn on_function(
  name: String,
  args: List(String),
  body: List(Step(Statement)),
) -> Step(Statement) {
  let #(body, refs) = fold_statements(body)

  #(imp.Function(name, args, body), refs)
}

fn on_if(cond: Step(Expression), then: List(Step(Statement))) -> Step(Statement) {
  let #(cond, cond_refs) = cond
  let #(then, then_refs) = fold_statements(then)

  #(imp.If(cond, then), set.union(cond_refs, then_refs))
}

fn on_if_else(
  cond: Step(Expression),
  then: List(Step(Statement)),
  else: List(Step(Statement)),
) -> Step(Statement) {
  let #(cond, cond_refs) = cond
  let #(then, then_refs) = fold_statements(then)
  let #(else, else_refs) = fold_statements(else)
  let refs = list.fold([cond_refs, then_refs, else_refs], set.new(), set.union)

  #(imp.IfElse(cond, then, else), refs)
}

fn on_iife(body: List(Step(Statement))) -> Step(Expression) {
  let #(body, refs) = fold_statements(body)

  #(imp.IIFE(body), refs)
}

fn on_index(expr: Step(Expression), idx: Step(Expression)) -> Step(Expression) {
  let #(expr, expr_refs) = expr
  let #(idx, idx_refs) = idx

  #(imp.Index(expr, idx), set.union(expr_refs, idx_refs))
}

fn on_let(name: String, value: Step(Expression)) -> Step(Statement) {
  let #(value, refs) = value

  #(imp.Let(name, value), refs)
}

fn on_object(fields: List(Field(Step(Expression)))) -> Step(Expression) {
  let #(fields, refs) =
    list.unzip(list.map(
      fields,
      fn(field) { #(Field(field.key, field.value.0), field.value.1) },
    ))
  let refs = list.fold(refs, set.new(), set.union)

  #(imp.Object(fields), refs)
}

fn on_return(expr: Step(Expression)) -> Step(Statement) {
  let #(expr, refs) = expr

  #(imp.Return(expr), refs)
}

fn on_spread(expr: Step(Expression)) -> Step(Expression) {
  let #(expr, refs) = expr

  #(imp.Spread(expr), refs)
}

fn on_ternary(
  cond: Step(Expression),
  then: Step(Expression),
  else: Step(Expression),
) -> Step(Expression) {
  let #(cond, cond_refs) = cond
  let #(then, then_refs) = then
  let #(else, else_refs) = else
  let refs = list.fold([cond_refs, then_refs, else_refs], set.new(), set.union)

  #(imp.Ternary(cond, then, else), refs)
}

fn on_unop(op: Unop, expr: Step(Expression)) -> Step(Expression) {
  let #(expr, refs) = expr

  #(imp.Unop(op, expr), refs)
}

fn on_var(var: String) -> Step(Expression) {
  #(imp.Var(var), set.from_list([var]))
}

fn on_while(
  cond: Step(Expression),
  body: List(Step(Statement)),
) -> Step(Statement) {
  let #(cond, cond_refs) = cond
  let #(body, body_refs) = fold_statements(body)

  #(imp.While(cond, body), set.union(cond_refs, body_refs))
}

// UTILS -----------------------------------------------------------------------

fn constant(a: a) -> Step(a) {
  #(a, set.new())
}

fn simple(f: fn(a) -> b) -> fn(a) -> Step(b) {
  fn(a) { constant(f(a)) }
}

fn fold_statements(stmts: List(Step(Statement))) -> Step(List(Statement)) {
  use acc, stmt <- list.fold_right(stmts, #([], set.new()))
  let #(stmt, stmt_refs) = stmt

  case stmt {
    imp.Const(name, _) ->
      case set.contains(acc.1, name) {
        True -> #([stmt, ..acc.0], set.delete(acc.1, name))
        False -> acc
      }

    imp.Let(name, _) ->
      case set.contains(acc.1, name) {
        True -> #([stmt, ..acc.0], set.delete(acc.1, name))
        False -> acc
      }

    imp.Expr(imp.Assign(name, _)) ->
      case set.contains(acc.1, name) {
        True -> #([stmt, ..acc.0], set.delete(acc.1, name))
        False -> acc
      }

    imp.Return(_) -> #([stmt], stmt_refs)

    _ -> #([stmt, ..acc.0], acc.1)
  }
}
