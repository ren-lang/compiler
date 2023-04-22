// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import gleam/map
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import ren/ast/lit.{Arr, Con, Field, Lit, Num, Obj, Str}
import ren/ast/pat.{Case, Pat}
import ren/ir/core
import ren/util/debug
import ren/util/rec

// TYPES -----------------------------------------------------------------------

///
///
pub type Statement {
  Block(List(Statement))
  Break
  Comment(String)
  Const(String, Expression)
  Continue
  Export(Statement)
  Expr(Expression)
  ForIn(String, Expression, List(Statement))
  Function(String, List(String), List(Statement))
  If(Expression, List(Statement))
  IfElse(Expression, List(Statement), List(Statement))
  Let(String, Expression)
  Return(Expression)
  Throw(String)
  While(Expression, List(Statement))
}

///
///
pub type Expression {
  Access(Expression, List(String))
  Array(List(Expression))
  Arrow(List(String), List(Statement))
  Assign(String, Expression)
  Binop(Expression, Binop, Expression)
  Call(Expression, List(Expression))
  IIFE(List(Statement))
  Index(Expression, Expression)
  JSFalse
  JSTrue
  Null
  Number(Float)
  Object(List(Field(Expression)))
  Spread(Expression)
  String(String)
  Ternary(Expression, Expression, Expression)
  Undefined
  Unop(Unop, Expression)
  Var(String)
}

///
///
pub type Unop {
  Neg
  New
  Not
  Pos
  Typeof
}

///
///
pub type Binop {
  Add
  And
  Comma
  Div
  Eq
  Gt
  Gte
  In
  Instanceof
  Lt
  Lte
  Mod
  Mul
  Neq
  Or
  Pow
  Sub
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn from_core(expr: core.Expr) -> Statement {
  core.fold(
    expr,
    on_app: from_app_expr,
    on_lam: from_lam_expr,
    on_let: from_let_expr,
    on_lit: from_lit_expr,
    on_pat: from_pat_expr,
    on_var: from_var_expr,
  )
}

fn from_app_expr(fun: Statement, args: List(Statement)) -> Statement {
  let fexpr = as_expression(fun)
  let argsexpr = list.map(args, as_expression)

  case fexpr, argsexpr {
    Var("$add"), [lhs, rhs] -> Expr(Binop(lhs, Add, rhs))
    Var("$and"), [lhs, rhs] -> Expr(Binop(lhs, And, rhs))
    Var("$concat"), [lhs, rhs] -> Expr(Binop(lhs, Add, rhs))
    Var("$div"), [lhs, rhs] -> Expr(Binop(lhs, Div, rhs))
    Var("$gt"), [lhs, rhs] -> Expr(Binop(lhs, Gt, rhs))
    Var("$gte"), [lhs, rhs] -> Expr(Binop(lhs, Gte, rhs))
    Var("$lt"), [lhs, rhs] -> Expr(Binop(lhs, Lt, rhs))
    Var("$lte"), [lhs, rhs] -> Expr(Binop(lhs, Lte, rhs))
    Var("$mod"), [lhs, rhs] -> Expr(Binop(lhs, Mod, rhs))
    Var("$mul"), [lhs, rhs] -> Expr(Binop(lhs, Mul, rhs))
    Var("$neq"), [lhs, rhs] -> Expr(Binop(lhs, Neq, rhs))
    Var("$or"), [lhs, rhs] -> Expr(Binop(lhs, Or, rhs))
    Var("$pow"), [lhs, rhs] -> Expr(Binop(lhs, Pow, rhs))
    Var("$sub"), [lhs, rhs] -> Expr(Binop(lhs, Sub, rhs))

    // We have our own built-in custom equality function that checks for
    // structural equality, so we want to make sure we use that instead of the
    // normal JavaScript operator.
    //
    Var("$eq"), [_, _] -> Expr(Call(fexpr, argsexpr))

    // `If` expressions get turned into if/else blocks here. If we ever need the
    // expression form back it is easily recoverable with the `as_expression`
    // utility function.
    Var("$if"), [cond, _, _] -> {
      let assert [_, then, else] = args
      IfElse(cond, statements(then), statements(else))
    }

    // Translating the pipe operator is important, because the naive transformation
    // leads to some really unwieldy code. The simplest approach would be to simply
    // transform pipes into function calls such that `a |> b` becomes `b(a)`.
    //
    // This is problematic for long pipes, because it means that we end up with
    // deeply nested function calls. For example, `a |> b |> c |> d` becomes
    // `d(c(b(a)))`.
    //
    // What we'd like to do, instead, is translate pipelines into a assignment
    // and subsequent mutation of a temporary variable. This means that the
    // above example would be translated into:
    //
    // ```js
    // let $pipe = a
    // $pipe = b($pipe)
    // $pipe = c($pipe)
    // d($pipe)
    // ```
    //
    Var("$pipe"), [lhs, rhs] ->
      case lhs {
        IIFE(body) -> {
          let #(stmts, [Return(last)]) = list.split(body, list.length(body) - 1)
          let assign = Expr(Assign("$pipe", last))
          let call = Expr(merge_args(rhs, [Var("$pipe")]))

          Block(list.append(stmts, [assign, call]))
        }

        _ -> Block([Let("$pipe", lhs), Expr(merge_args(rhs, [Var("$pipe")]))])
      }

    // In ren code, the sequence operator is just an expression like any other
    // that we can use to discard the result of the left expression and return
    // the result of the right expression.
    //
    // This is exactly what statements are for in imperative languages, so we can
    // just smush the two operands together into a single block. This works out
    // quite nicely because we're already building up statements from the operands.
    //
    // The only thing we need to do is to make sure that we don't duplicate any
    // variable declarations. We can do this by keeping track of the variables
    // that have been declared in the current scope and only emitting a new
    // declaration if we haven't already seen it.
    //
    // This should only come up in the case of the pipe operator being used in
    // both operands because ren doesn't permit shadowing in userland code.
    //
    Var("$seq"), [_, _] -> {
      let stmts = list.flat_map(args, statements)
      let #(stmts, _) = {
        use acc, stmt <- list.fold(stmts, #([], map.new()))
        case stmt {
          Const(var, value) | Let(var, value) ->
            map.get(acc.1, var)
            |> result.replace(#([Expr(Assign(var, value)), ..acc.0], acc.1))
            |> result.unwrap(#([stmt, ..acc.0], map.insert(acc.1, var, value)))

          _ -> #([stmt, ..acc.0], acc.1)
        }
      }
      Block(list.reverse(stmts))
    }

    // For all other functions we'll just generate a function call as normal.
    _, _ -> Expr(merge_args(fexpr, argsexpr))
  }
}

fn from_lam_expr(args: List(String), body: Statement) -> Statement {
  let body = return_last(statements(body))

  Expr(Arrow(args, body))
}

fn from_let_expr(name: String, value: Statement, body: Statement) -> Statement {
  let value = as_expression(value)
  let assignment = case value {
    Arrow(args, body) -> Function(name, args, body)
    _ -> Const(name, value)
  }
  let body = statements(body)

  Block([assignment, ..body])
}

fn from_lit_expr(lit: Lit(Statement)) -> Statement {
  case lit {
    Arr(elements) -> Expr(Array(list.map(elements, as_expression)))
    Con("true", []) -> Expr(JSTrue)
    Con("false", []) -> Expr(JSFalse)
    Con("null", []) -> Expr(Null)
    Con("undefined", []) -> Expr(Undefined)
    Con(tag, args) ->
      Expr(Array([String(tag), ..list.map(args, as_expression)]))
    Num(num) -> Expr(Number(num))
    Obj(fields) ->
      Expr(Object(list.map(
        fields,
        fn(field) { Field(field.key, as_expression(field.value)) },
      )))
    Str(str) -> Expr(String(str))
  }
}

fn from_pat_expr(expr: Statement, cases: List(Case(Statement))) -> Statement {
  let temp = Const("$switch", as_expression(expr))
  let cases = list.map(cases, branch_from_case(_, "$switch"))
  let catch = Throw("[MatchError] Non-exhaustive pattern match")

  Block(list.flatten([[temp], cases, [catch]]))
}

fn from_var_expr(var) {
  Expr(Var(var))
}

fn branch_from_case(case_: Case(Statement), var: String) -> Statement {
  let Case(pat, guard, body) = case_
  let checks = checks_from_pat(pat, Var(var))
  let assignments = assignments_from_pat(pat, Var(var))
  let body = case guard {
    Some(guard) -> return(If(as_expression(guard), statements(body)))
    None -> return(body)
  }

  If(checks, list.append(assignments, statements(body)))
}

fn checks_from_pat(pat: Pat, expr: Expression) -> Expression {
  // Patterns like the wildcard `_` are always just `true`. This is necessary
  // to handle top-level patterns that match on anything but once we're
  // doing some sort of compound check we can safely eliminate them.
  let and = fn(lhs, rhs) {
    case lhs, rhs {
      JSTrue, _ -> rhs
      _, JSTrue -> lhs
      _, _ -> Binop(lhs, And, rhs)
    }
  }

  let eq = fn(lhs, rhs) { Binop(lhs, Eq, rhs) }
  let or = fn(lhs, rhs) { Binop(lhs, Or, rhs) }

  let checks_from_obj = fn(fields) {
    // As with the `is_array` check, we'll first want to make sure what we're
    // dealing with is actually an object.
    let is_object = Binop(Unop(Typeof, expr), Eq, String("object"))
    use pats, field <- list.fold(fields, is_object)
    let Field(key, pat) = field
    // For each key, before we bother checking the pattnern we first always
    // want to verify that the key exists in the object.
    let key_in = Binop(String(key), In, expr)
    let pats = and(pats, key_in)
    let expr = Access(expr, [key])
    and(pats, checks_from_pat(pat, expr))
  }

  case pat {
    pat.Alias(_, _) -> {
      let msg = "[TODO] Implement alias patterns."
      debug.crash("expr.gleam", 511, msg)
    }

    pat.Bind(_) -> JSTrue
    pat.Value(lit.Arr(elements)) -> checks_from_arr(elements, expr)
    pat.Value(lit.Con(tag, args)) ->
      checks_from_arr([pat.Value(lit.Str(tag)), ..args], expr)
    pat.Value(lit.Num(n)) -> eq(expr, Number(n))
    pat.Value(lit.Obj(fields)) -> checks_from_obj(fields)
    pat.Value(lit.Str(s)) -> eq(expr, String(s))
    pat.Wildcard -> JSTrue
    // 
    pat.Typeof("Array", pat.Value(lit.Arr(elements))) ->
      checks_from_arr(elements, expr)
    pat.Typeof("Array", _) ->
      Call(Access(Var("globalThis"), ["Array", "isArray"]), [expr])
    // There is a catch-all case below that will perform the `typeof expr == "object"`
    // check below, but that is not sufficient for the `Object` primitive type
    // because that check will also check if the constructor name matches the
    // pattern type name.
    //
    // For `@Object` patterns we want to be able to match *any* JavaScript object,
    // so we *only* need to perform the `typeof` check and any checks from the
    // inner pattern.
    pat.Typeof("Object", pat.Value(lit.Obj(fields))) -> checks_from_obj(fields)
    pat.Typeof("Object", _) -> eq(Unop(Typeof, expr), String("object"))
    // For primitive types, we need to check either the `typeof` or the
    // object constructor name matches the primitive type. This is because when
    // performing `typeof` on primitives constructed using `new`, such as
    // `new Number(1)`, the operation will return `"object"` and *not* `"Number"`.
    pat.Typeof("Boolean" as t, pat)
    | pat.Typeof("Number" as t, pat)
    | pat.Typeof("String" as t, pat) -> {
      let typeof = eq(Unop(Typeof, expr), String(string.lowercase(t)))
      let constructor = eq(Access(expr, ["constructor", "name"]), String(t))
      let is_type = or(typeof, constructor)
      // Now we can check the main pattern against the expression.
      and(is_type, checks_from_pat(pat, expr))
    }
    pat.Typeof(t, pat) -> {
      let typeof = eq(Unop(Typeof, expr), String("object"))
      let constructor = eq(Access(expr, ["constructor", "name"]), String(t))
      // Note how for primitives this check was an *or* check, but for classes
      // and objects it's now an *and* check. 
      let is_type = and(typeof, constructor)
      // Now we can check the main pattern against the expression.
      and(is_type, checks_from_pat(pat, expr))
    }
  }
}

fn checks_from_arr(elements: List(Pat), expr: Expression) -> Expression {
  // Patterns like the wildcard `_` are always just `true`. This is necessary
  // to handle top-level patterns that match on anything but once we're
  // doing some sort of compound check we can safely eliminate them.
  let and = fn(lhs, rhs) {
    case lhs, rhs {
      JSTrue, _ -> rhs
      _, JSTrue -> lhs
      _, _ -> Binop(lhs, And, rhs)
    }
  }

  // We'll want to first check that the expression is actually an array, so
  // we don't have to waste time checking the length or indexing into it if
  // it doesn't make sense to.
  let is_array = Call(Access(Var("globalThis"), ["Array", "isArray"]), [expr])
  // Similarly, we'll want to check that the length of the array is correct
  // so we don't have to worry about out-of-bounds errors.
  let length =
    Binop(
      Access(expr, ["length"]),
      Gte,
      Number(int.to_float(list.length(elements))),
    )
  let init = and(is_array, length)
  use checks, pat, i <- // Now we can fold over the patterns and generate the checks for each
  // element. This ignores constant `true` checks because they don't change
  // the result.
  list.index_fold(elements, init)
  let index = Number(int.to_float(i))
  let expr = Index(expr, index)
  and(checks, checks_from_pat(pat, expr))
}

fn assignments_from_pat(pat: Pat, expr: Expression) -> List(Statement) {
  case pat {
    pat.Alias(_, _) ->
      debug.crash("imp.gleam", 368, "Alias patterns unsupported")
    pat.Bind(name) -> [Const(name, expr)]
    pat.Value(lit.Arr(elements)) -> {
      use assignments, pat, i <- list.index_fold(elements, [])
      let index = Number(int.to_float(i))
      list.append(assignments, assignments_from_pat(pat, Index(expr, index)))
    }
    pat.Value(lit.Con(_, args)) -> {
      let arr = pat.Value(lit.Arr([pat.Wildcard, ..args]))
      assignments_from_pat(arr, expr)
    }
    pat.Value(lit.Num(_)) -> []
    pat.Value(lit.Obj(fields)) -> {
      use assignments, field <- list.fold(fields, [])
      let Field(key, pat) = field
      let expr = Access(expr, [key])
      list.append(assignments, assignments_from_pat(pat, expr))
    }
    pat.Value(lit.Str(_)) -> []
    pat.Wildcard -> []
    pat.Typeof(_, pat) -> assignments_from_pat(pat, expr)
  }
}

// QUERIES ---------------------------------------------------------------------

/// Used for determining when to wrap subexpressions in parentheses. Uses precedence
/// numbers from:
///  <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence>
///
pub fn precedence_of(expr: Expression) -> Int {
  case expr {
    Access(_, _) -> 17
    Index(_, _) -> 17
    Unop(New, _) -> 17
    Call(_, _) -> 17
    Unop(Pos, _) -> 14
    Unop(Neg, _) -> 14
    Unop(Typeof, _) -> 14
    Binop(_, Mul, _) -> 12
    Binop(_, Div, _) -> 12
    Binop(_, Mod, _) -> 12
    Binop(_, Add, _) -> 11
    Binop(_, Sub, _) -> 11
    Binop(_, Lt, _) -> 9
    Binop(_, Lte, _) -> 9
    Binop(_, Gt, _) -> 9
    Binop(_, Gte, _) -> 9
    Binop(_, In, _) -> 9
    Binop(_, Instanceof, _) -> 9
    Binop(_, Eq, _) -> 8
    Binop(_, Neq, _) -> 8
    Binop(_, And, _) -> 4
    Binop(_, Or, _) -> 3
    Binop(_, Comma, _) -> 1

    // The way we check if a current expression should be wrapped in parentheses,
    // we look at the expression's precedence and compare it to the current
    // precedence level. If it is lower, it get's wrapped.
    //
    // Besides operators, other kinds of expression don't really need to deal
    // with this so we say they have a massive precedence and never wrap.
    _ -> 999
  }
}

///
///
pub fn statements(stmt: Statement) -> List(Statement) {
  case stmt {
    Block(stmts) -> stmts
    _ -> [stmt]
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
fn merge_args(fun: Expression, args: List(Expression)) -> Expression {
  case fun {
    Call(f, first_args) -> Call(f, list.append(first_args, args))
    _ -> Call(fun, args)
  }
}

///
///
pub fn flatten(stmt: Statement) -> Statement {
  case stmt {
    Block(stmts) -> Block(list.flat_map(stmts, do_flatten))
    _ -> stmt
  }
}

fn do_flatten(stmt: Statement) -> List(Statement) {
  case stmt {
    Block(stmts) -> list.flat_map(stmts, do_flatten)
    _ -> [stmt]
  }
}

///
///
pub fn return(stmt: Statement) -> Statement {
  case stmt {
    Block([]) -> Return(Undefined)
    Block([stmt]) -> return(stmt)
    Block(stmts) -> Block(return_last(stmts))
    Break -> stmt
    Comment(_) -> stmt
    Const(_, expr) -> Return(expr)
    Continue -> stmt
    Export(stmt) -> return(stmt)
    Expr(expr) -> Return(expr)
    ForIn(_, _, _) -> Block([stmt, Return(Undefined)])
    Function(_, args, body) -> Return(Arrow(args, body))
    If(cond, then) -> If(cond, return_last(then))
    IfElse(cond, [Expr(then)], [Expr(else)]) ->
      Return(Ternary(cond, then, else))
    IfElse(cond, [Return(then)], [Return(else)]) ->
      Return(Ternary(cond, then, else))
    IfElse(cond, then, else) ->
      IfElse(cond, return_last(then), return_last(else))
    Let(_, expr) -> Return(expr)
    Return(_) -> stmt
    Throw(_) -> stmt
    While(_, _) -> Block([stmt, Return(Undefined)])
  }
}

fn return_last(stmts: List(Statement)) -> List(Statement) {
  case list.reverse(stmts) {
    [last, ..rest] -> list.reverse([return(last), ..rest])
    [] -> []
  }
}

///
///
pub fn transform(stmt: Statement, f: fn(Statement) -> Statement) -> Statement {
  fold(
    stmt,
    on_access: Access,
    on_array: Array,
    on_arrow: Arrow,
    on_assign: Assign,
    on_binop: Binop,
    on_block: fn(stmts) { f(Block(stmts)) },
    on_break: f(Break),
    on_call: Call,
    on_comment: fn(text) { f(Comment(text)) },
    on_const: fn(name, value) { f(Const(name, value)) },
    on_continue: f(Continue),
    on_export: fn(stmt) { f(Export(stmt)) },
    on_expr: fn(expr) { f(Expr(expr)) },
    on_false: JSFalse,
    on_for_in: fn(name, expr, stmts) { f(ForIn(name, expr, stmts)) },
    on_function: fn(name, args, stmts) { f(Function(name, args, stmts)) },
    on_if: fn(cond, then) { f(If(cond, then)) },
    on_if_else: fn(cond, then, else) { f(IfElse(cond, then, else)) },
    on_iife: IIFE,
    on_index: Index,
    on_let: fn(name, value) { f(Let(name, value)) },
    on_null: Null,
    on_number: Number,
    on_object: Object,
    on_return: Return,
    on_spread: Spread,
    on_string: String,
    on_ternary: Ternary,
    on_throw: fn(message) { f(Throw(message)) },
    on_true: JSTrue,
    on_undefined: Undefined,
    on_unop: Unop,
    on_var: Var,
    on_while: fn(cond, stmts) { f(While(cond, stmts)) },
  )
}

///
///
pub fn transform_many(
  expr: Statement,
  steps: List(fn(Statement) -> Statement),
) -> Statement {
  transform(expr, do_transform_many(_, steps))
}

fn do_transform_many(
  expr: Statement,
  steps: List(fn(Statement) -> Statement),
) -> Statement {
  case list.fold(steps, expr, fn(expr, step) { step(expr) }) {
    next if next == expr -> expr
    next -> transform_many(next, steps)
  }
}

///
///
pub fn fold(
  over stmt: Statement,
  on_access do_access: fn(b, List(String)) -> b,
  on_array do_array: fn(List(b)) -> b,
  on_arrow do_arrow: fn(List(String), List(a)) -> b,
  on_assign do_assign: fn(String, b) -> b,
  on_binop do_binop: fn(b, Binop, b) -> b,
  on_block do_block: fn(List(a)) -> a,
  on_break do_break: a,
  on_call do_call: fn(b, List(b)) -> b,
  on_comment do_comment: fn(String) -> a,
  on_const do_const: fn(String, b) -> a,
  on_continue do_continue: a,
  on_export do_export: fn(a) -> a,
  on_expr do_expr: fn(b) -> a,
  on_false do_false: b,
  on_for_in do_for_in: fn(String, b, List(a)) -> a,
  on_function do_function: fn(String, List(String), List(a)) -> a,
  on_if do_if: fn(b, List(a)) -> a,
  on_if_else do_if_else: fn(b, List(a), List(a)) -> a,
  on_iife do_iife: fn(List(a)) -> b,
  on_index do_index: fn(b, b) -> b,
  on_let do_let: fn(String, b) -> a,
  on_null do_null: b,
  on_number do_number: fn(Float) -> b,
  on_object do_object: fn(List(Field(b))) -> b,
  on_return do_return: fn(b) -> a,
  on_spread do_spread: fn(b) -> b,
  on_string do_string: fn(String) -> b,
  on_ternary do_ternary: fn(b, b, b) -> b,
  on_throw do_throw: fn(String) -> a,
  on_true do_true: b,
  on_undefined do_undefined: b,
  on_unop do_unop: fn(Unop, b) -> b,
  on_var do_var: fn(String) -> b,
  on_while do_while: fn(b, List(a)) -> a,
) -> a {
  let fold_expr = fold_expr(
    _,
    do_access,
    do_array,
    do_arrow,
    do_assign,
    do_binop,
    do_block,
    do_break,
    do_call,
    do_comment,
    do_const,
    do_continue,
    do_export,
    do_expr,
    do_false,
    do_for_in,
    do_function,
    do_if,
    do_if_else,
    do_iife,
    do_index,
    do_let,
    do_null,
    do_number,
    do_object,
    do_return,
    do_spread,
    do_string,
    do_ternary,
    do_throw,
    do_true,
    do_undefined,
    do_unop,
    do_var,
    do_while,
  )

  use stmt <- rec.run(stmt)

  case stmt {
    Block(stmts) -> {
      use x <- rec.list(stmts)
      rec.base(do_block(x))
    }

    Break -> rec.base(do_break)

    Comment(comment) -> {
      let x = do_comment(comment)
      rec.base(x)
    }

    Const(name, expr) -> {
      let x = fold_expr(expr)
      rec.base(do_const(name, x))
    }

    Continue -> rec.base(do_continue)

    Export(stmt) -> {
      use x <- rec.step(stmt)
      rec.base(do_export(x))
    }

    Expr(expr) -> {
      let x = fold_expr(expr)
      rec.base(do_expr(x))
    }

    ForIn(name, expr, body) -> {
      let x = fold_expr(expr)
      use y <- rec.list(body)
      rec.base(do_for_in(name, x, y))
    }

    Function(name, args, body) -> {
      use x <- rec.list(body)
      rec.base(do_function(name, args, x))
    }

    If(cond, body) -> {
      let x = fold_expr(cond)
      use y <- rec.list(body)
      rec.base(do_if(x, y))
    }

    IfElse(cond, then, else) -> {
      let x = fold_expr(cond)
      use y <- rec.list(then)
      use z <- rec.list(else)
      rec.base(do_if_else(x, y, z))
    }

    Let(name, expr) -> {
      let x = fold_expr(expr)
      rec.base(do_let(name, x))
    }

    Return(expr) -> {
      let x = fold_expr(expr)
      rec.base(do_return(x))
    }

    Throw(msg) -> {
      let x = do_throw(msg)
      rec.base(x)
    }

    While(cond, body) -> {
      let x = fold_expr(cond)
      use y <- rec.list(body)
      rec.base(do_while(x, y))
    }
  }
}

pub fn fold_expr(
  over expr: Expression,
  on_access do_access: fn(a, List(String)) -> a,
  on_array do_array: fn(List(a)) -> a,
  on_arrow do_arrow: fn(List(String), List(b)) -> a,
  on_assign do_assign: fn(String, a) -> a,
  on_binop do_binop: fn(a, Binop, a) -> a,
  on_block do_block: fn(List(b)) -> b,
  on_break do_break: b,
  on_call do_call: fn(a, List(a)) -> a,
  on_comment do_comment: fn(String) -> b,
  on_const do_const: fn(String, a) -> b,
  on_continue do_continue: b,
  on_export do_export: fn(b) -> b,
  on_expr do_expr: fn(a) -> b,
  on_false do_false: a,
  on_for_in do_for_in: fn(String, a, List(b)) -> b,
  on_function do_function: fn(String, List(String), List(b)) -> b,
  on_if do_if: fn(a, List(b)) -> b,
  on_if_else do_if_else: fn(a, List(b), List(b)) -> b,
  on_iife do_iife: fn(List(b)) -> a,
  on_index do_index: fn(a, a) -> a,
  on_let do_let: fn(String, a) -> b,
  on_null do_null: a,
  on_number do_number: fn(Float) -> a,
  on_object do_object: fn(List(Field(a))) -> a,
  on_return do_return: fn(a) -> b,
  on_spread do_spread: fn(a) -> a,
  on_string do_string: fn(String) -> a,
  on_ternary do_ternary: fn(a, a, a) -> a,
  on_throw do_throw: fn(String) -> b,
  on_true do_true: a,
  on_undefined do_undefined: a,
  on_unop do_unop: fn(Unop, a) -> a,
  on_var do_var: fn(String) -> a,
  on_while do_while: fn(a, List(b)) -> b,
) -> a {
  let fold_stmt = fold(
    _,
    do_access,
    do_array,
    do_arrow,
    do_assign,
    do_binop,
    do_block,
    do_break,
    do_call,
    do_comment,
    do_const,
    do_continue,
    do_export,
    do_expr,
    do_false,
    do_for_in,
    do_function,
    do_if,
    do_if_else,
    do_iife,
    do_index,
    do_let,
    do_null,
    do_number,
    do_object,
    do_return,
    do_spread,
    do_string,
    do_ternary,
    do_throw,
    do_true,
    do_undefined,
    do_unop,
    do_var,
    do_while,
  )

  use expr <- rec.run(expr)

  case expr {
    Access(expr, keys) -> {
      use x <- rec.step(expr)
      rec.base(do_access(x, keys))
    }

    Array(exprs) -> {
      use x <- rec.list(exprs)
      rec.base(do_array(x))
    }

    Arrow(args, body) -> {
      let x = list.map(body, fold_stmt)
      rec.base(do_arrow(args, x))
    }

    Assign(left, right) -> {
      use y <- rec.step(right)
      rec.base(do_assign(left, y))
    }

    Binop(left, op, right) -> {
      use x <- rec.step(left)
      use y <- rec.step(right)
      rec.base(do_binop(x, op, y))
    }

    Call(expr, args) -> {
      use x <- rec.step(expr)
      use y <- rec.list(args)
      rec.base(do_call(x, y))
    }

    IIFE(stmts) -> {
      let x = list.map(stmts, fold_stmt)
      rec.base(do_iife(x))
    }

    Index(left, right) -> {
      use x <- rec.step(left)
      use y <- rec.step(right)
      rec.base(do_index(x, y))
    }

    JSFalse -> rec.base(do_false)

    JSTrue -> rec.base(do_true)

    Null -> rec.base(do_null)

    Number(num) -> {
      let x = do_number(num)
      rec.base(x)
    }

    Object(fields) -> {
      let do_field = fn(field: Field(r)) {
        use x <- rec.step(field.value)
        rec.base(Field(field.key, x))
      }
      use x <- rec.traverse(fields, do_field)
      rec.base(do_object(x))
    }

    Spread(expr) -> {
      use x <- rec.step(expr)
      rec.base(do_spread(x))
    }

    String(str) -> {
      let x = do_string(str)
      rec.base(x)
    }

    Ternary(cond, then, else) -> {
      use x <- rec.step(cond)
      use y <- rec.step(then)
      use z <- rec.step(else)
      rec.base(do_ternary(x, y, z))
    }

    Undefined -> rec.base(do_undefined)

    Unop(op, expr) -> {
      use x <- rec.step(expr)
      rec.base(do_unop(op, x))
    }

    Var(name) -> {
      let x = do_var(name)
      rec.base(x)
    }
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn as_expression(stmt: Statement) -> Expression {
  case stmt {
    Block(stmts) -> IIFE(return_last(stmts))
    Const(_, expr) -> expr
    Expr(expr) -> expr
    Function(_, args, body) -> Arrow(args, body)
    If(cond, [Expr(then)]) -> Ternary(cond, then, Undefined)
    IfElse(cond, [Expr(then)], [Expr(else)]) -> Ternary(cond, then, else)
    Return(expr) -> expr
    _ -> IIFE(return_last(do_flatten(stmt)))
  }
}
