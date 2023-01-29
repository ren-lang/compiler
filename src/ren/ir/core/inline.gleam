// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/map.{Map}
import gleam/option
import gleam/set
import ren/ast/lit.{Arr, Con, Lit, Num, Obj, Str}
import ren/ast/pat.{Case}
import ren/ir/core.{Expr}

// TYPES -----------------------------------------------------------------------

/// 
///
type Step =
  fn(Map(String, Expr)) -> Expr

// INLINE ----------------------------------------------------------------------

///
///
pub fn run(expr: Expr) -> Expr {
  let state = map.new()
  let run = core.fold(expr, on_app, on_lam, on_let, on_lit, on_pat, on_var)

  run(state)
}

// HANDLERS --------------------------------------------------------------------

///
///
pub fn on_app(fun: Step, args: List(Step)) -> Step {
  fn(bindings) {
    let fun = fun(bindings)
    let args = list.map(args, fn(arg) { arg(bindings) })

    core.App(fun, args)
  }
}

///
///
pub fn on_lam(args: List(String), body: Step) -> Step {
  fn(bindings) {
    let bindings = list.fold(args, bindings, map.delete)
    let body = body(bindings)

    core.Lam(args, body)
  }
}

///
///
pub fn on_let(name: String, value: Step, body: Step) -> Step {
  fn(bindings) {
    let value = value(map.delete(bindings, name))
    let bindings = map.insert(bindings, name, value)
    let body = body(bindings)

    core.Let(name, value, body)
  }
}

///
///
pub fn on_lit(lit: Lit(Step)) -> Step {
  fn(bindings) {
    let lit = lit.map(lit, fn(expr) { expr(bindings) })

    core.Lit(lit)
  }
}

///
///
pub fn on_pat(expr: Step, cases: List(Case(Step))) -> Step {
  fn(bindings) {
    let expr = expr(bindings)
    let cases = {
      use case_ <- list.map(cases)
      let Case(pat, guard, body) = case_
      let guard = option.map(guard, fn(guard) { guard(bindings) })
      let bindings = set.fold(pat.bindings(pat), bindings, map.delete)
      let body = body(bindings)

      Case(pat, guard, body)
    }

    core.Pat(expr, cases)
  }
}

///
///
pub fn on_var(name: String) -> Step {
  fn(bindings) { lookup(bindings, name) }
}

// UTILS -----------------------------------------------------------------------

fn lookup(bindings: Map(String, Expr), name: String) -> Expr {
  case map.get(bindings, name) {
    // If we've somehow ended up in a cycle then we need to just bail out and 
    // return the variable. I'm not entirely sure we can ever end up in this case
    // but I think it's better to just return the variable than to crash in this
    // instance.
    Ok(core.Var(var)) if var == name -> core.Var(var)

    // Variables can, of course, reference other variables so we want to make
    // sure we're following the chain until we get to a value we can inline or
    // something else.
    Ok(core.Var(var)) -> lookup(bindings, var)

    // We only want to inline simple literals, otherwise we risk duplicating
    // expensive work.
    //
    // In the future we could use a heuristic to calculate the potential cost
    // of an expression and only inline if it's cheap, but that sounds complicated
    // and advanced and really any amount of optimisation is good!
    Ok(core.Lit(Arr([])) as expr) -> expr
    Ok(core.Lit(Con(_, [])) as expr) -> expr
    Ok(core.Lit(Num(_)) as expr) -> expr
    Ok(core.Lit(Obj([])) as expr) -> expr
    Ok(core.Lit(Str(_)) as expr) -> expr

    _ -> core.Var(name)
  }
}
