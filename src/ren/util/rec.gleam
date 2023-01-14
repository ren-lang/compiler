// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Rec(a, r, t) {
  Rec(r, fn(t) -> Rec(a, r, t))
  Base(a)
}

// CONSTANTS -------------------------------------------------------------------
// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn base(a: a) -> Rec(a, r, t) {
  Base(a)
}

///
///
pub fn rec(r: r) -> Rec(a, r, a) {
  Rec(r, Base)
}

// QUERIES ---------------------------------------------------------------------
// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn map(step: Rec(a, r, t), f: fn(a) -> b) -> Rec(b, r, t) {
  case step {
    Base(a) -> Base(f(a))
    Rec(r, cont) -> Rec(r, fn(a) { map(cont(a), f) })
  }
}

///
///
pub fn do(step: Rec(a, r, t), f: fn(a) -> Rec(b, r, t)) -> Rec(b, r, t) {
  case step {
    Base(a) -> f(a)
    Rec(r, cont) -> Rec(r, fn(a) { do(cont(a), f) })
  }
}

///
///
pub fn step(r: r, cont: fn(t) -> Rec(a, r, t)) -> Rec(a, r, t) {
  Rec(r, cont)
}

///
///
pub fn list(items: List(r), next) -> Rec(b, r, t) {
  use xs <- fold([], items, list.prepend)
  next(list.reverse(xs))
}

///
///
pub fn fold(
  acc: a,
  items: List(r),
  f: fn(a, t) -> a,
  next: fn(a) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  case items {
    [] -> next(acc)
    [item, ..rest] -> step(item, fn(t) { fold(f(acc, t), rest, f, next) })
  }
}

///
///
pub fn traverse(
  items: List(x),
  proj: fn(x) -> Rec(a, r, t),
  next: fn(List(a)) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  do_traverse([], items, proj, next)
}

fn do_traverse(
  acc: List(a),
  stack: List(x),
  proj: fn(x) -> Rec(a, r, t),
  next: fn(List(a)) -> Rec(b, r, t),
) -> Rec(b, r, t) {
  case stack {
    [] -> next(list.reverse(acc))
    [item, ..rest] ->
      do(proj(item), fn(a) { do_traverse([a, ..acc], rest, proj, next) })
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn run(init: r, proj: fn(r) -> Rec(a, r, a)) -> a {
  do_run(proj(init), [], proj)
}

fn do_run(
  step: Rec(a, r, a),
  stack: List(fn(a) -> Rec(a, r, a)),
  proj: fn(r) -> Rec(a, r, a),
) -> a {
  case step, stack {
    Base(a), [] -> a
    Base(a), [next, ..rest] -> do_run(next(a), rest, proj)
    Rec(r, cont), _ -> do_run(proj(r), [cont, ..stack], proj)
  }
}
// UTILS -----------------------------------------------------------------------
