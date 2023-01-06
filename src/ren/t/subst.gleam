// IMPORTS ---------------------------------------------------------------------

import gleam/map.{Map}
import gleam/option.{Option}

// TYPES -----------------------------------------------------------------------

///
///
pub type Subst(t) =
  Map(String, t)

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn empty() -> Subst(t) {
  map.new()
}

///
///
pub fn from(var: String, t: t) -> Subst(t) {
  map.from_list([#(var, t)])
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn contains(subst: Subst(t), var: String) -> Bool {
  map.has_key(subst, var)
}

///
///
pub fn lookup(subst: Subst(t), var: String) -> Option(t) {
  map.get(subst, var)
  |> option.from_result
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn extend(subst: Subst(t), var: String, t: t) -> Subst(t) {
  map.insert(subst, var, t)
}

///
///
pub fn compose(
  apply: fn(Subst(t), t) -> t,
  s1: Subst(t),
  s2: Subst(t),
) -> Subst(t) {
  map.map_values(s1, fn(_, t) { apply(s2, t) })
  |> map.merge(s2)
}
