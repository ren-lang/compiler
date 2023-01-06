// IMPORTS ---------------------------------------------------------------------

import gleam/map.{Map}
import gleam/option.{Option}
import gleam/set.{Set}
import ren/t.{Type}
import ren/t/subst.{Subst}

// TYPES -----------------------------------------------------------------------

///
///
pub type Monoenv =
  Map(String, Type)

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn empty() -> Monoenv {
  map.new()
}

///
///
pub fn from(var: String, t: Type) -> Monoenv {
  map.from_list([#(var, t)])
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn free(monoenv: Monoenv) -> Set(String) {
  use vars, _, t <- map.fold(monoenv, set.new())
  set.union(vars, t.free(t))
}

///
///
pub fn lookup(monoenv: Monoenv, var: String) -> Option(Type) {
  map.get(monoenv, var)
  |> option.from_result
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn filter(monoenv: Monoenv, predicate: fn(Type) -> Bool) -> Monoenv {
  use _, t <- map.filter(monoenv)
  predicate(t)
}

///
///
pub fn merge(m1: Monoenv, m2: Monoenv, subst: Subst(Type)) -> Monoenv {
  let ms1 = substitute(m1, subst)
  let ms2 = substitute(m2, subst)
  map.merge(ms1, ms2)
}

///
///
pub fn substitute(monoenv: Monoenv, subst: Subst(Type)) -> Monoenv {
  use _, t <- map.map_values(monoenv)
  t.substitute(t, subst)
}
