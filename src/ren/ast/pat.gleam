// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{Option}
import gleam/set.{Set}
import ren/ast/lit.{Arr, Con, Lit, Num, Obj, Str}

// TYPES -----------------------------------------------------------------------

///
///
pub type Pat {
  Alias(Pat, String)
  Bind(String)
  Value(Lit(Pat))
  Wildcard
  Typeof(String, Pat)
}

///
///
pub type Case(e) {
  Case(pat: Pat, guard: Option(e), body: e)
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn bindings(pat: Pat) -> Set(String) {
  case pat {
    Alias(pat, _) -> bindings(pat)
    Bind(name) -> set.from_list([name])
    Value(Arr(pats)) -> {
      use set, pat <- list.fold(pats, set.new())
      set.union(bindings(pat), set)
    }
    Value(Con(_, pats)) -> {
      use set, pat <- list.fold(pats, set.new())
      set.union(bindings(pat), set)
    }
    Value(Num(_)) -> set.new()
    Value(Obj(pairs)) -> {
      use set, pat <- list.fold(pairs, set.new())
      set.union(bindings(pat.value), set)
    }
    Value(Str(_)) -> set.new()
    Wildcard -> set.new()
    Typeof(_, pat) -> bindings(pat)
  }
}

///
///
pub fn is_simple(pat: Pat) -> Bool {
  case pat {
    Alias(_, _) -> False
    Bind(_) -> True
    Value(_) -> False
    Wildcard -> True
    Typeof(_, _) -> False
  }
}
