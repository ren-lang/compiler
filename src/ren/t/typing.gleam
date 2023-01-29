// IMPORTS ---------------------------------------------------------------------

import gleam/set.{Set}
import ren/t.{Type}
import ren/t/subst.{Subst}
import ren/t/monoenv.{Monoenv}

// TYPES -----------------------------------------------------------------------

///
///
pub type Typing =
  #(Monoenv, Type)

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn from(monoenv: Monoenv, t: Type) -> Typing {
  #(monoenv, t)
}

///
///
pub fn mono(var: String, t: Type) -> Typing {
  from(monoenv.from(var, t), t)
}

///
///
pub fn poly(t: Type) -> Typing {
  from(monoenv.empty(), t)
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn free(typing: Typing) -> Set(String) {
  let m = monoenv.free(typing.0)
  let t = t.free(typing.1)
  set.union(m, t)
}

///
///
pub fn env(typing: Typing) -> Monoenv {
  typing.0
}

///
///
pub fn t(typing: Typing) -> Type {
  typing.1
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn substitute(typing: Typing, subst: Subst(Type)) -> Typing {
  let monoenv = monoenv.substitute(typing.0, subst)
  let t = t.substitute(typing.1, subst)
  from(monoenv, t)
}

///
///
pub fn map_monoenv(typing: Typing, f: fn(Monoenv) -> Monoenv) -> Typing {
  from(f(typing.0), typing.1)
}

///
///
pub fn map_t(typing: Typing, f: fn(Type) -> Type) -> Typing {
  from(typing.0, f(typing.1))
}
