// IMPORTS ---------------------------------------------------------------------

import gleam/list

// TYPES -----------------------------------------------------------------------

///
///
pub type Lit(e) {
  Arr(List(e))
  Con(String, List(e))
  Num(Float)
  Obj(List(Field(e)))
  Str(String)
}

///
///
pub type Field(e) {
  Field(key: String, value: e)
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn bool(b: Bool) -> Lit(e) {
  case b {
    True -> Con("true", [])
    False -> Con("false", [])
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn map(lit: Lit(e), f: fn(e) -> a) -> Lit(a) {
  case lit {
    Arr(elements) -> Arr(list.map(elements, f))
    Con(name, elements) -> Con(name, list.map(elements, f))
    Num(n) -> Num(n)
    Obj(fields) ->
      Obj(list.map(
        fields,
        fn(field) { Field(key: field.key, value: f(field.value)) },
      ))
    Str(s) -> Str(s)
  }
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn key_val_pairs(fields: List(Field(e))) -> List(#(String, e)) {
  use field <- list.map(fields)

  #(field.key, field.value)
}
