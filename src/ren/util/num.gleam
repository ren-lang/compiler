// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int

// QUERIES ---------------------------------------------------------------------

///
///
pub fn is_int(num: Float) -> Bool {
  let int = float.round(num)

  int.to_float(int) == num
}
