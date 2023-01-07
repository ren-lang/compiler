// IMPORTS ---------------------------------------------------------------------

import gleam/io

/// Intentionally crash the program with some message. 
///
pub fn crash(message: String) -> a {
  io.println(message)

  assert False = True

  // The above assertion will always fail and crash the program. The Gleam compiler
  // can't know that, though, so we pretend to infinitely recurse the `crash`
  // function so we can satisfy the return type `a`.
  crash(message)
}
