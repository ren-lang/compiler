// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/io
import gleam/list
import gleam/string

// UTILS -----------------------------------------------------------------------

/// Intentionally crash the program with some message along with information
/// about where the crash was called from.
///
pub fn crash(module: String, line: Int, message: String) -> a {
  io.println("[" <> module <> "]:" <> int.to_string(line) <> " FATAL ERROR")
  io.println(fit_to_width(message, 80))
  io.println("")
  io.println("This is probably a bug. Please report it by opening an issue at:")
  io.println("https://github.com/ren-lang/compiler/issues/new")

  assert False = True

  // The above assertion will always fail and crash the program. The Gleam compiler
  // can't know that, though, so we pretend to infinitely recurse the `crash`
  // function so we can satisfy the return type `a`.
  crash(module, line, message)
}

fn fit_to_width(message: String, max_width: Int) -> String {
  let #(_, lines) = {
    use acc, word <- list.fold(string.split(message, " "), #(0, []))
    let #(w, lines) = acc
    let l = string.length(word)
    let w = w + l

    case lines {
      [] -> #(w, [word])
      [line, ..lines] if w < max_width -> #(w, [line <> " " <> word, ..lines])
      lines -> #(l, [word, ..lines])
    }
  }

  lines
  |> list.reverse
  |> string.join("\n")
}

/// Like Gleam's `io.debug` function but with a custom message and information
/// about where the log was called from.
///
pub fn log(value: a, module: String, line: Int, message: String) -> a {
  io.println("[" <> module <> "]:" <> int.to_string(line) <> " " <> message)
  io.debug(value)
}
