// IMPORTS ---------------------------------------------------------------------

import gleeunit/should
import ren/pretty

// TESTS -----------------------------------------------------------------------

/// The `Strictly Pretty` paper comes with a brief example at the end showing
/// off the algorithm and the expected output at various line widths. The following
/// tests are a faithful reproduction of those examples so go peep the paper if
/// you want to learn more:
///
/// https://lindig.github.io/papers/strictly-pretty-2000.pdf
///
pub fn lindig_32_test() {
  let doc = lindig_doc()
  let expected = "if a == b then a << 2 else a + b\n"

  pretty.print(doc, 32)
  |> should.equal(expected)
}

pub fn lindig_15_test() {
  let doc = lindig_doc()
  let expected = "if a == b\nthen a << 2\nelse a + b\n"

  pretty.print(doc, 15)
  |> should.equal(expected)
}

pub fn lindig_10_test() {
  let doc = lindig_doc()
  let expected = "if a == b\nthen\n  a << 2\nelse a + b\n"

  pretty.print(doc, 10)
  |> should.equal(expected)
}

pub fn lindig_8_test() {
  let doc = lindig_doc()
  let expected = "if\n  a == b\nthen\n  a << 2\nelse\n  a + b\n"

  pretty.print(doc, 8)
  |> should.equal(expected)
}

pub fn lindig_7_test() {
  let doc = lindig_doc()
  let expected = "if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a + b\n"

  pretty.print(doc, 7)
  |> should.equal(expected)
}

pub fn lindig_6_test() {
  let doc = lindig_doc()
  let expected = "if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a +\n    b\n"

  pretty.print(doc, 6)
  |> should.equal(expected)
}

// UTILS -----------------------------------------------------------------------

fn lindig_doc() {
  lindig_if(
    lindig_binop("a", "==", "b"),
    lindig_binop("a", "<<", "2"),
    lindig_binop("a", "+", "b"),
  )
}

fn lindig_binop(l, op, r) {
  pretty.concat_group([pretty.text(l), pretty.text(op)])
  |> pretty.append(pretty.text(r))
  |> pretty.nest(2)
  |> pretty.group
}

fn lindig_if(c, t, f) {
  pretty.concat_group([
    pretty.group(pretty.nest(pretty.concat([pretty.text("if"), c]), 2)),
    pretty.group(pretty.nest(pretty.concat([pretty.text("then"), t]), 2)),
    pretty.group(pretty.nest(pretty.concat([pretty.text("else"), f]), 2)),
  ])
}
