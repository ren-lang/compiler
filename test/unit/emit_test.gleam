// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/string
import gleeunit/should
import ren/ast/mod.{Mod}
import ren/ir/imp.{Statement}
import ren/query/emit
import ren/t

// TESTS: PRELUDE --------------------------------------------------------------

pub fn prelude_test() {
  use result <- should("emit the prelude", [])
  let expect =
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"
",
    )

  should.equal(result, expect)
}

// TESTS: IMPORTS --------------------------------------------------------------

pub fn local_import_test() {
  let input = mod.Imp(mod.Project, "./wibble/wobble", ["Wibble", "Wobble"])
  use result <- should("emit a local import", [input])

  should.equal(
    result,
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"

import * as Wibble$Wobble from \"./wibble/wobble.ren.js\"
",
    ),
  )
}

pub fn package_import_test() {
  let input = mod.Imp(mod.Package, "ren/std/arr", ["Arr"])
  use result <- should("emit a package import", [input])
  let expect =
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"

import * as Arr from \".ren/pkgs/ren/std/arr.ren.js\"
",
    )

  should.equal(result, expect)
}

pub fn external_import_test() {
  let input = mod.Imp(mod.External, "react", ["React"])
  use result <- should("emit an external import", [input])
  let expect =
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"

import * as React from \"react\"
",
    )

  should.equal(result, expect)
}

pub fn multiple_imports_test() {
  let input = [
    mod.Imp(mod.Project, "./wibble/wobble", ["Wibble", "Wobble"]),
    mod.Imp(mod.Package, "ren/std/arr", ["Arr"]),
    mod.Imp(mod.External, "react", ["React"]),
  ]
  use result <- should("emit multiple imports", input)
  let expect =
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"

import * as Wibble$Wobble from \"./wibble/wobble.ren.js\"
import * as Arr from \".ren/pkgs/ren/std/arr.ren.js\"
import * as React from \"react\"
",
    )

  should.equal(result, expect)
}

// TESTS: LET DECLARATIONS -----------------------------------------------------

pub fn simple_private_let_declaration_test() {
  let input = mod.Let(False, "x", t.Concrete("*"), imp.Expr(imp.Number(1.0)))
  use result <- should("emit a simple private let declaration", [input])
  let expect =
    string.trim(
      "
import { $eq, $fn } from \".ren/pkgs/prelude.js\"

const x = 1
",
    )

  should.equal(result, expect)
}

// UTILS -----------------------------------------------------------------------

fn should(d: String, m: Mod(Statement), k: fn(String) -> a) {
  io.println("should " <> d)

  emit.mod(m, ".ren/pkgs", "test.ren")
  |> k
}
