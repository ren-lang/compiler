// IMPORTS ---------------------------------------------------------------------

import gleam/list
import ren/t.{Type}

// TYPES -----------------------------------------------------------------------

/// A Ren module is just a collection of declarations. A declaration might be one
/// of the following:
///
/// - An `Imp` declaration is an import. Note that there's no "exposing" or
///   "unqualified" field: imports are always fully qualified.
///
/// - A `Let` declaration is a variable binding. Nothing out of the ordinary here.
///
/// - An `Ext` declaration is a way to pull in FFI code from a companion FFI
///   module. The `name` field is the name of the symbol to import from the FFI
///   module.
///
///   If we have a Ren module `foo.ren`, then any `Ext` declarations will be
///   importing symbols defined in `foo.ffi.js`.
///
/// - A `Typ` declaration is how we define new types in Ren. For the most part,
///   Ren's type system is structural and so most types defined are aliases for
///   some record or enum.
///
///   One could define an abstract type by leaving out the type's definition
///   entirely. These are particularly useful for FFI code whose implementation
///   cannot or should not be accessible from Ren.
///
pub type Dec(expr) {
  Imp(source: Src, path: String, alias: List(String))
  Let(exposed: Bool, var: String, typ: Type, expr: expr)
  Ext(exposed: Bool, var: String, typ: Type, name: String)
  Typ(exposed: Bool, name: String, vars: List(String), typ: Type)
}

/// For now, a module is just a collection of declarations. 
/// 
/// ❓ Gleam expands type aliases as soon as the compiler comes across them, so
/// we need to define it *after* we define the `Dec` type.
///
pub type Mod(expr) =
  List(Dec(expr))

/// Ren has three types of import:
///
/// - `External` imports are for pulling in external JavaScript (either local
///   files or npm modules) or maybe some other resource like CSS. Ren doesn't
///   do anything special with these imports: they're translated directly to ES6
///   import statements.
///
/// - `Package` imports are for pulling in other Ren packages. The path for these
///   imports should always be `"author/package/path/to/module"`. Ren will resolve
///   these during compilation.
///
/// - `Project` imports are for pulling in other modules within the same project.
///   The path for these imports should always be relative to the current module.
///
pub type Src {
  External
  Package
  Project
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn imports(mod: Mod(expr)) -> List(Dec(expr)) {
  use dec <- list.filter(mod)

  case dec {
    Imp(_, _, _) -> True
    _ -> False
  }
}

///
///
pub fn bindings(mod: Mod(expr)) -> List(Dec(expr)) {
  use dec <- list.filter(mod)

  case dec {
    Let(_, _, _, _) -> True
    _ -> False
  }
}

///
///
pub fn externals(mod: Mod(expr)) -> List(Dec(expr)) {
  use dec <- list.filter(mod)

  case dec {
    Ext(_, _, _, _) -> True
    _ -> False
  }
}

///
///
pub fn types(mod: Mod(expr)) -> List(Dec(expr)) {
  use dec <- list.filter(mod)

  case dec {
    Typ(_, _, _, _) -> True
    _ -> False
  }
}

///
///
pub fn emittables(
  mod: Mod(expr),
) -> #(List(Dec(expr)), List(Dec(expr)), List(Dec(expr))) {
  use emittables, dec <- list.fold_right(mod, #([], [], []))
  let #(imports, externals, bindings) = emittables

  case dec {
    Imp(_, _, _) -> #([dec, ..imports], externals, bindings)
    Let(_, _, _, _) -> #(imports, externals, [dec, ..bindings])
    Ext(_, _, _, _) -> #(imports, [dec, ..externals], bindings)
    Typ(_, _, _, _) -> emittables
  }
}

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn map(mod: Mod(a), f: fn(String, a) -> b) -> Mod(b) {
  use dec <- list.map(mod)

  case dec {
    Imp(source, path, alias) -> Imp(source, path, alias)
    Let(exposed, var, typ, expr) -> Let(exposed, var, typ, f(var, expr))
    Ext(exposed, var, typ, name) -> Ext(exposed, var, typ, name)
    Typ(exposed, name, vars, typ) -> Typ(exposed, name, vars, typ)
  }
}
