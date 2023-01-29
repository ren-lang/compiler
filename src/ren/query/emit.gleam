// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{None, Option}
import gleam/string
import ren/ast/mod.{
  Dec, Ext, External, Imp, Let as LetDec, Mod, Package, Project,
}
import ren/ast/lit.{Field}
import ren/pretty.{Document}
import ren/ir/imp.{
  Access, Add, And, Array, Arrow, Assign, Binop, Block, Break, Call, Comma,
  Comment, Const, Continue, Div, Eq, Export, Expr, Expression, ForIn, Function,
  Gt, Gte, IIFE, If, IfElse, In, Index, Instanceof, JSFalse, JSTrue, Let, Lt,
  Lte, Mod as Modulo, Mul, Neg, Neq, New, Not, Null, Number, Object, Or, Pos,
  Pow, Return, Spread, Statement, String, Sub, Ternary, Throw, Typeof, Undefined,
  Unop, Var, While,
}

// EMIT ------------------------------------------------------------------------

///
///
pub fn mod(input: Mod(Statement)) -> String {
  input
  |> emit_mod(None, None)
  |> pretty.print(100)
}

//

fn emit_mod(
  mod: Mod(Statement),
  pkg_path: Option(String),
  mod_name: Option(String),
) -> Document {
  let prelude_import = {
    use pkgs <- pretty.optional(pkg_path)

    pretty.words([
      import_,
      pretty.text("{ $eq, $fn }"),
      from,
      pretty.doublequotes(pretty.text(pkgs <> "/prelude.js")),
    ])
  }

  let ffi_import = {
    use <- pretty.when(list.length(mod.externals(mod)) > 0)
    use name <- pretty.optional(mod_name)

    pretty.words([
      import_,
      pretty.asterisk,
      as_,
      pretty.text("$FFI"),
      from,
      pretty.doublequotes(pretty.text("./" <> name <> ".ffi.js")),
    ])
  }

  let #(imports, externals, bindings) = mod.emittables(mod)

  pretty.doublelines([
    pretty.lines([prelude_import, ffi_import]),
    pretty.lines(list.map(imports, emit_imp(_, pkg_path))),
    pretty.lines(list.map(externals, emit_ext)),
    pretty.doublelines(list.map(bindings, emit_let)),
  ])
}

fn emit_imp(dec: Dec(Statement), pkg_path: Option(String)) -> Document {
  assert Imp(source, path, alias) = dec

  use pkgs <- pretty.optional(pkg_path)
  let path = case source {
    Project -> pretty.text(path <> ".ren.js")
    Package -> pretty.text(pkgs <> "/" <> path <> ".ren.js")
    External -> pretty.text(path)
  }

  case alias {
    [] -> pretty.words([import_, pretty.doublequotes(path)])
    _ ->
      pretty.words([
        import_,
        pretty.asterisk,
        as_,
        pretty.text(string.join(alias, "$")),
        from,
        pretty.doublequotes(path),
      ])
  }
}

fn emit_ext(dec: Dec(Statement)) -> Document {
  assert Ext(exposed, var, _, ffi_name) = dec
  let export = pretty.when(exposed, fn() { export })

  pretty.words([
    export,
    const_,
    emit_js_var(var),
    pretty.equals,
    pretty.text("$FFI"),
    pretty.dot,
    pretty.text(ffi_name),
    pretty.semi,
  ])
}

fn emit_let(dec: Dec(Statement)) -> Document {
  assert LetDec(exposed, var, _, stmt) = dec
  let export = pretty.when(exposed, fn() { export })

  pretty.words([
    export,
    case stmt {
      Expr(Arrow(args, body)) -> emit_statement(Function(var, args, body))
      Expr(expr) -> emit_statement(Const(var, expr))
      _ ->
        emit_statement(Const(
          var,
          stmt
          |> imp.flatten
          |> imp.return
          |> imp.as_expression,
        ))
    },
  ])
}

//

fn emit_statement(stmt: Statement) -> Document {
  case stmt {
    Block([]) -> pretty.empty
    Block([stmt]) -> emit_statement(stmt)
    Block(_) -> emit_as_block(stmt)
    Break -> break
    Comment(text) -> pretty.text("// " <> text)
    Const(var, expr) ->
      pretty.words([
        const_,
        emit_js_var(var),
        pretty.equals,
        emit_expression(expr),
      ])
    Continue -> continue
    Export(stmt) -> emit_statement(stmt)
    Expr(expr) -> emit_expression(expr)
    ForIn(var, expr, stmts) ->
      pretty.words([
        for,
        pretty.parens(pretty.words([
          const_,
          emit_js_var(var),
          in,
          emit_expression(expr),
        ])),
        emit_as_block(Block(stmts)),
      ])
    Function(var, args, stmts) ->
      pretty.words([
        function,
        emit_js_var(var),
        pretty.parens(pretty.join(
          list.map(args, emit_js_var),
          pretty.text(", "),
        )),
        emit_as_block(Block(stmts)),
      ])
    If(cond, then) ->
      pretty.words([
        if_,
        pretty.parens(emit_expression(cond)),
        emit_as_block(Block(then)),
      ])
    IfElse(cond, then, else_) ->
      pretty.words([
        if_,
        pretty.parens(emit_expression(cond)),
        emit_as_block(Block(then)),
        else,
        emit_as_block(Block(else_)),
      ])
    Let(var, expr) ->
      pretty.words([
        let_,
        emit_js_var(var),
        pretty.equals,
        emit_expression(expr),
      ])
    Return(expr) -> pretty.words([return, emit_expression(expr)])
    Throw(message) ->
      pretty.words([
        throw,
        new,
        pretty.text("Error")
        |> pretty.append(pretty.parens(pretty.doublequotes(pretty.text(message)))),
      ])
    While(cond, stmts) ->
      pretty.words([
        while,
        pretty.parens(emit_expression(cond)),
        emit_as_block(Block(stmts)),
      ])
  }
}

fn emit_expression(expr: Expression) -> Document {
  let precedence = imp.precedence_of(expr)

  case expr {
    Access(expr, []) -> emit_expression(expr)
    Access(expr, keys) ->
      pretty.concat([
        emit_parens(expr, precedence),
        pretty.dot,
        pretty.join(list.map(keys, pretty.text), pretty.dot),
      ])
    Array(elements) ->
      list.map(elements, emit_expression)
      |> pretty.join(pretty.text(", "))
      |> pretty.brackets
    Arrow(args, body) ->
      pretty.words([
        pretty.parens(pretty.join(
          list.map(args, emit_js_var),
          pretty.text(", "),
        )),
        pretty.fatarrow,
        case body {
          [Expr(Object(_) as expr)] -> pretty.parens(emit_expression(expr))
          [Expr(expr)] -> emit_expression(expr)
          [Return(expr)] -> emit_expression(expr)
          [Block(stmts)] -> emit_as_block(Block(stmts))
          _ -> emit_as_block(Block(body))
        },
      ])
    Assign(name, expr) ->
      pretty.words([pretty.text(name), pretty.equals, emit_expression(expr)])
    Binop(lhs, Add, rhs) -> emit_binop(lhs, "+", rhs)
    Binop(lhs, And, rhs) -> emit_binop(lhs, "&&", rhs)
    Binop(lhs, Comma, rhs) -> emit_binop(lhs, ",", rhs)
    Binop(lhs, Div, rhs) -> emit_binop(lhs, "/", rhs)
    Binop(lhs, Eq, rhs) -> emit_binop(lhs, "===", rhs)
    Binop(lhs, Gt, rhs) -> emit_binop(lhs, ">", rhs)
    Binop(lhs, Gte, rhs) -> emit_binop(lhs, ">=", rhs)
    Binop(lhs, In, rhs) -> emit_binop(lhs, "in", rhs)
    Binop(lhs, Instanceof, rhs) -> emit_binop(lhs, "instanceof", rhs)
    Binop(lhs, Lt, rhs) -> emit_binop(lhs, "<", rhs)
    Binop(lhs, Lte, rhs) -> emit_binop(lhs, "<=", rhs)
    Binop(lhs, Modulo, rhs) -> emit_binop(lhs, "%", rhs)
    Binop(lhs, Mul, rhs) -> emit_binop(lhs, "*", rhs)
    Binop(lhs, Neq, rhs) -> emit_binop(lhs, "!==", rhs)
    Binop(lhs, Or, rhs) -> emit_binop(lhs, "||", rhs)
    Binop(lhs, Pow, rhs) -> emit_binop(lhs, "**", rhs)
    Binop(lhs, Sub, rhs) -> emit_binop(lhs, "-", rhs)
    Call(fun, args) ->
      pretty.concat([
        emit_parens(fun, precedence),
        pretty.parens(pretty.join(
          list.map(args, emit_expression),
          pretty.text(", "),
        )),
      ])
    IIFE(stmts) ->
      pretty.words([
        pretty.text("()"),
        pretty.fatarrow,
        emit_statement(Block(stmts)),
      ])
      |> pretty.parens
      |> pretty.append(pretty.text("()"))
    Index(expr, index) ->
      pretty.concat([
        emit_parens(expr, precedence),
        pretty.brackets(emit_expression(index)),
      ])
    JSFalse -> false
    JSTrue -> true
    Null -> null
    Number(num) -> pretty.number(num)
    Object(fields) -> {
      let emit_field = fn(field: Field(Expression)) {
        case field {
          Field(key, Var(var)) if var == key -> pretty.text(field.key)
          _ ->
            pretty.concat([
              pretty.text(field.key),
              pretty.colon,
              pretty.space,
              emit_expression(field.value),
            ])
        }
      }
      list.map(fields, emit_field)
      |> pretty.join(pretty.text(", "))
      |> pretty.braces
    }
    Spread(expr) ->
      pretty.concat([
        pretty.dot,
        pretty.dot,
        pretty.dot,
        emit_parens(expr, precedence),
      ])
    String(str) -> pretty.doublequotes(pretty.text(str))
    Ternary(cond, then, else) ->
      pretty.words([
        emit_parens(cond, precedence),
        pretty.question,
        emit_parens(then, precedence),
        pretty.colon,
        emit_parens(else, precedence),
      ])
    Undefined -> undefined
    Unop(Neg, expr) ->
      pretty.concat([pretty.minus, emit_parens(expr, precedence)])
    Unop(New, expr) -> pretty.words([new, emit_parens(expr, precedence)])
    Unop(Not, expr) ->
      pretty.concat([pretty.exclamation, emit_parens(expr, precedence)])
    Unop(Pos, expr) ->
      pretty.concat([pretty.plus, emit_parens(expr, precedence)])
    Unop(Typeof, expr) -> pretty.words([typeof, emit_parens(expr, precedence)])
    Var(var) -> emit_js_var(var)
  }
}

//

fn emit_as_block(stmt: Statement) -> Document {
  // We're going to join each of the statements in the block with a newline, but
  // for some statements we want to add an extra newline before them for nicer
  // formatting.
  //
  let with_spacing = fn(stmt) {
    case stmt {
      Block(_) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      Break -> emit_statement(stmt)
      Comment(_) -> emit_statement(stmt)
      Const(_, _) -> emit_statement(stmt)
      Continue -> pretty.concat([pretty.newline, emit_statement(stmt)])
      Export(stmt) -> emit_statement(stmt)
      Expr(_) -> emit_statement(stmt)
      ForIn(_, _, _) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      Function(_, _, _) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      If(_, _) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      IfElse(_, _, _) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      Let(_, _) -> emit_statement(stmt)
      Return(_) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      Throw(_) -> pretty.concat([pretty.newline, emit_statement(stmt)])
      While(_, _) -> pretty.concat([pretty.newline, emit_statement(stmt)])
    }
  }

  pretty.lines([
    pretty.lbrace,
    case imp.statements(stmt) {
      [] -> pretty.empty
      [stmt, ..rest] ->
        pretty.lines([emit_statement(stmt), ..list.map(rest, with_spacing)])
        |> pretty.indent(4)
    },
    pretty.rbrace,
  ])
}

fn emit_parens(expr: Expression, precedence: Int) -> Document {
  case precedence > imp.precedence_of(expr) {
    True -> pretty.parens(emit_expression(expr))
    False -> emit_expression(expr)
  }
}

fn emit_binop(lhs: Expression, op: String, rhs: Expression) -> Document {
  pretty.words([
    emit_parens(lhs, imp.precedence_of(lhs)),
    pretty.text(op),
    emit_parens(rhs, imp.precedence_of(rhs)),
  ])
}

fn emit_js_var(name: String) -> Document {
  let name = pretty.text(name)
  let keywords = [
    as_,
    async,
    await,
    break,
    case_,
    catch,
    class,
    const_,
    continue,
    debugger,
    default,
    delete,
    do,
    else,
    enum,
    export,
    extends,
    false,
    finally,
    from,
    for,
    function,
    if_,
    implements,
    import_,
    in,
    instanceof,
    interface,
    let_,
    new,
    null,
    package,
    private,
    protected,
    public,
    return,
    static,
    super,
    switch,
    this,
    throw,
    true,
    try_,
    typeof,
    undefined,
    var,
    void,
    while,
    with,
    yield,
  ]

  case list.contains(keywords, name) {
    True -> pretty.append(pretty.underscore, name)
    False -> name
  }
}

//

const as_: Document = pretty.Text("as")

const async: Document = pretty.Text("async")

const await: Document = pretty.Text("await")

const break: Document = pretty.Text("break")

const case_: Document = pretty.Text("case")

const catch: Document = pretty.Text("catch")

const class: Document = pretty.Text("class")

const const_: Document = pretty.Text("const")

const continue: Document = pretty.Text("continue")

const debugger: Document = pretty.Text("debugger")

const default: Document = pretty.Text("default")

const delete: Document = pretty.Text("delete")

const do: Document = pretty.Text("do")

const else: Document = pretty.Text("else")

const enum: Document = pretty.Text("enum")

const export: Document = pretty.Text("export")

const extends: Document = pretty.Text("extends")

const false: Document = pretty.Text("false")

const finally: Document = pretty.Text("finally")

const from: Document = pretty.Text("from")

const for: Document = pretty.Text("for")

const function: Document = pretty.Text("function")

const if_: Document = pretty.Text("if")

const implements: Document = pretty.Text("implements")

const import_: Document = pretty.Text("import")

const in: Document = pretty.Text("in")

const instanceof: Document = pretty.Text("instanceof")

const interface: Document = pretty.Text("interface")

const let_: Document = pretty.Text("let")

const new: Document = pretty.Text("new")

const null: Document = pretty.Text("null")

const package: Document = pretty.Text("package")

const private: Document = pretty.Text("private")

const protected: Document = pretty.Text("protected")

const public: Document = pretty.Text("public")

const return: Document = pretty.Text("return")

const static: Document = pretty.Text("static")

const super: Document = pretty.Text("super")

const switch: Document = pretty.Text("switch")

const this: Document = pretty.Text("this")

const throw: Document = pretty.Text("throw")

const true: Document = pretty.Text("true")

const try_: Document = pretty.Text("try")

const typeof: Document = pretty.Text("typeof")

const undefined: Document = pretty.Text("undefined")

const var: Document = pretty.Text("var")

const void: Document = pretty.Text("void")

const while: Document = pretty.Text("while")

const with: Document = pretty.Text("with")

const yield: Document = pretty.Text("yield")

// CONSTANTS -------------------------------------------------------------------

/// export function $fn (f, thisArg) {
///     if (typeof f == "object" || Array.isArray(f)) {
///         for (const k in f) {
///             f[k] = $fn(f[k], f)
///         }
///     }
/// 
///     if (typeof f != "function") {
///         return f
///     }
/// 
///     return (...args) => {
///         if (args.length == f.length) {
///             return f(...args)
///         }
/// 
///         if (args.length > f.length) {
///             return $fn(f(...args.slice(0, f.length)), thisArg)(...args.slice(f.length))
///         }
/// 
///         return $fn(f.bind(thisArg, ...args))
///     }
/// }
const prelude_fn: Statement = Export(
  Function(
    "$fn",
    ["f"],
    [
      If(
        Binop(
          Binop(Unop(Typeof, Var("f")), Eq, String("object")),
          Or,
          Call(Access(Var("Array"), ["isArray"]), [Var("f")]),
        ),
        [
          ForIn(
            "k",
            Var("f"),
            [
              Expr(
                Assign(
                  "f.k",
                  Call(Var("$fn"), [Access(Var("f"), ["k"]), Var("f")]),
                ),
              ),
            ],
          ),
        ],
      ),
      If(
        Binop(Unop(Typeof, Var("f")), Neq, String("function")),
        [Return(Var("f"))],
      ),
      Return(
        Arrow(
          ["...args"],
          [
            If(
              Binop(
                Access(Var("args"), ["length"]),
                Eq,
                Access(Var("f"), ["length"]),
              ),
              [Return(Call(Var("f"), [Spread(Var("args"))]))],
            ),
            If(
              Binop(
                Access(Var("args"), ["length"]),
                Gt,
                Access(Var("f"), ["length"]),
              ),
              [
                Return(
                  Call(
                    Call(
                      Var("$fn"),
                      [
                        Call(
                          Var("f"),
                          [
                            Spread(
                              Call(
                                Access(Var("args"), ["slice"]),
                                [Number(0.0), Access(Var("f"), ["length"])],
                              ),
                            ),
                          ],
                        ),
                        Var("thisArg"),
                      ],
                    ),
                    [
                      Spread(
                        Call(
                          Access(Var("args"), ["slice"]),
                          [Access(Var("f"), ["length"])],
                        ),
                      ),
                    ],
                  ),
                ),
              ],
            ),
            Return(
              Call(
                Var("$fn"),
                [
                  Call(
                    Access(Var("f"), ["bind"]),
                    [Var("thisArg"), Spread(Var("args"))],
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
    ],
  ),
)

/// export function $eq (x, y) {
///     const values = [x, y]
/// 
///     while (values.length > 0) {
///         const a = values.pop()
///         const b = values.pop()
/// 
///         if (a === b) continue
///         if (a === null || a === undefined || b === null || b === undefined) return false
/// 
///         if (typeof a === 'object' || typeof b === 'object') {
///             if (a.valueOf() === b.valueOf()) continue
///             if (a.constructor !== b.constructor) return false
///             if (a.constructor === Date) {
///                 if (!(a > b || a < b)) {
///                     continue
///                 } else {
///                     return false
///                 }
///             }
/// 
///             for (const k in a) {
///                 values.push(a[k], b[k])
///             }
/// 
///             continue
///         }
/// 
///         return false
///     }
/// 
///     return true
/// }
const prelude_eq: Statement = Export(
  Function(
    "$eq",
    ["x", "y"],
    [
      Const("values", Array([Var("x"), Var("y")])),
      While(
        Binop(Access(Var("values"), ["length"]), Gt, Number(0.0)),
        [
          Const("a", Call(Access(Var("values"), ["pop"]), [])),
          Const("b", Call(Access(Var("values"), ["pop"]), [])),
          If(Binop(Var("a"), Eq, Var("b")), [Continue]),
          If(
            Binop(
              Binop(
                Binop(Var("a"), Eq, Null),
                Or,
                Binop(Var("a"), Eq, Undefined),
              ),
              Or,
              Binop(
                Binop(Var("b"), Eq, Null),
                Or,
                Binop(Var("b"), Eq, Undefined),
              ),
            ),
            [Return(JSFalse)],
          ),
          If(
            Binop(
              Binop(Unop(Typeof, Var("a")), Eq, String("object")),
              Or,
              Binop(Unop(Typeof, Var("b")), Eq, String("object")),
            ),
            [
              If(
                Binop(
                  Call(Access(Var("a"), ["valueOf"]), []),
                  Eq,
                  Call(Access(Var("b"), ["valueOf"]), []),
                ),
                [Continue],
              ),
              If(
                Binop(
                  Access(Var("a"), ["constructor"]),
                  Neq,
                  Access(Var("b"), ["constructor"]),
                ),
                [Return(JSFalse)],
              ),
              If(
                Binop(
                  Access(Var("a"), ["constructor"]),
                  Eq,
                  Access(Var("Date"), ["constructor"]),
                ),
                [
                  IfElse(
                    Unop(
                      Not,
                      Binop(
                        Binop(Var("a"), Gt, Var("b")),
                        Or,
                        Binop(Var("a"), Lt, Var("b")),
                      ),
                    ),
                    [Continue],
                    [Return(JSFalse)],
                  ),
                ],
              ),
              ForIn(
                "k",
                Var("a"),
                [
                  Expr(
                    Call(
                      Access(Var("values"), ["push"]),
                      [Access(Var("a"), ["k"]), Access(Var("b"), ["k"])],
                    ),
                  ),
                ],
              ),
              Continue,
            ],
          ),
          Return(JSFalse),
        ],
      ),
      Return(JSTrue),
    ],
  ),
)

pub const prelude: List(Statement) = [
  Comment("This utility gets applied to all functions defined in ren:"),
  Comment("top-level declarations, let bindings, or anonymous functions. It"),
  Comment("allows us to support partial application of functions without"),
  Comment("explicit currying."),
  Comment(""),
  Comment("Any external imports are also automatically wrapped in this"),
  Comment("utility, which means external JavaScript will also support"),
  Comment("partial application!"),
  Comment(""),
  Comment("Beyond some performance benefits, another reason this utility is"),
  Comment("useful is that it makes it easier to consume ren code in"),
  Comment("JavaScript. If ren functions were auto-curried, external"),
  Comment("JavaScript would need to call ren functions like `add(1)(2)`"),
  Comment("which is not very idiomatic!"),
  prelude_fn,
  Comment("Ren uses structural equality to compare objects and arrays. This"),
  Comment("is different to equality in JavaScript that is purely referential."),
  Comment("We need this utility to use in place of the usual `==` operator."),
  prelude_eq,
]
