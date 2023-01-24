// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{None, Option, Some}
import gleam/string
import gleam/io
import gleam/float

// TYPES -----------------------------------------------------------------------

pub type Document {
  Column(fn(Int) -> Document)
  Concat(fn() -> Document, fn() -> Document)
  Empty
  Line(String, String)
  Nest(Int, fn() -> Document)
  Nesting(fn(Int) -> Document)
  Text(String)
  Union(Document, Document)
}

type Simple {
  SEmpty
  SText(String, fn() -> Simple)
  SLine(Int, String, fn() -> Simple)
}

// CONSTANTS: SYMBOLS ----------------------------------------------------------

pub const ampersand: Document = Text("&")

pub const arrow: Document = Text("->")

pub const asterisk: Document = Text("*")

pub const at: Document = Text("@")

pub const backslash: Document = Text("\\")

pub const backtick: Document = Text("`")

pub const caret: Document = Text("^")

pub const colon: Document = Text(":")

pub const comma: Document = Text(",")

pub const dollar: Document = Text("$")

pub const dot: Document = Text(".")

pub const doublequote: Document = Text("\"")

pub const equals: Document = Text("=")

pub const exclamation: Document = Text("!")

pub const fatarrow: Document = Text("=>")

pub const forwardslash: Document = Text("/")

pub const hash: Document = Text("#")

pub const lbrace: Document = Text("{")

pub const lbracket: Document = Text("[")

pub const lcaret: Document = Text("<")

pub const lparen: Document = Text("(")

pub const minus: Document = Text("-")

pub const percent: Document = Text("%")

pub const pipe: Document = Text("|")

pub const plus: Document = Text("+")

pub const question: Document = Text("?")

pub const rbrace: Document = Text("}")

pub const rbracket: Document = Text("]")

pub const rcaret: Document = Text(">")

pub const rparen: Document = Text(")")

pub const semi: Document = Text(";")

pub const singlequote: Document = Text("'")

pub const space: Document = Text(" ")

pub const tilde: Document = Text("~")

pub const underscore: Document = Text("_")

// CONSTRUCTORS ----------------------------------------------------------------

pub fn append(a: Document, b: Document) -> Document {
  Concat(fn() { b }, fn() { a })
}

pub fn concat(docs: List(Document)) -> Document {
  case docs {
    [] -> empty
    [doc] -> doc
    [Empty, ..rest] -> concat(rest)
    [doc, ..rest] -> append(doc, concat(rest))
  }
}

pub fn column(f: fn(Int) -> Document) -> Document {
  Column(f)
}

pub const empty: Document = Empty

pub fn group(doc: Document) -> Document {
  Union(flatten(doc), doc)
}

pub fn nest(doc: Document, i: Int) -> Document {
  Nest(i, fn() { doc })
}

pub fn nesting(f: fn(Int) -> Document) -> Document {
  Nesting(f)
}

pub const newline: Document = Line(" ", "")

pub fn doubleline() -> Document {
  append(newline, newline)
}

pub fn text(text: String) -> Document {
  Text(text)
}

pub fn number(num: Float) -> Document {
  text(float.to_string(num))
}

pub const tightline: Document = Line("", "")

pub fn softline() -> Document {
  group(newline)
}

// COMBINATORS -----------------------------------------------------------------

pub fn surround(doc: Document, left: Document, right: Document) -> Document {
  left
  |> append(doc)
  |> append(right)
}

pub fn singlequotes(doc: Document) -> Document {
  surround(doc, singlequote, singlequote)
}

pub fn doublequotes(doc: Document) -> Document {
  surround(doc, doublequote, doublequote)
}

pub fn parens(doc: Document) -> Document {
  surround(doc, lparen, rparen)
}

pub fn brackets(doc: Document) -> Document {
  surround(doc, lbracket, rbracket)
}

pub fn braces(doc: Document) -> Document {
  surround(doc, lbrace, rbrace)
}

//

pub fn join(docs: List(Document), separator: Document) -> Document {
  case docs {
    [] -> empty
    [doc] -> doc
    [Empty, ..rest] -> join(rest, separator)
    [doc, ..rest] -> {
      use acc, cur <- list.fold(rest, doc)
      case cur {
        Empty -> acc
        _ -> append(append(acc, separator), cur)
      }
    }
  }
}

pub fn lines(docs: List(Document)) -> Document {
  join(docs, newline)
}

pub fn doublelines(docs: List(Document)) -> Document {
  join(docs, append(newline, newline))
}

pub fn softlines(docs: List(Document)) -> Document {
  join(docs, softline())
}

pub fn words(docs: List(Document)) -> Document {
  join(docs, space)
}

//

pub fn align(doc: Document) -> Document {
  use col <- column
  use indent <- nesting

  nest(doc, col - indent)
}

pub fn hang(doc: Document, i: Int) -> Document {
  doc
  |> nest(i)
  |> align
}

pub fn indent(doc: Document, i: Int) -> Document {
  let spaces = text(string.repeat(" ", i))

  spaces
  |> append(doc)
  |> hang(i)
}

//

pub fn optional(value: Option(a), f: fn(a) -> Document) -> Document {
  case value {
    None -> empty
    Some(v) -> f(v)
  }
}

pub fn when(condition: Bool, f: fn() -> Document) -> Document {
  case condition {
    True -> f()
    False -> empty
  }
}

// QUERIES ---------------------------------------------------------------------

fn fits(doc: Simple, width: Int) -> Bool {
  case doc {
    _ if width < 0 -> False
    SEmpty -> True
    SText(text, thunk) -> fits(thunk(), width - string.length(text))
    SLine(_, _, _) -> True
  }
}

fn choose(doc: Simple, thunk: fn() -> Simple, w: Int, k: Int) -> Simple {
  case fits(doc, w - k) {
    True -> doc
    False -> thunk()
  }
}

// MANIPULATIONS ---------------------------------------------------------------
// CONVERSIONS -----------------------------------------------------------------

pub fn print(doc: Document, w: Int) -> String {
  [#(doc, 0)]
  |> simplify(w, 0)
  |> layout
}

fn simplify(docs: List(#(Document, Int)), w: Int, k: Int) -> Simple {
  case docs {
    [] -> SEmpty
    [#(Column(thunk), i), ..rest] -> simplify([#(thunk(k), i), ..rest], w, k)
    [#(Concat(a, b), i), ..rest] ->
      simplify([#(a(), i), #(b(), i), ..rest], w, k)
    [#(Empty, _), ..rest] -> simplify(rest, w, k)
    [#(Line(_, separator), i), ..rest] ->
      SLine(
        i,
        separator,
        fn() { simplify(rest, w, i + string.length(separator)) },
      )
    [#(Nest(j, thunk), i), ..rest] ->
      simplify([#(thunk(), i + j), ..rest], w, k)
    [#(Nesting(thunk), i), ..rest] -> simplify([#(thunk(i), i), ..rest], w, k)
    [#(Text(text), _), ..rest] ->
      SText(text, fn() { simplify(rest, w, k + string.length(text)) })
    [#(Union(a, b), i), ..rest] ->
      choose(
        simplify([#(a, i), ..rest], w, k),
        fn() { simplify([#(b, i), ..rest], w, k) },
        w,
        k,
      )
  }
}

// UTILS -----------------------------------------------------------------------

fn flatten(doc: Document) -> Document {
  case doc {
    Concat(a, b) -> Concat(fn() { flatten(a()) }, fn() { flatten(b()) })
    Nest(i, a) -> Nest(i, fn() { flatten(a()) })
    Union(a, _) -> flatten(a)
    Line(hsep, _) -> Text(hsep)
    Nesting(f) -> flatten(f(0))
    Column(f) -> flatten(f(0))
    _ -> doc
  }
}

fn layout(doc: Simple) -> String {
  use s, c <- list.fold(layout_inner(doc, []), "")
  s <> c
}

fn layout_inner(doc: Simple, lines: List(String)) -> List(String) {
  case doc {
    SEmpty -> lines
    SText(text, thunk) -> layout_inner(thunk(), [text, ..lines])
    SLine(indent, seperator, thunk) ->
      layout_inner(
        thunk(),
        ["\n" <> string.repeat(" ", indent) <> seperator, ..lines],
      )
  }
}
