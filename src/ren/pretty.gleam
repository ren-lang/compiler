// IMPORTS ---------------------------------------------------------------------

import gleam/string
import gleam/string_builder.{StringBuilder}

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Document {
  Break
  Concat(Document, Document)
  Empty
  Group(Document)
  Nest(Int, Document)
  Text(String)
}

type SimpleDocument {
  SEmpty
  SText(String, SimpleDocument)
  SLine(Int, SimpleDocument)
}

type Mode {
  Flat
  Split
}

// BASIC COMBINATORS -----------------------------------------------------------

pub fn empty() -> Document {
  Empty
}

pub fn text(str: String) -> Document {
  Text(str)
}

pub fn nest(doc: Document, indent: Int) -> Document {
  Nest(indent, doc)
}

pub fn group(doc: Document) -> Document {
  Group(doc)
}

pub fn break() -> Document {
  Break
}

pub fn concat(docs: List(Document)) -> Document {
  join(docs, Break)
}

pub fn concat_group(docs: List(Document)) -> Document {
  concat(docs)
  |> group
}

pub fn append(left: Document, right: Document) -> Document {
  case left, right {
    Empty, _ -> right
    _, Empty -> left
    _, _ -> Concat(left, Concat(Break, right))
  }
}

pub fn join(docs: List(Document), sep: Document) -> Document {
  case docs {
    [x, y, ..z] -> Concat(x, Concat(sep, join([y, ..z], sep)))
    [x] -> x
    [] -> Empty
  }
}

// COMBINATORS -----------------------------------------------------------------

pub fn surround(left: Document, right: Document, doc: Document) -> Document {
  join([left, doc, right], Empty)
}

pub fn brackets(doc: Document) -> Document {
  surround(text("["), text("]"), doc)
}

pub fn braces(doc: Document) -> Document {
  surround(text("{"), text("}"), doc)
}

pub fn parens(doc: Document) -> Document {
  surround(text("("), text(")"), doc)
}

pub fn indent(doc: Document) -> Document {
  nest(doc, 2)
}

// QUERIES ---------------------------------------------------------------------

fn fits(docs: List(#(Int, Mode, Document)), w: Int) -> Bool {
  case docs {
    _ if w < 0 -> False
    [] -> True
    [#(_, _, Empty), ..z] -> fits(z, w)
    [#(i, m, Concat(x, y)), ..z] -> fits([#(i, m, x), #(i, m, y), ..z], w)
    [#(i, m, Nest(j, x)), ..z] -> fits([#(i + j, m, x), ..z], w)
    [#(_, _, Text(s)), ..z] -> fits(z, w - string.length(s))
    [#(_, Flat, Break), ..z] -> fits(z, w - 1)
    [#(_, Split, Break), ..] -> True
    [#(i, _, Group(x)), ..z] -> fits([#(i, Flat, x), ..z], w)
  }
}

// MANIPULATIONS ---------------------------------------------------------------

fn format(docs: List(#(Int, Mode, Document)), w: Int, k: Int) -> SimpleDocument {
  case docs {
    [] -> SEmpty
    [#(_, _, Empty), ..z] -> format(z, w, k)
    [#(i, m, Concat(x, y)), ..z] -> format([#(i, m, x), #(i, m, y), ..z], w, k)
    [#(i, m, Nest(j, x)), ..z] -> format([#(i + j, m, x), ..z], w, k)
    [#(_, _, Text(s)), ..z] -> SText(s, format(z, w, k + string.length(s)))
    [#(_, Flat, Break), ..z] -> SText(" ", format(z, w, k + 1))
    [#(i, Split, Break), ..z] -> SLine(i, format(z, w, i))
    [#(i, _, Group(x)), ..z] ->
      case fits([#(i, Flat, x), ..z], w - k) {
        True -> format([#(i, Flat, x), ..z], w, k)
        False -> format([#(i, Split, x), ..z], w, k)
      }
  }
}

// CONVERSIONS -----------------------------------------------------------------

pub fn print(doc: Document, width: Int) {
  let doc = format([#(0, Flat, Group(doc))], width, 0)
  let str = to_string(doc)

  str <> "\n"
}

fn to_string(doc: SimpleDocument) -> String {
  to_string_builder(doc)
  |> string_builder.to_string()
}

fn to_string_builder(doc: SimpleDocument) -> StringBuilder {
  case doc {
    SEmpty -> string_builder.new()
    SText(str, doc) ->
      to_string_builder(doc)
      |> string_builder.prepend(str)
    SLine(indent, doc) ->
      to_string_builder(doc)
      |> string_builder.prepend(string.repeat(" ", indent))
      |> string_builder.prepend("\n")
  }
}
// UTILS -----------------------------------------------------------------------
