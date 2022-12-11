// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/option.{None, Option, Some}
import gleam/result
import gleam/string

// TYPES -----------------------------------------------------------------------

///
///
pub type Token {
  EOF
  Identifier(Identifier)
  Keyword(Keyword)
  Literal(Literal)
  Operator(Operator)
  Symbol(Symbol)
  Unknown(String)
}

///
///
pub type Keyword {
  As
  Assert
  Case
  Else
  Expect
  Ext
  Fun
  If
  Import
  Let
  On
  Pkg
  Pub
  Switch
  Then
  Type
}

///
///
pub type Identifier {
  Upper(String)
  Lower(String)
}

///
///
pub type Literal {
  Number(Float)
  String(String)
}

///
///
pub type Symbol {
  Arrow
  Colon
  Comma
  Dot
  DoubleDot
  DoubleSlash
  Equals
  LBrace
  LBracket
  LParen
  Question
  RBrace
  RBracket
  RParen
  Semicolon
  Underscore
}

///
///
pub type Operator {
  Add
  And
  Div
  Eq
  Gt
  Gte
  Lt
  Lte
  Mod
  Mul
  Neq
  Or
  Pipe
  Pow
  Sub
}

// CONSTANTS -------------------------------------------------------------------

pub const as_: Token = Keyword(As)

pub const assert_: Token = Keyword(Assert)

pub const case_: Token = Keyword(Case)

pub const else: Token = Keyword(Else)

pub const expect: Token = Keyword(Expect)

pub const ext: Token = Keyword(Ext)

pub const fun: Token = Keyword(Fun)

pub const if_: Token = Keyword(If)

pub const import_: Token = Keyword(Import)

pub const let_: Token = Keyword(Let)

pub const on: Token = Keyword(On)

pub const pkg: Token = Keyword(Pkg)

pub const pub_: Token = Keyword(Pub)

pub const switch: Token = Keyword(Switch)

pub const then: Token = Keyword(Then)

pub const type_: Token = Keyword(Type)

pub const arrow: Token = Symbol(Arrow)

pub const colon: Token = Symbol(Colon)

pub const comma: Token = Symbol(Comma)

pub const dot: Token = Symbol(Dot)

pub const double_dot: Token = Symbol(DoubleDot)

pub const double_slash: Token = Symbol(DoubleSlash)

pub const equals: Token = Symbol(Equals)

pub const lbrace: Token = Symbol(LBrace)

pub const lbracket: Token = Symbol(LBracket)

pub const lparen: Token = Symbol(LParen)

pub const question: Token = Symbol(Question)

pub const rbrace: Token = Symbol(RBrace)

pub const rbracket: Token = Symbol(RBracket)

pub const rparen: Token = Symbol(RParen)

pub const semicolon: Token = Symbol(Semicolon)

pub const underscore: Token = Symbol(Underscore)

pub const add: Token = Operator(Add)

pub const sub: Token = Operator(Sub)

pub const mul: Token = Operator(Mul)

pub const div: Token = Operator(Div)

pub const mod: Token = Operator(Mod)

pub const pow: Token = Operator(Pow)

pub const and: Token = Operator(And)

pub const or: Token = Operator(Or)

pub const eq: Token = Operator(Eq)

pub const neq: Token = Operator(Neq)

pub const lt: Token = Operator(Lt)

pub const lte: Token = Operator(Lte)

pub const gt: Token = Operator(Gt)

pub const gte: Token = Operator(Gte)

pub const pipe: Token = Operator(Pipe)

// CONSTRUCTORS ----------------------------------------------------------------

/// Take some alphanumeric string and (potentially) convert it into either an
/// `Upper` or `Lower` identifier token based on casing. Invalid identifiers
/// will return `None`.
///
pub fn identifier(name: String) -> Option(Token) {
  let first = result.unwrap(string.first(name), "")

  case <<first:utf8>> {
    <<code:int>> if code > 64 && code < 91 -> Some(Identifier(Upper(name)))
    <<code:int>> if code > 96 && code < 123 -> Some(Identifier(Lower(name)))
    _ -> None
  }
}

///
///
/// ❗️ THIS FUNCTION IS UNSAFE! It will panic if the first character of the
/// string is not an uppercase letter. If you'd like a safe version of this function,
/// use the more general `identifier` function instead.
///
pub fn upper(name: String) -> Token {
  assert Ok(first) = string.first(name)

  // This matching is intentionally not exhaustive: we want the program to crash
  // if an unexpected string was passed in.
  case <<first:utf8>> {
    <<code:int>> if code > 64 && code < 91 -> Identifier(Upper(name))
  }
}

///
///
/// ❗️ THIS FUNCTION IS UNSAFE! It will panic if the first character of the
/// string is not a lowercase letter. If you'd like a safe version of this 
/// use the more general `identifier` function instead.
///
pub fn lower(name: String) -> Token {
  assert Ok(first) = string.first(name)

  // This matching is intentionally not exhaustive: we want the program to crash
  // if an unexpected string was passed in.
  case <<first:utf8>> {
    <<code:int>> if code > 96 && code < 123 -> Identifier(Lower(name))
  }
}

///
///
pub fn num(num: Float) -> Token {
  Literal(Number(num))
}

/// In Ren code, there are only floating point numbers. But we might want to
/// construct a `Number` token from a Gleam `Int` so this function just wraps up
/// the conversion for us.
///
pub fn int(int: Int) -> Token {
  num(int.to_float(int))
}

///
///
pub fn str(str: String) -> Token {
  Literal(String(str))
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn to_string(token: Token) {
  case token {
    EOF -> "EOF"

    Identifier(Upper(id)) -> id
    Identifier(Lower(id)) -> id

    Keyword(As) -> "as"
    Keyword(Assert) -> "assert"
    Keyword(Case) -> "case"
    Keyword(Else) -> "else"
    Keyword(Expect) -> "expect"
    Keyword(Ext) -> "ext"
    Keyword(Fun) -> "fun"
    Keyword(If) -> "if"
    Keyword(Import) -> "import"
    Keyword(Let) -> "let"
    Keyword(On) -> "on"
    Keyword(Pkg) -> "pkg"
    Keyword(Pub) -> "pub"
    Keyword(Switch) -> "switch"
    Keyword(Then) -> "then"
    Keyword(Type) -> "type"

    Literal(Number(num)) -> float.to_string(num)
    Literal(String(str)) -> "\"" <> str <> "\""

    Operator(Add) -> "+"
    Operator(And) -> "&"
    Operator(Div) -> "/"
    Operator(Eq) -> "=="
    Operator(Gt) -> ">"
    Operator(Gte) -> ">="
    Operator(Lt) -> "<"
    Operator(Lte) -> "<="
    Operator(Mod) -> "%"
    Operator(Mul) -> "*"
    Operator(Neq) -> "!="
    Operator(Or) -> "|"
    Operator(Pipe) -> "|>"
    Operator(Pow) -> "^"
    Operator(Sub) -> "-"

    Symbol(Arrow) -> "->"
    Symbol(Colon) -> ":"
    Symbol(Comma) -> ","
    Symbol(Dot) -> "."
    Symbol(DoubleDot) -> ".."
    Symbol(DoubleSlash) -> "//"
    Symbol(Equals) -> "="
    Symbol(LBrace) -> "{"
    Symbol(LBracket) -> "["
    Symbol(LParen) -> "("
    Symbol(Question) -> "?"
    Symbol(RBrace) -> "}"
    Symbol(RBracket) -> "]"
    Symbol(RParen) -> ")"
    Symbol(Semicolon) -> ";"
    Symbol(Underscore) -> "_"

    Unknown(char) -> char
  }
}
