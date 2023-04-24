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
  Token(
    start_row: Int,
    start_col: Int,
    end_row: Int,
    end_col: Int,
    lexeme: String,
    token: TokenT,
  )
}

///
///
pub type TokenT {
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
  Forall
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
  At
  Colon
  Comma
  Dot
  DoubleDot
  DoubleSlash
  Equals
  Hash
  LBrace
  LBracket
  LParen
  Question
  RBrace
  RBracket
  RParen
  Underscore
}

///
///
pub type Operator {
  Add
  And
  Concat
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
  Seq
  Sub
}

// CONSTANTS -------------------------------------------------------------------

pub const as_: TokenT = Keyword(As)

pub const assert_: TokenT = Keyword(Assert)

pub const case_: TokenT = Keyword(Case)

pub const else: TokenT = Keyword(Else)

pub const expect: TokenT = Keyword(Expect)

pub const ext: TokenT = Keyword(Ext)

pub const forall: TokenT = Keyword(Forall)

pub const fun: TokenT = Keyword(Fun)

pub const if_: TokenT = Keyword(If)

pub const import_: TokenT = Keyword(Import)

pub const let_: TokenT = Keyword(Let)

pub const on: TokenT = Keyword(On)

pub const pkg: TokenT = Keyword(Pkg)

pub const pub_: TokenT = Keyword(Pub)

pub const switch: TokenT = Keyword(Switch)

pub const then: TokenT = Keyword(Then)

pub const type_: TokenT = Keyword(Type)

pub const arrow: TokenT = Symbol(Arrow)

pub const at: TokenT = Symbol(At)

pub const colon: TokenT = Symbol(Colon)

pub const comma: TokenT = Symbol(Comma)

pub const dot: TokenT = Symbol(Dot)

pub const double_dot: TokenT = Symbol(DoubleDot)

pub const double_slash: TokenT = Symbol(DoubleSlash)

pub const equals: TokenT = Symbol(Equals)

pub const hash: TokenT = Symbol(Hash)

pub const lbrace: TokenT = Symbol(LBrace)

pub const lbracket: TokenT = Symbol(LBracket)

pub const lparen: TokenT = Symbol(LParen)

pub const question: TokenT = Symbol(Question)

pub const rbrace: TokenT = Symbol(RBrace)

pub const rbracket: TokenT = Symbol(RBracket)

pub const rparen: TokenT = Symbol(RParen)

pub const underscore: TokenT = Symbol(Underscore)

pub const add: TokenT = Operator(Add)

pub const concat: TokenT = Operator(Concat)

pub const sub: TokenT = Operator(Sub)

pub const mul: TokenT = Operator(Mul)

pub const div: TokenT = Operator(Div)

pub const mod: TokenT = Operator(Mod)

pub const pow: TokenT = Operator(Pow)

pub const and: TokenT = Operator(And)

pub const or: TokenT = Operator(Or)

pub const eq: TokenT = Operator(Eq)

pub const neq: TokenT = Operator(Neq)

pub const lt: TokenT = Operator(Lt)

pub const lte: TokenT = Operator(Lte)

pub const gt: TokenT = Operator(Gt)

pub const gte: TokenT = Operator(Gte)

pub const pipe: TokenT = Operator(Pipe)

pub const seq: TokenT = Operator(Seq)

// CONSTRUCTORS ----------------------------------------------------------------

/// Take some alphanumeric string and (potentially) convert it into either an
/// `Upper` or `Lower` identifier token based on casing. Invalid identifiers
/// will return `None`.
///
pub fn identifier(name: String) -> Option(TokenT) {
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
pub fn upper(name: String) -> TokenT {
  let assert Ok(first) = string.first(name)

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
pub fn lower(name: String) -> TokenT {
  let assert Ok(first) = string.first(name)

  // This matching is intentionally not exhaustive: we want the program to crash
  // if an unexpected string was passed in.
  case <<first:utf8>> {
    <<code:int>> if code > 96 && code < 123 -> Identifier(Lower(name))
  }
}

///
///
pub fn num(num: Float) -> TokenT {
  Literal(Number(num))
}

/// In Ren code, there are only floating point numbers. But we might want to
/// construct a `Number` token from a Gleam `Int` so this function just wraps up
/// the conversion for us.
///
pub fn int(int: Int) -> TokenT {
  num(int.to_float(int))
}

///
///
pub fn str(str: String) -> TokenT {
  Literal(String(str))
}

// CONVERSIONS -----------------------------------------------------------------

///
///
pub fn to_string(token: TokenT) {
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
    Keyword(Forall) -> "forall"
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
    Operator(Concat) -> "<>"
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
    Operator(Seq) -> ";"
    Operator(Sub) -> "-"

    Symbol(Arrow) -> "->"
    Symbol(At) -> "@"
    Symbol(Colon) -> ":"
    Symbol(Comma) -> ","
    Symbol(Dot) -> "."
    Symbol(DoubleDot) -> ".."
    Symbol(DoubleSlash) -> "//"
    Symbol(Equals) -> "="
    Symbol(Hash) -> "#"
    Symbol(LBrace) -> "{"
    Symbol(LBracket) -> "["
    Symbol(LParen) -> "("
    Symbol(Question) -> "?"
    Symbol(RBrace) -> "}"
    Symbol(RBracket) -> "]"
    Symbol(RParen) -> ")"
    Symbol(Underscore) -> "_"

    Unknown(char) -> char
  }
}
