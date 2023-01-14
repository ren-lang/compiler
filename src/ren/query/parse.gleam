// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/list
import gleam/option.{Option}
import ren/ast/expr.{Call,
  Expr, Fun, If, Let, Literal, Placeholder, Switch, Var}
import ren/ast/lit.{Arr, Con, Int, Lit, Num, Obj, Str}
import ren/ast/pat.{Alias, Bind, Pat, Typeof, Value, Wildcard}
import ren/ast/mod.{Dec, Mod}
import ren/data/error.{UnexpectedInput}
import ren/data/token.{Token}
import ren/parser.{
  Break, Continue, Parser, do, end, keyword, loop, lower_identifier, many, map,
  number, one_of, operator, optional, or, replace, return, string, symbol,
  then_replace, throw, upper_identifier,
}
import ren/parser/pratt
import ren/query.{Query}
import ren/query/lex
import ren/t.{Type}
import ren/util/debug

// QUERIES ---------------------------------------------------------------------

///
///
pub fn file(path: String) -> Query(Mod(Expr), env) {
  lex.file(path)
  |> query.then(run(_, parse_mod()))
}

///
///
pub fn mod(input: List(Token)) -> Query(Mod(Expr), env) {
  run(input, parse_mod())
}

///
///
pub fn dec(input: List(Token)) -> Query(Dec(Expr), env) {
  run(input, parse_dec())
}

///
///
pub fn expr(input: List(Token)) -> Query(Expr, env) {
  run(input, parse_expr())
}

fn run(tokens: List(Token), parser: Parser(a)) -> Query(a, env) {
  tokens
  |> parser.run(parser)
  |> query.from_result
}

// PARSERS ---------------------------------------------------------------------

fn parse_mod() -> Parser(Mod(Expr)) {
  use decs <- loop([])

  let dec =
    parse_dec()
    |> map(list.prepend(decs, _))
    |> map(Continue)

  let end =
    end()
    |> map(fn(_) { list.reverse(decs) })
    |> map(Break)

  one_of([dec, end])
}

// DECLARATION PARSERS ---------------------------------------------------------

fn parse_dec() -> Parser(Dec(Expr)) {
  one_of([parse_imp(), parse_let_dec(), parse_ext(), parse_typ_dec()])
}

fn parse_imp() -> Parser(Dec(Expr)) {
  use _ <- do(keyword(token.Import))
  use src <- do(parse_imp_src())
  use path <- do(string())
  use alias <- do(one_of([parse_imp_alias(), return([])]))

  return(mod.Imp(src, path, alias))
}

fn parse_imp_src() -> Parser(mod.Src) {
  let ext = replace(keyword(token.Ext), with: mod.External)
  let pkg = replace(keyword(token.Pkg), with: mod.Package)
  let prj = return(mod.Project)

  one_of([ext, pkg, prj])
}

fn parse_imp_alias() -> Parser(List(String)) {
  let rest = fn(first) {
    use segments <- loop([first])

    one_of([
      symbol(token.Dot)
      |> then_replace(with: lower_identifier())
      |> map(list.prepend(segments, _))
      |> map(Continue),
      return(list.reverse(segments))
      |> map(Break),
    ])
  }

  use _ <- do(keyword(token.As))
  use first <- do(lower_identifier())

  one_of([rest(first), return([first])])
}

fn parse_let_dec() -> Parser(Dec(Expr)) {
  use exposed <- do(parse_exposed())
  use _ <- do(keyword(token.Let))
  use name <- do(lower_identifier())
  use typ <- do(parse_annotation())
  use _ <- do(symbol(token.Equals))
  use expr <- do(parse_expr())

  return(mod.Let(exposed, name, typ, expr))
}

fn parse_ext() -> Parser(Dec(Expr)) {
  debug.crash(
    "parse.gleam",
    133,
    "External declarations are not yet implemented.",
  )
}

fn parse_typ_dec() -> Parser(Dec(Expr)) {
  debug.crash("parse.gleam", 139, "Type declarations are not yet implemented.")
}

fn parse_annotation() -> Parser(Type) {
  debug.crash("parse.gleam", 143, "Type annotations are not yet implemented.")
}

fn parse_exposed() -> Parser(Bool) {
  keyword(token.Pub)
  |> replace(with: True)
  |> or(return(False))
}

// EXPRESSION PARSERS ----------------------------------------------------------

fn parse_expr() -> Parser(Expr) {
  let binop = fn(op) { fn(lhs, rhs) { return(op(lhs, rhs)) } }
  let call = fn(fun, arg) {
    case fun {
      Call(f, args) -> return(Call(f, list.append(args, [arg])))
      _ -> return(Call(fun, [arg]))
    }
  }

  pratt.expr(
    one_of: [
      fn(parsers) { parse_parens(pratt.subexpr(0, parsers)) },
      parse_fun,
      parse_if,
      parse_let_expr,
      parse_lit,
      pratt.literal(parse_placeholder()),
      parse_switch,
      pratt.literal(parse_var()),
    ],
    then: [
      // Left associative operators.
      pratt.infixl(2, operator(token.Pipe), binop(expr.pipe)),
      pratt.infixl(4, operator(token.Eq), binop(expr.eq)),
      pratt.infixl(4, operator(token.Gt), binop(expr.gt)),
      pratt.infixl(4, operator(token.Gte), binop(expr.gte)),
      pratt.infixl(4, operator(token.Lt), binop(expr.lt)),
      pratt.infixl(4, operator(token.Lte), binop(expr.lte)),
      pratt.infixl(4, operator(token.Neq), binop(expr.neq)),
      pratt.infixl(6, operator(token.Add), binop(expr.add)),
      pratt.infixl(6, operator(token.Sub), binop(expr.sub)),
      pratt.infixl(7, operator(token.Mul), binop(expr.mul)),
      pratt.infixl(7, operator(token.Div), binop(expr.div)),
      pratt.infixl(10, return(Nil), call),
      // Right associative operators.
      pratt.infixr(1, operator(token.Seq), binop(expr.seq)),
      pratt.postfix(1, operator(token.Seq), return),
      pratt.infixr(2, operator(token.Or), binop(expr.or)),
      pratt.infixr(3, operator(token.And), binop(expr.and)),
    ],
  )
}

fn parse_fun(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.Fun))
  use pat <- do(parse_pattern())
  use pats <- do(many(parse_pattern()))
  use _ <- do(symbol(token.Arrow))
  use body <- do(pratt.subexpr(0, parsers))

  return(Fun([pat, ..pats], body))
}

fn parse_if(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.If))
  use cond <- do(pratt.subexpr(0, parsers))
  use _ <- do(keyword(token.Then))
  use then <- do(pratt.subexpr(0, parsers))
  use _ <- do(keyword(token.Else))
  use else <- do(pratt.subexpr(0, parsers))

  return(If(cond, then, else))
}

fn parse_let_expr(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.Let))
  use name <- do(lower_identifier())
  use _ <- do(symbol(token.Equals))
  use value <- do(pratt.subexpr(0, parsers))

  return(Let(Bind(name), value))
}

fn parse_lit(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  one_of([
    parse_array(parsers),
    parse_enum(),
    parse_num(),
    parse_record(parsers),
    parse_str(),
  ])
  |> map(Literal)
}

fn parse_placeholder() -> Parser(Expr) {
  use _ <- do(symbol(token.Underscore))

  return(Placeholder)
}

fn parse_switch(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.Switch))
  use expr <- do(pratt.subexpr(0, parsers))
  use _ <- do(keyword(token.On))
  use cases <- do(many(parse_case(parsers)))

  return(Switch(expr, cases))
}

fn parse_case(
  parsers: pratt.Parsers(Expr),
) -> Parser(#(Pat, Option(Expr), Expr)) {
  use _ <- do(keyword(token.Case))
  use pat <- do(parse_pattern())
  use guard <- do(optional(parse_guard(parsers)))
  use _ <- do(symbol(token.Arrow))
  use body <- do(pratt.subexpr(0, parsers))

  return(#(pat, guard, body))
}

fn parse_guard(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.If))
  use expr <- do(pratt.subexpr(0, parsers))

  return(expr)
}

fn parse_var() -> Parser(Expr) {
  lower_identifier()
  |> map(Var)
}

// LITERAL PARSERS -------------------------------------------------------------

fn parse_array(parsers: pratt.Parsers(a)) -> Parser(Lit(a)) {
  use _ <- do(symbol(token.LBracket))
  use elements <- do(or(parse_array_elements(parsers), return([])))
  use _ <- do(symbol(token.RBracket))

  return(Arr(elements))
}

fn parse_array_elements(parsers: pratt.Parsers(a)) -> Parser(List(a)) {
  use first <- do(pratt.subexpr(0, parsers))
  use rest <- loop([first])

  one_of([
    symbol(token.Comma)
    |> then_replace(with: pratt.subexpr(0, parsers))
    |> map(list.prepend(rest, _))
    |> map(Continue),
    //
    return(list.reverse(rest))
    |> map(Break),
  ])
}

// ❗️ The `Enum` parser will always succeed with a simple enum with no arguments.
// For example `:foo 1 2` will be parsed as `Enum("foo", []). We do this because
// we handle the parsing of the arguments in the `parse_call` parser as a special
// case of function application.
//
fn parse_enum() -> Parser(Lit(a)) {
  use _ <- do(symbol(token.Hash))
  use name <- do(lower_identifier())

  return(Con(name, []))
}

fn parse_num() -> Parser(Lit(a)) {
  use n <- do(number())
  let i = float.round(n)

  case int.to_float(i) == n {
    True -> return(Int(i))
    False -> return(Num(n))
  }
}

fn parse_record(parsers: pratt.Parsers(a)) -> Parser(Lit(a)) {
  use _ <- do(symbol(token.LBrace))
  use fields <- do(or(parse_record_fields(parsers), return([])))
  use _ <- do(symbol(token.RBrace))

  return(Obj(fields))
}

fn parse_record_fields(parsers: pratt.Parsers(a)) -> Parser(List(#(String, a))) {
  use first <- do(parse_record_field(parsers))
  use rest <- loop([first])

  one_of([
    symbol(token.Comma)
    |> then_replace(with: parse_record_field(parsers))
    |> map(list.prepend(rest, _))
    |> map(Continue),
    //
    return(list.reverse(rest))
    |> map(Break),
  ])
}

fn parse_record_field(parsers: pratt.Parsers(a)) -> Parser(#(String, a)) {
  use label <- do(lower_identifier())
  use _ <- do(symbol(token.Colon))
  use expr <- do(pratt.subexpr(0, parsers))

  return(#(label, expr))
}

fn parse_str() -> Parser(Lit(a)) {
  string()
  |> map(Str)
}

// PATTERN PARSERS ------------------------------------------------------------

fn parse_pattern() -> Parser(Pat) {
  pratt.expr(
    one_of: [
      pratt.literal(parse_bind()),
      parse_value,
      pratt.literal(parse_wildcard()),
      parse_typeof,
      fn(parsers) { parse_parens(pratt.subexpr(0, parsers)) },
    ],
    then: [
      pratt.infixl(2, keyword(token.As), parse_alias),
      pratt.infixr(2, return(Nil), parse_enum_pattern),
    ],
  )
}

fn parse_alias(pattern: Pat, name: Pat) -> Parser(Pat) {
  case name {
    Bind(name) -> return(Alias(pattern, name))
    _ -> throw(UnexpectedInput)
  }
}

fn parse_bind() -> Parser(Pat) {
  use name <- do(lower_identifier())

  return(Bind(name))
}

fn parse_value(parsers: pratt.Parsers(Pat)) -> Parser(Pat) {
  one_of([
    parse_array(parsers),
    parse_enum(),
    parse_num(),
    parse_record(parsers),
    parse_str(),
  ])
  |> map(Value)
}

fn parse_enum_pattern(lhs: Pat, rhs: Pat) -> Parser(Pat) {
  case lhs {
    Value(Con(name, args)) -> return(Value(Con(name, list.append(args, [rhs]))))
    _ -> throw(UnexpectedInput)
  }
}

fn parse_wildcard() -> Parser(Pat) {
  use _ <- do(symbol(token.Underscore))

  return(Wildcard)
}

fn parse_typeof(parsers: pratt.Parsers(Pat)) -> Parser(Pat) {
  use _ <- do(symbol(token.At))
  use t <- do(upper_identifier())
  use pat <- do(pratt.subexpr(0, parsers))

  return(Typeof(t, pat))
}

// UTIL PARSERS ----------------------------------------------------------------

fn parse_parens(parser: Parser(a)) -> Parser(a) {
  use _ <- do(symbol(token.LParen))
  use expr <- do(parser)
  use _ <- do(symbol(token.RParen))

  return(expr)
}
