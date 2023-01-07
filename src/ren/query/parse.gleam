// IMPORTS ---------------------------------------------------------------------

// 
import gleam/function
import gleam/list
import ren/ast/expr.{Expr}
import ren/ast/mod.{Dec, Mod}
import ren/data/token.{Token}
import ren/parser.{
  Break, Continue, Parser, do, end, keyword, loop, lower_identifier, many, map,
  number, one_of, operator, or, replace, return, string, symbol, then_replace,
}
import ren/parser/pratt
import ren/query.{Query}
import ren/query/lex
import ren/t.{Type}
import util

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
  util.crash("External declarations are not yet implemented.")
}

fn parse_typ_dec() -> Parser(Dec(Expr)) {
  util.crash("Type declarations are not yet implemented.")
}

fn parse_annotation() -> Parser(Type) {
  symbol(token.Colon)
  |> then_replace(with: parse_typ())
  |> or(return(t.any))
}

fn parse_exposed() -> Parser(Bool) {
  keyword(token.Pub)
  |> replace(with: True)
  |> or(return(False))
}

// EXPRESSION PARSERS ----------------------------------------------------------

fn parse_expr() -> Parser(Expr) {
  pratt.expr(
    one_of: [parse_lam, parse_let_expr, parse_app],
    then: [
      pratt.infixl(2, operator(token.Pipe), expr.pipe),
      pratt.infixl(4, operator(token.Eq), expr.eq),
      pratt.infixl(4, operator(token.Gt), expr.gt),
      pratt.infixl(4, operator(token.Gte), expr.gte),
      pratt.infixl(4, operator(token.Lt), expr.lt),
      pratt.infixl(4, operator(token.Lte), expr.lte),
      pratt.infixl(4, operator(token.Neq), expr.neq),
      pratt.infixl(6, operator(token.Add), expr.add),
      pratt.infixl(6, operator(token.Sub), expr.sub),
      pratt.infixl(7, operator(token.Mul), expr.mul),
      pratt.infixl(7, operator(token.Div), expr.div),
      //
      pratt.infixr(1, symbol(token.Semicolon), expr.seq),
      pratt.infixr(2, operator(token.Or), expr.or),
      pratt.infixr(3, operator(token.And), expr.and),
    ],
  )
}

fn parse_app(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use fun <- do(parse_callables(parsers))
  use args <- do(many(parse_callables(parsers)))

  return(expr.app(fun, args))
}

fn parse_callables(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  one_of([parse_parens(parsers), parse_lit(parsers), parse_var()])
}

fn parse_parens(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(symbol(token.LParen))
  use expr <- do(pratt.subexpr(0, parsers))
  use _ <- do(symbol(token.RParen))

  return(expr)
}

fn parse_lam(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.Fun))
  use name <- do(lower_identifier())
  use names <- do(many(lower_identifier()))
  use _ <- do(symbol(token.Arrow))
  use body <- do(pratt.subexpr(0, parsers))

  return(expr.Lam(name, list.fold_right(names, body, function.flip(expr.Lam))))
}

fn parse_let_expr(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  use _ <- do(keyword(token.Let))
  use name <- do(lower_identifier())
  use _ <- do(symbol(token.Equals))
  use value <- do(pratt.subexpr(0, parsers))
  use _ <- do(symbol(token.Semicolon))
  use body <- do(pratt.subexpr(0, parsers))

  return(expr.Let(name, value, body))
}

fn parse_lit(parsers: pratt.Parsers(Expr)) -> Parser(Expr) {
  one_of([parse_array(parsers), parse_num(), parse_record(parsers), parse_str()])
  |> map(expr.Lit)
}

fn parse_array(parsers: pratt.Parsers(Expr)) -> Parser(expr.Lit) {
  use _ <- do(symbol(token.LBracket))
  use elements <- do(or(parse_array_elements(parsers), return([])))
  use _ <- do(symbol(token.RBracket))

  return(expr.Array(elements))
}

fn parse_array_elements(parsers: pratt.Parsers(Expr)) -> Parser(List(Expr)) {
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

fn parse_num() -> Parser(expr.Lit) {
  number()
  |> map(expr.Num)
}

fn parse_record(parsers: pratt.Parsers(Expr)) -> Parser(expr.Lit) {
  use _ <- do(symbol(token.LBrace))
  use fields <- do(or(parse_record_fields(parsers), return([])))
  use _ <- do(symbol(token.RBrace))

  return(expr.Record(fields))
}

fn parse_record_fields(
  parsers: pratt.Parsers(Expr),
) -> Parser(List(#(String, Expr))) {
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

fn parse_record_field(parsers: pratt.Parsers(Expr)) -> Parser(#(String, Expr)) {
  use label <- do(lower_identifier())
  use _ <- do(symbol(token.Colon))
  use expr <- do(pratt.subexpr(0, parsers))

  return(#(label, expr))
}

fn parse_str() -> Parser(expr.Lit) {
  string()
  |> map(expr.Str)
}

fn parse_var() -> Parser(Expr) {
  lower_identifier()
  |> map(expr.Var)
}

fn parse_typ() -> Parser(Type) {
  return(t.any)
}
