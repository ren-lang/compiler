// IMPORTS ---------------------------------------------------------------------

// 
import control/parser.{
  Break, Continue, Parser, do, end, keyword, loop, lower_identifier, many, map,
  number, one_of, operator, or, replace, return, string, symbol, then_replace,
  throw,
}
import control/parser/pratt
import gleam/list
import gleam/function
import ren/ast/expr.{Expr}
import ren/ast/mod.{Dec, Mod}
import ren/data/token.{Token}
import ren/data/error.{Error, InternalError}
import ren/t.{Type}
import ren/query.{Query}
import ren/query/lex

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

fn run(tokens: List(Token), parser: Parser(a, Error)) -> Query(a, env) {
  tokens
  |> parser.run(parser)
  |> query.from_result
}

// PARSERS ---------------------------------------------------------------------

fn parse_mod() -> Parser(Mod(Expr), Error) {
  use decs <- loop([])

  let dec =
    parse_dec()
    |> map(list.prepend(decs, _))
    |> map(Continue)

  let end =
    end(InternalError("end"))
    |> map(fn(_) { list.reverse(decs) })
    |> map(Break)

  one_of([dec, end])
}

// DECLARATION PARSERS ---------------------------------------------------------

fn parse_dec() -> Parser(Dec(Expr), Error) {
  one_of([parse_imp(), parse_let_dec(), parse_ext(), parse_typ_dec()])
}

fn parse_imp() -> Parser(Dec(Expr), Error) {
  use _ <- do(keyword(token.Import, InternalError("import")))
  use src <- do(parse_imp_src())
  use path <- do(string(InternalError("string")))
  use alias <- do(one_of([parse_imp_alias(), return([])]))

  return(mod.Imp(src, path, alias))
}

fn parse_imp_src() -> Parser(mod.Src, Error) {
  let ext =
    replace(keyword(token.Ext, InternalError("ext")), with: mod.External)
  let pkg = replace(keyword(token.Pkg, InternalError("pkg")), with: mod.Package)
  let prj = return(mod.Project)

  one_of([ext, pkg, prj])
}

fn parse_imp_alias() -> Parser(List(String), Error) {
  let rest = fn(first) {
    use segments <- loop([first])

    one_of([
      symbol(token.Dot, InternalError("."))
      |> then_replace(with: lower_identifier(InternalError("lower id")))
      |> map(list.prepend(segments, _))
      |> map(Continue),
      return(list.reverse(segments))
      |> map(Break),
    ])
  }

  use _ <- do(keyword(token.As, InternalError("as")))
  use first <- do(lower_identifier(InternalError("lower id")))

  one_of([rest(first), return([first])])
}

fn parse_let_dec() -> Parser(Dec(Expr), Error) {
  use exposed <- do(parse_exposed())
  use _ <- do(keyword(token.Let, InternalError("let")))
  use name <- do(lower_identifier(InternalError("lower identifier")))
  use typ <- do(parse_annotation())
  use _ <- do(symbol(token.Equals, InternalError("=")))
  use expr <- do(parse_expr())

  return(mod.Let(exposed, name, typ, expr))
}

fn parse_ext() -> Parser(Dec(Expr), Error) {
  throw(InternalError("not implemented"))
}

fn parse_typ_dec() -> Parser(Dec(Expr), Error) {
  throw(InternalError("not implemented"))
}

fn parse_annotation() -> Parser(Type, Error) {
  symbol(token.Colon, InternalError(":"))
  |> then_replace(with: parse_typ())
  |> or(return(t.any))
}

fn parse_exposed() -> Parser(Bool, Error) {
  keyword(token.Pub, InternalError("pub"))
  |> replace(with: True)
  |> or(return(False))
}

// EXPRESSION PARSERS ----------------------------------------------------------

fn parse_expr() -> Parser(Expr, Error) {
  pratt.expr(
    one_of: [parse_lam, parse_let_expr, parse_app],
    then: [
      pratt.infixl(2, operator(token.Pipe, InternalError("|>")), expr.pipe),
      pratt.infixl(4, operator(token.Eq, InternalError("==")), expr.eq),
      pratt.infixl(4, operator(token.Gt, InternalError(">")), expr.gt),
      pratt.infixl(4, operator(token.Gte, InternalError(">=")), expr.gte),
      pratt.infixl(4, operator(token.Lt, InternalError("<")), expr.lt),
      pratt.infixl(4, operator(token.Lte, InternalError("<=")), expr.lte),
      pratt.infixl(4, operator(token.Neq, InternalError("!=")), expr.neq),
      pratt.infixl(6, operator(token.Add, InternalError("+")), expr.add),
      pratt.infixl(6, operator(token.Sub, InternalError("-")), expr.sub),
      pratt.infixl(7, operator(token.Mul, InternalError("*")), expr.mul),
      pratt.infixl(7, operator(token.Div, InternalError("/")), expr.div),
      //
      pratt.infixr(1, symbol(token.Semicolon, InternalError(";")), expr.seq),
      pratt.infixr(2, operator(token.Or, InternalError("|")), expr.or),
      pratt.infixr(3, operator(token.And, InternalError("&")), expr.and),
    ],
  )
}

fn parse_app(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  use fun <- do(parse_callables(parsers))
  use args <- do(many(parse_callables(parsers)))

  return(expr.app(fun, args))
}

fn parse_callables(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  one_of([parse_parens(parsers), parse_lit(parsers), parse_var()])
}

fn parse_parens(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  use _ <- do(symbol(token.LParen, InternalError("(")))
  use expr <- do(pratt.subexpr(0, parsers))
  use _ <- do(symbol(token.RParen, InternalError(")")))

  return(expr)
}

fn parse_lam(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  use _ <- do(keyword(token.Fun, InternalError("fun")))
  use name <- do(lower_identifier(InternalError("lower identifier")))
  use names <- do(many(lower_identifier(InternalError("lower identifier"))))
  use _ <- do(symbol(token.Arrow, InternalError("->")))
  use body <- do(pratt.subexpr(0, parsers))

  return(expr.Lam(name, list.fold_right(names, body, function.flip(expr.Lam))))
}

fn parse_let_expr(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  use _ <- do(keyword(token.Let, InternalError("let")))
  use name <- do(lower_identifier(InternalError("lower identifier")))
  use _ <- do(symbol(token.Equals, InternalError("=")))
  use value <- do(pratt.subexpr(0, parsers))
  use _ <- do(symbol(token.Semicolon, InternalError(";")))
  use body <- do(pratt.subexpr(0, parsers))

  return(expr.Let(name, value, body))
}

fn parse_lit(parsers: pratt.Parsers(Expr, Error)) -> Parser(Expr, Error) {
  one_of([parse_array(parsers), parse_num(), parse_record(parsers), parse_str()])
  |> map(expr.Lit)
}

fn parse_array(parsers: pratt.Parsers(Expr, Error)) -> Parser(expr.Lit, Error) {
  use _ <- do(symbol(token.LBracket, InternalError("[")))
  use elements <- do(or(parse_array_elements(parsers), return([])))
  use _ <- do(symbol(token.RBracket, InternalError("]")))

  return(expr.Array(elements))
}

fn parse_array_elements(
  parsers: pratt.Parsers(Expr, Error),
) -> Parser(List(Expr), Error) {
  use first <- do(pratt.subexpr(0, parsers))
  use rest <- loop([first])

  one_of([
    symbol(token.Comma, InternalError(","))
    |> then_replace(with: pratt.subexpr(0, parsers))
    |> map(list.prepend(rest, _))
    |> map(Continue),
    //
    return(list.reverse(rest))
    |> map(Break),
  ])
}

fn parse_num() -> Parser(expr.Lit, Error) {
  number(InternalError("number"))
  |> map(expr.Num)
}

fn parse_record(parsers: pratt.Parsers(Expr, Error)) -> Parser(expr.Lit, Error) {
  use _ <- do(symbol(token.LBrace, InternalError("{")))
  use fields <- do(or(parse_record_fields(parsers), return([])))
  use _ <- do(symbol(token.RBrace, InternalError("}")))

  return(expr.Record(fields))
}

fn parse_record_fields(
  parsers: pratt.Parsers(Expr, Error),
) -> Parser(List(#(String, Expr)), Error) {
  use first <- do(parse_record_field(parsers))
  use rest <- loop([first])

  one_of([
    symbol(token.Comma, InternalError(","))
    |> then_replace(with: parse_record_field(parsers))
    |> map(list.prepend(rest, _))
    |> map(Continue),
    //
    return(list.reverse(rest))
    |> map(Break),
  ])
}

fn parse_record_field(
  parsers: pratt.Parsers(Expr, Error),
) -> Parser(#(String, Expr), Error) {
  use label <- do(lower_identifier(InternalError("lower identifier")))
  use _ <- do(symbol(token.Colon, InternalError(":")))
  use expr <- do(pratt.subexpr(0, parsers))

  return(#(label, expr))
}

fn parse_str() -> Parser(expr.Lit, Error) {
  string(InternalError("string"))
  |> map(expr.Str)
}

fn parse_var() -> Parser(Expr, Error) {
  lower_identifier(InternalError("lower identifier"))
  |> map(expr.Var)
}

fn parse_typ() -> Parser(Type, Error) {
  return(t.any)
}
