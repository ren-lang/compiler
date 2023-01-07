// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import ren/data/error.{Error, ParseError}
import ren/data/token.{Token}

// TYPES -----------------------------------------------------------------------

///
///
/// ❗️ Because our `Parser` type is just a type alias, any types it references
/// must be public so other modules can understand the type. Our `Parser` type is
/// only an alias as a minor optimisation to avoid the overhead of a custom type
/// wrapper: you should *never* need to use or consume this `Step` type directly.
///
pub opaque type Step(a) {
  Good(Bool, a, State)
  Bad(Bool, Bag)
}

type State =
  #(Map(Int, Token), Int)

/// A `Parser` is actually nothing special, it's just a function! Specifically,
/// it's a function that takes the current parser `State` and returns the next
/// parser `Step` (either a succeed indicating we should carry on or a failure
/// telling us to stop or try another path). 
///
/// ❓ Why is this a type alias? Generally I dislike type aliases in part because
/// they leak implementation details to consumers of the type. However, Gleam
/// doesn't do automatic unboxing of so-called "newtypes" (custom types with only
/// one variant) so preferring a type alias is a tiny bit more efficient.
///
pub type Parser(a) =
  fn(State) -> Step(a)

/// A `Bag` acts as a more efficient way to build up a list of errors. Crucially,
/// this defers the concatenation of lists until the very end, when the parser
/// finishes running. 
///
/// This seems like a small optimisation, but parsing is often a bottleneck in
/// compilers, so we try and take any perf wins we can get.
///
type Bag {
  Empty
  AddRight(Bag, ParseError)
  Append(Bag, Bag)
}

///
///
pub type Loop(a, b) {
  Continue(b)
  Break(a)
}

// RUNNING A PARSER ------------------------------------------------------------

///
///
pub fn run(tokens: List(Token), parser: Parser(a)) -> Result(a, Error) {
  let tokens =
    list.index_fold(
      tokens,
      map.new(),
      fn(map, tok, idx) { map.insert(map, idx, tok) },
    )

  case parser(#(tokens, 0)) {
    Good(_, value, _) -> Ok(value)
    Bad(_, bag) ->
      bag_to_list(bag, [])
      |> list.map(error.ParseError)
      |> error.MultipleErrors
      |> Error
  }
}

// ERROR CONSTRUCTORS ----------------------------------------------------------

fn bag_to_list(bag: Bag, errors: List(ParseError)) -> List(ParseError) {
  case bag {
    Empty -> errors
    AddRight(bag, error) -> bag_to_list(bag, [error, ..errors])
    Append(left, right) -> bag_to_list(left, bag_to_list(right, errors))
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn return(value: a) -> Parser(a) {
  Good(False, value, _)
}

///
///
pub fn commit(value: a) -> Parser(a) {
  Good(True, value, _)
}

///
///
pub fn throw(error: ParseError) -> Parser(a) {
  fn(_) { Bad(False, AddRight(Empty, error)) }
}

//

///
///
pub fn any() -> Parser(Token) {
  fn(state) {
    case peek(state) {
      token.EOF -> Bad(False, AddRight(Empty, error.UnexpectedEOF))
      token -> Good(False, token, next(state))
    }
  }
}

///
///
pub fn token(token: Token) -> Parser(Nil) {
  fn(state) {
    let next_token = peek(state)
    case next_token == token {
      True -> Good(True, Nil, next(state))
      False ->
        Bad(False, AddRight(Empty, error.Expected(token, got: next_token)))
    }
  }
}

///
///
pub fn keyword(keyword: token.Keyword) -> Parser(Nil) {
  token(token.Keyword(keyword))
}

///
///
pub fn symbol(symbol: token.Symbol) -> Parser(Nil) {
  token(token.Symbol(symbol))
}

///
///
pub fn operator(operator: token.Operator) -> Parser(Nil) {
  token(token.Operator(operator))
}

///
///
pub fn end() -> Parser(Nil) {
  fn(state) {
    case peek(state), map.size(state.0) <= state.1 {
      token.EOF, _ | _, True -> Good(False, Nil, state)
      token, False ->
        Bad(False, AddRight(Empty, error.Expected(token.EOF, got: token)))
    }
  }
}

//

///
///
pub fn lower_identifier() -> Parser(String) {
  fn(state) {
    let next_token = peek(state)
    case next_token {
      token.Identifier(token.Lower(name)) -> Good(False, name, next(state))
      token ->
        Bad(False, AddRight(Empty, error.ExpectedLowerIdentifier(got: token)))
    }
  }
}

///
///
pub fn upper_identifier() -> Parser(String) {
  fn(state) {
    case peek(state) {
      token.Identifier(token.Upper(name)) -> Good(False, name, next(state))
      token ->
        Bad(False, AddRight(Empty, error.ExpectedUpperIdentifier(got: token)))
    }
  }
}

///
///
pub fn number() -> Parser(Float) {
  fn(state) {
    case peek(state) {
      token.Literal(token.Number(num)) -> Good(False, num, next(state))
      token -> Bad(False, AddRight(Empty, error.ExpectedNumber(got: token)))
    }
  }
}

///
///
pub fn string() -> Parser(String) {
  fn(state) {
    case peek(state) {
      token.Literal(token.String(str)) -> Good(False, str, next(state))
      token -> Bad(False, AddRight(Empty, error.ExpectedString(got: token)))
    }
  }
}

//

///
///
pub fn do(parser: Parser(a), k: fn(a) -> Parser(b)) -> Parser(b) {
  fn(state) {
    case parser(state) {
      Bad(commit, error) -> Bad(commit, error)
      // NOTE: The original implementation pattern matched on the result of the
      // parser, and then used boolaen OR to determine whether to commit or not.
      //
      // Removing that means this parser is tail-recursive, which is important
      // for not exploding on large inputs, but it might do Wrong Things as a
      // result.
      //
      // I'm leaving this comment here in case I need to come back and revert to
      // the original implementation.
      Good(_, a, state) -> k(a)(state)
    }
  }
}

///
///
pub fn then(parser: Parser(a), k: fn(a) -> Parser(b)) -> Parser(b) {
  do(parser, k)
}

///
///
pub fn map(parser: Parser(a), f: fn(a) -> b) -> Parser(b) {
  use a <- do(parser)
  return(f(a))
}

///
///
pub fn replace(parser: Parser(a), with val: b) -> Parser(b) {
  use _ <- do(parser)
  return(val)
}

///
///
pub fn then_replace(parser: Parser(a), with other: Parser(b)) -> Parser(b) {
  use _ <- do(parser)
  other
}

///
///
pub fn or(parser: Parser(a), other: Parser(a)) -> Parser(a) {
  one_of([parser, other])
}

//

///
///
pub fn lazy(parser: fn() -> Parser(a)) -> Parser(a) {
  fn(state) { parser()(state) }
}

///
///
pub fn backtrackable(parser: Parser(a)) -> Parser(a) {
  fn(state) {
    case parser(state) {
      Bad(_, error) -> Bad(False, error)
      Good(_, a, state) -> Good(False, a, state)
    }
  }
}

//

///
///
pub fn one_of(parsers: List(Parser(a))) -> Parser(a) {
  fn(state) { do_one_of(state, Empty, parsers) }
}

fn do_one_of(state: State, bag: Bag, parsers: List(Parser(a))) -> Step(a) {
  case parsers {
    [] -> Bad(False, bag)
    [parser, ..parsers] ->
      case parser(state) {
        Bad(commit, _) as step if commit -> step
        Bad(_, error) -> do_one_of(state, Append(bag, error), parsers)
        Good(_, _, _) as step -> step
      }
  }
}

///
///
pub fn optional(parser: Parser(a)) -> Parser(Option(a)) {
  one_of([map(parser, Some), return(None)])
}

///
///
pub fn loop(init: state, k: fn(state) -> Parser(Loop(a, state))) -> Parser(a) {
  fn(state) { do_loop(False, init, state, k) }
}

fn do_loop(
  c1: Bool,
  acc: state,
  state: State,
  k: fn(state) -> Parser(Loop(a, state)),
) -> Step(a) {
  case k(acc)(state) {
    Good(c2, Continue(acc), state) -> do_loop(c1 || c2, acc, state, k)
    Good(c2, Break(a), state) -> Good(c1 || c2, a, state)
    Bad(c2, error) -> Bad(c1 || c2, error)
  }
}

///
///
pub fn many(parser: Parser(a)) -> Parser(List(a)) {
  use xs <- loop([])
  one_of([
    map(parser, fn(x) { Continue([x, ..xs]) }),
    map(return(xs), fn(xs) { Break(list.reverse(xs)) }),
  ])
}

// UTILS -----------------------------------------------------------------------

fn peek(state: State) -> Token {
  case map.get(state.0, state.1) {
    Ok(token) -> token
    Error(_) -> token.EOF
  }
}

fn next(state: State) -> State {
  #(state.0, state.1 + 1)
}
