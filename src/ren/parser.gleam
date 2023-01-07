// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/map.{Map}
import ren/data/token.{Token}

// TYPES -----------------------------------------------------------------------

///
///
/// ❗️ Because our `Parser` type is just a type alias, any types it references
/// must be public so other modules can understand the type. Our `Parser` type is
/// only an alias as a minor optimisation to avoid the overhead of a custom type
/// wrapper: you should *never* need to use or consume this `Step` type directly.
///
pub opaque type Step(a, e) {
  Good(Bool, a, State)
  Bad(Bool, Bag(e))
}

type State =
  #(Map(Int, Token), Int)

///
/// ❓ Why is this a type alias? Generally I dislike type aliases in part because
/// they leak implementation details to consumers of the type. However, Gleam
/// doesn't do automatic unboxing of so-called "newtypes" (custom types with only
/// one variant) so preferring a type alias is a tiny bit more efficient.
///
pub type Parser(a, e) =
  fn(State) -> Step(a, e)

type Bag(e) {
  Empty
  AddRight(Bag(e), e)
  Append(Bag(e), Bag(e))
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
pub fn run(tokens: List(Token), parser: Parser(a, e)) -> Result(a, e) {
  let tokens =
    list.index_fold(
      tokens,
      map.new(),
      fn(map, tok, idx) { map.insert(map, idx, tok) },
    )

  case parser(#(tokens, 0)) {
    Good(_, value, _) -> Ok(value)
    Bad(_, bag) -> {
      let errors = bag_to_list(bag, [])
      assert [error, ..] = errors
      Error(error)
    }
  }
}

// ERROR CONSTRUCTORS ----------------------------------------------------------

fn bag_to_list(bag: Bag(e), errors: List(e)) -> List(e) {
  case bag {
    Empty -> errors
    AddRight(bag, error) -> bag_to_list(bag, [error, ..errors])
    Append(left, right) -> bag_to_list(left, bag_to_list(right, errors))
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn return(value: a) -> Parser(a, e) {
  Good(False, value, _)
}

///
///
pub fn commit(value: a) -> Parser(a, e) {
  Good(True, value, _)
}

///
///
pub fn throw(error: e) -> Parser(a, e) {
  fn(_) { Bad(False, AddRight(Empty, error)) }
}

//

///
///
pub fn any(error: e) -> Parser(Token, e) {
  fn(state: State) {
    case peek(state) {
      token.EOF -> Bad(False, AddRight(Empty, error))
      token -> Good(False, token, next(state))
    }
  }
}

///
///
pub fn token(token: Token, error: e) -> Parser(Nil, e) {
  fn(state: State) {
    let next_token = peek(state)
    case next_token == token {
      True -> Good(True, Nil, next(state))
      False -> Bad(False, AddRight(Empty, error))
    }
  }
}

///
///
pub fn keyword(keyword: token.Keyword, error: e) -> Parser(Nil, e) {
  token(token.Keyword(keyword), error)
}

///
///
pub fn symbol(symbol: token.Symbol, error: e) -> Parser(Nil, e) {
  token(token.Symbol(symbol), error)
}

///
///
pub fn operator(operator: token.Operator, error: e) -> Parser(Nil, e) {
  token(token.Operator(operator), error)
}

///
///
pub fn end(error: e) -> Parser(Nil, e) {
  fn(state: State) {
    case peek(state), map.size(state.0) <= state.1 {
      token.EOF, _ | _, True -> Good(False, Nil, state)
      _, False -> Bad(False, AddRight(Empty, error))
    }
  }
}

//

///
///
pub fn lower_identifier(error: e) -> Parser(String, e) {
  fn(state: State) {
    let next_token = peek(state)
    case next_token {
      token.Identifier(token.Lower(name)) -> Good(False, name, next(state))
      _ -> Bad(False, AddRight(Empty, error))
    }
  }
}

///
///
pub fn upper_identifier(error: e) -> Parser(String, e) {
  fn(state: State) {
    case peek(state) {
      token.Identifier(token.Upper(name)) -> Good(False, name, next(state))
      _ -> Bad(False, AddRight(Empty, error))
    }
  }
}

///
///
pub fn number(error: e) -> Parser(Float, e) {
  fn(state: State) {
    case peek(state) {
      token.Literal(token.Number(num)) -> Good(False, num, next(state))
      _ -> Bad(False, AddRight(Empty, error))
    }
  }
}

///
///
pub fn string(error: e) -> Parser(String, e) {
  fn(state: State) {
    case peek(state) {
      token.Literal(token.String(str)) -> Good(False, str, next(state))
      _ -> Bad(False, AddRight(Empty, error))
    }
  }
}

//

///
///
pub fn do(parser: Parser(a, e), k: fn(a) -> Parser(b, e)) -> Parser(b, e) {
  fn(state: State) {
    case parser(state) {
      Bad(commit, error) -> Bad(commit, error)
      Good(c1, a, state) ->
        case k(a)(state) {
          Bad(c2, bag) -> Bad(c1 || c2, bag)
          Good(c2, b, state) -> Good(c1 || c2, b, state)
        }
    }
  }
}

///
///
pub fn then(parser: Parser(a, e), k: fn(a) -> Parser(b, e)) -> Parser(b, e) {
  do(parser, k)
}

///
///
pub fn map(parser: Parser(a, e), f: fn(a) -> b) -> Parser(b, e) {
  use a <- do(parser)
  return(f(a))
}

///
///
pub fn replace(parser: Parser(a, e), with val: b) -> Parser(b, e) {
  use _ <- do(parser)
  return(val)
}

///
///
pub fn then_replace(
  parser: Parser(a, e),
  with other: Parser(b, e),
) -> Parser(b, e) {
  use _ <- do(parser)
  other
}

///
///
pub fn or(parser: Parser(a, e), other: Parser(a, e)) -> Parser(a, e) {
  one_of([parser, other])
}

//

///
///
pub fn lazy(parser: fn() -> Parser(a, e)) -> Parser(a, e) {
  fn(state: State) { parser()(state) }
}

///
///
pub fn backtrackable(parser: Parser(a, e)) -> Parser(a, e) {
  fn(state: State) {
    case parser(state) {
      Bad(_, error) -> Bad(False, error)
      Good(_, a, state) -> Good(False, a, state)
    }
  }
}

//

///
///
pub fn one_of(parsers: List(Parser(a, e))) -> Parser(a, e) {
  fn(state: State) { do_one_of(state, Empty, parsers) }
}

fn do_one_of(
  state: State,
  bag: Bag(e),
  parsers: List(Parser(a, e)),
) -> Step(a, e) {
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
pub fn loop(
  init: state,
  k: fn(state) -> Parser(Loop(a, state), e),
) -> Parser(a, e) {
  fn(state: State) { do_loop(False, init, state, k) }
}

fn do_loop(
  c1: Bool,
  acc: state,
  state: State,
  k: fn(state) -> Parser(Loop(a, state), e),
) -> Step(a, e) {
  case k(acc)(state) {
    Good(c2, Continue(acc), state) -> do_loop(c1 || c2, acc, state, k)
    Good(c2, Break(a), state) -> Good(c1 || c2, a, state)
    Bad(c2, error) -> Bad(c1 || c2, error)
  }
}

///
///
pub fn many(parser: Parser(a, e)) -> Parser(List(a), e) {
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
