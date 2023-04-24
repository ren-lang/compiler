//// This module is a pretty faithful port of Elm's great parser combinator package.
//// Most of the changes are just to make it work with a stream of tokens rather than
//// a string directly. You can find the original source and docs here:
//// 
////   https://github.com/elm/parser/tree/1.1.0
////
//// The original package is licensed under the BSD 3-Clause License, reproduced
//// below:
////
////   Copyright (c) 2017-present, Evan Czaplicki
////   All rights reserved.
////   
////   Redistribution and use in source and binary forms, with or without
////   modification, are permitted provided that the following conditions are met:
////   
////   * Redistributions of source code must retain the above copyright notice, this
////     list of conditions and the following disclaimer.
////   
////   * Redistributions in binary form must reproduce the above copyright notice,
////     this list of conditions and the following disclaimer in the documentation
////     and/or other materials provided with the distribution.
////   
////   * Neither the name of the {organization} nor the names of its
////     contributors may be used to endorse or promote products derived from
////     this software without specific prior written permission.
////   
////   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
////   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
////   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
////   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
////   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
////   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
////   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
////   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
////   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
////   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{None, Option, Some}
import ren/data/error.{Error, ParseError}
import ren/data/token.{Token, TokenT}

// TYPES -----------------------------------------------------------------------

///
///
/// ❗️ Because our `Parser` type is just a type alias, any types it references
/// must be public so other modules can understand the type. Our `Parser` type is
/// only an alias as a minor optimisation to avoid the overhead of a custom type
/// wrapper: you should *never* need to use or consume this `Step` type directly.
///
pub opaque type Step(a) {
  Good(Bool, a, List(Token))
  Bad(Bool, Bag)
}

/// A `Parser` is actually nothing special, it's just a function! Specifically,
/// it's a function that takes the current parser `tokens` and returns the next
/// parser `Step` (either a succeed indicating we should carry on or a failure
/// telling us to stop or try another path). 
///
/// ❓ Why is this a type alias? Generally I dislike type aliases in part because
/// they leak implementation details to consumers of the type. However, Gleam
/// doesn't do automatic unboxing of so-called "newtypes" (custom types with only
/// one variant) so preferring a type alias is a tiny bit more efficient.
///
pub type Parser(a) =
  fn(List(Token)) -> Step(a)

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
  case parser(tokens) {
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
pub fn any() -> Parser(TokenT) {
  fn(tokens) {
    case peek(tokens) {
      token -> Good(False, token, next(tokens))
      token.EOF -> Bad(False, AddRight(Empty, error.UnexpectedEOF))
    }
  }
}

///
///
pub fn token(token: TokenT) -> Parser(Nil) {
  fn(tokens) {
    let next_token = peek(tokens)
    case next_token == token {
      True -> Good(True, Nil, next(tokens))
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
  fn(tokens) {
    case peek(tokens) {
      token.EOF -> Good(False, Nil, tokens)
      token ->
        Bad(False, AddRight(Empty, error.Expected(token.EOF, got: token)))
    }
  }
}

//

///
///
pub fn lower_identifier() -> Parser(String) {
  fn(tokens) {
    let next_token = peek(tokens)
    case next_token {
      token.Identifier(token.Lower(name)) -> Good(False, name, next(tokens))
      token ->
        Bad(False, AddRight(Empty, error.ExpectedLowerIdentifier(got: token)))
    }
  }
}

///
///
pub fn upper_identifier() -> Parser(String) {
  fn(tokens) {
    case peek(tokens) {
      token.Identifier(token.Upper(name)) -> Good(False, name, next(tokens))
      token ->
        Bad(False, AddRight(Empty, error.ExpectedUpperIdentifier(got: token)))
    }
  }
}

///
///
pub fn number() -> Parser(Float) {
  fn(tokens) {
    case peek(tokens) {
      token.Literal(token.Number(num)) -> Good(False, num, next(tokens))
      token -> Bad(False, AddRight(Empty, error.ExpectedNumber(got: token)))
    }
  }
}

///
///
pub fn string() -> Parser(String) {
  fn(tokens) {
    case peek(tokens) {
      token.Literal(token.String(str)) -> Good(False, str, next(tokens))
      token -> Bad(False, AddRight(Empty, error.ExpectedString(got: token)))
    }
  }
}

//

///
///
pub fn do(parser: Parser(a), k: fn(a) -> Parser(b)) -> Parser(b) {
  fn(tokens) {
    case parser(tokens) {
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
      Good(_, a, tokens) -> k(a)(tokens)
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
  fn(tokens) { parser()(tokens) }
}

///
///
pub fn backtrackable(parser: Parser(a)) -> Parser(a) {
  fn(tokens) {
    case parser(tokens) {
      Bad(_, error) -> Bad(False, error)
      Good(_, a, tokens) -> Good(False, a, tokens)
    }
  }
}

//

///
///
pub fn one_of(parsers: List(Parser(a))) -> Parser(a) {
  fn(tokens) { do_one_of(tokens, Empty, parsers) }
}

fn do_one_of(tokens: List(Token), bag: Bag, parsers: List(Parser(a))) -> Step(a) {
  case parsers {
    [] -> Bad(False, bag)
    [parser, ..parsers] ->
      case parser(tokens) {
        Bad(commit, _) as step if commit -> step
        Bad(_, error) -> do_one_of(tokens, Append(bag, error), parsers)
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
pub fn loop(init: tokens, k: fn(tokens) -> Parser(Loop(a, tokens))) -> Parser(a) {
  fn(tokens) { do_loop(False, init, tokens, k) }
}

fn do_loop(
  c1: Bool,
  acc: tokens,
  tokens: List(Token),
  k: fn(tokens) -> Parser(Loop(a, tokens)),
) -> Step(a) {
  case k(acc)(tokens) {
    Good(c2, Continue(acc), tokens) -> do_loop(c1 || c2, acc, tokens, k)
    Good(c2, Break(a), tokens) -> Good(c1 || c2, a, tokens)
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

fn peek(tokens: List(Token)) -> TokenT {
  case tokens {
    [Token(_, _, _, _, _, token), ..] -> token
    [] -> token.EOF
  }
}

fn next(tokens: List(Token)) -> List(Token) {
  case tokens {
    [_, ..tokens] -> tokens
    [] -> []
  }
}
