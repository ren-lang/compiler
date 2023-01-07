// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/list
import gleam/int
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/string
import ren/data/token.{Token}
import ren/query.{Query}

// TYPES -----------------------------------------------------------------------

type State {
  State(
    chars: List(String),
    stack: List(String),
    tokens: List(Token),
    row: Int,
    col: Int,
  )
}

// RUNNING THE LEXER -----------------------------------------------------------

///
///
pub fn file(path: String) -> Query(List(Token), env) {
  query.read(path)
  |> query.then(run)
}

/// Splits some source code into a list of graphemes (*not* characters!) and then
/// reduces that list to a stream of tokens. This can never fail, instead unknown
/// tokens are simply assigned the `Unknown` token type.
///
pub fn run(input: String) -> Query(List(Token), env) {
  let chars = string.to_graphemes(input)
  let state = State(chars, [], [], 1, 1)

  query.return(step(state))
}

fn peek(chars: List(String)) -> String {
  case chars {
    [char, ..] -> char
    [] -> ""
  }
}

// STEPPING THROUGH THE LEXER --------------------------------------------------

fn step(state: State) -> List(Token) {
  let follow = peek(state.chars)
  let token = match_token(state.stack, follow)

  case token {
    Some(tok) if follow == "" -> list.reverse([token.EOF, tok, ..state.tokens])
    None if follow == "" -> list.reverse([token.EOF, ..state.tokens])

    Some(tok) -> {
      let [char, ..chars] = state.chars
      let tokens = [tok, ..state.tokens]
      case char {
        "\n" -> {
          let row = state.row + 1
          let col = 1
          let stack = []
          step(State(chars, stack, tokens, row, col))
        }
        " " | "\t" -> {
          let col = state.col + 1
          let stack = []
          let state =
            State(..state, chars: chars, stack: stack, tokens: tokens, col: col)
          step(state)
        }
        _ -> {
          let col = state.col + 1
          let stack = [char]
          step(
            State(..state, chars: chars, stack: stack, tokens: tokens, col: col),
          )
        }
      }
    }

    None -> {
      let [char, ..chars] = state.chars
      case char {
        "\n" -> {
          let row = state.row + 1
          let col = 1
          let stack = []
          step(State(..state, chars: chars, stack: stack, row: row, col: col))
        }
        " " | "\t" -> {
          let col = state.col + 1
          let stack = []
          step(State(..state, chars: chars, stack: stack, col: col))
        }
        _ -> {
          let col = state.col + 1
          let stack = [char, ..state.stack]
          step(State(..state, chars: chars, stack: stack, col: col))
        }
      }
    }
  }
}

//

fn match_token(stack: List(String), follow: String) -> Option(Token) {
  let follow_upper = is_upper(follow)
  let follow_lower = is_lower(follow)
  let follow_digit = is_digit(follow)
  let follow_alpha = follow_upper || follow_lower
  let follow_alphanum = follow_alpha || follow_digit
  let follow_whitespace = is_whitespace(follow)
  let follow_empty = follow == ""
  let follow_symbol = is_ren_symbol(follow)
  let follow_unknown = is_unknown_symbol(follow)

  let is_keyword = follow_empty || follow_whitespace || !follow_alphanum

  // ❗️ The stack is built in reverse, so we have to remember to match characters
  // on the stack backwards otherwise everything will be wrong.
  //
  // If we're chomping the characters "fun" then the stack is going to grow like
  // this:
  //
  //                [] "fun"
  //            [ "f"] "un"
  //      [ "u", "f" ] "n"
  // [ "n", "u", "f" ] ""
  // 
  case stack {
    // KEYWORDS ----------------------------------------------------------------
    ["s", "a"] if is_keyword -> Some(token.as_)
    ["t", "r", "e", "s", "s", "a"] if is_keyword -> Some(token.assert_)
    ["e", "s", "a", "c"] if is_keyword -> Some(token.case_)
    ["e", "s", "l", "e"] if is_keyword -> Some(token.else)
    ["t", "c", "e", "p", "x", "e"] if is_keyword -> Some(token.expect)
    ["t", "x", "e"] if is_keyword -> Some(token.ext)
    ["n", "u", "f"] if is_keyword -> Some(token.fun)
    ["λ"] -> Some(token.fun)
    ["f", "i"] if is_keyword -> Some(token.if_)
    ["t", "r", "o", "p", "m", "i"] if is_keyword -> Some(token.import_)
    ["t", "e", "l"] if is_keyword -> Some(token.let_)
    ["n", "o"] if is_keyword -> Some(token.on)
    ["g", "k", "p"] if is_keyword -> Some(token.pkg)
    ["b", "u", "p"] if is_keyword -> Some(token.pub_)
    ["h", "c", "t", "i", "w", "s"] if is_keyword -> Some(token.switch)
    ["n", "e", "h", "t"] if is_keyword -> Some(token.then)
    ["e", "p", "y", "t"] if is_keyword -> Some(token.type_)

    // SYMBOLS -----------------------------------------------------------------
    [">", "-"] -> Some(token.arrow)
    ["→"] -> Some(token.arrow)
    [":"] -> Some(token.colon)
    [","] -> Some(token.comma)
    ["."] if follow != "." -> Some(token.dot)
    ["."] -> None
    [".", "."] -> Some(token.double_dot)
    ["/", "/"] -> Some(token.double_slash)
    ["="] if follow != "=" -> Some(token.equals)
    ["="] -> None
    ["{"] -> Some(token.lbrace)
    ["["] -> Some(token.lbracket)
    ["("] -> Some(token.lparen)
    ["?"] -> Some(token.question)
    ["}"] -> Some(token.rbrace)
    ["]"] -> Some(token.rbracket)
    [")"] -> Some(token.rparen)
    ["_"] -> Some(token.underscore)

    // OPERATORS ---------------------------------------------------------------
    ["+"] -> Some(token.add)
    ["-"] if follow != ">" -> Some(token.sub)
    ["-"] -> None
    ["*"] -> Some(token.mul)
    ["/"] if follow != "/" -> Some(token.div)
    ["/"] -> None
    ["%"] -> Some(token.mod)
    ["^"] -> Some(token.pow)
    ["&"] -> Some(token.and)
    ["|"] if follow != ">" -> Some(token.or)
    ["|"] -> None
    ["=", "="] -> Some(token.eq)
    ["=", "!"] -> Some(token.neq)
    ["<"] if follow != "=" -> Some(token.lt)
    ["<"] -> None
    ["=", "<"] -> Some(token.lte)
    [">"] if follow != "=" -> Some(token.gt)
    [">"] -> None
    ["=", ">"] -> Some(token.gte)
    [">", "|"] -> Some(token.pipe)
    [";"] -> Some(token.seq)

    // EVERYTHING ELSE ---------------------------------------------------------
    [_, ..] if follow_alpha ->
      case list.all(stack, is_digit) {
        True -> {
          let chars = list.reverse(stack)
          let str = string.concat(chars)
          let num = float.parse(str)
          let int = result.map(int.parse(str), int.to_float)
          result.or(num, int)
          |> option.from_result
          |> option.map(token.num)
        }
        False -> None
      }

    [_, ..] if follow_empty || follow_whitespace || follow_symbol || follow_unknown && follow != "_" -> {
      let chars = list.reverse(stack)
      let str = string.concat(chars)
      case list.all(chars, is_digit) {
        True -> {
          let num = float.parse(str)
          let int = result.map(int.parse(str), int.to_float)
          result.or(num, int)
          |> option.from_result
          |> option.map(token.num)
        }
        False -> token.identifier(str)
      }
    }

    _ -> None
  }
}

// PREDICATES ------------------------------------------------------------------

fn is_upper(char: String) -> Bool {
  case char {
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" -> True
    "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" -> True
    "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" -> True
    _ -> False
  }
}

fn is_lower(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" -> True
    "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" -> True
    "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" -> True
    _ -> False
  }
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn is_whitespace(char: String) -> Bool {
  case char {
    " " | "\t" | "\r" | "\n" -> True
    _ -> False
  }
}

fn is_ren_symbol(char: String) -> Bool {
  case char {
    "-" | "," | ";" | ":" | "!" | "?" | "." -> True
    "(" | ")" | "[" | "]" | "{" | "}" | "*" | "/" -> True
    "&" | "%" | "^" | "+" | "<" | "=" | ">" | "|" -> True
    _ -> False
  }
}

fn is_unknown_symbol(char: String) -> Bool {
  case <<char:utf8>> {
    <<code:int>> if code > 47 && code < 58 -> False
    <<code:int>> if code > 64 && code < 91 -> False
    <<code:int>> if code > 96 && code < 123 -> False

    _ -> !is_ren_symbol(char)
  }
}
