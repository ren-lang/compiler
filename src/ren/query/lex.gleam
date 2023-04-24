// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/list.{Continue, Stop}
import gleam/regex
import gleam/string
import ren/data/token.{Token, TokenT}
import ren/query.{Query}

// TYPES -----------------------------------------------------------------------

type Matcher(a) =
  fn(String, String) -> Match(a)

type Match(a) {
  Keep(a)
  Drop
  NoMatch
}

type State {
  State(tokens: List(Token), buffer: #(Int, Int, String), row: Int, col: Int)
}

// QUERIES ---------------------------------------------------------------------

pub fn file(path: String) -> Query(List(Token), env) {
  use src <- query.do(query.read(path))

  run(src)
}

pub fn run(src: String) -> Query(List(Token), env) {
  let source = string.to_graphemes(src)
  let buffer = #(1, 1, "")
  let state = State([], buffer, 1, 1)

  source
  |> do_run(state)
  |> query.return
}

fn do_run(source: List(String), state: State) -> List(Token) {
  case source, state.buffer {
    // We're at the end of the source and there's nothing left in the buffer so
    // we can just return the tokens we've collected!
    [], #(_, _, "") -> list.reverse(state.tokens)

    // We're at the end of the source but there's some stuff left in the buffer
    // so we need to try and match it.
    [], #(start_row, start_col, lexeme) ->
      case do_match(lexeme, "") {
        NoMatch ->
          list.reverse([
            Token(
              start_row,
              start_col,
              state.row,
              state.col,
              lexeme,
              token.Unknown(lexeme),
            ),
            ..state.tokens
          ])
        Drop -> list.reverse(state.tokens)
        Keep(token) -> {
          list.reverse([
            Token(start_row, start_col, state.row, state.col, lexeme, token),
            ..state.tokens
          ])
        }
      }

    // We get one grapheme lookahead when trying to match the current buffer with
    // a token.
    [lookahead, ..rest], #(start_row, start_col, lexeme) -> {
      let #(row, col) = next(state.row, state.col, lexeme)

      case do_match(lexeme, lookahead) {
        // We've found a match so we can add the token to the list of tokens and
        // set the buffer to the next grapheme, progress the row/col and continue.
        Keep(token) -> {
          let buffer = #(state.row, state.col, lookahead)
          let tokens = [
            Token(start_row, start_col, state.row, state.col, lexeme, token),
            ..state.tokens
          ]
          do_run(rest, State(tokens, buffer, row, col))
        }

        // We matched a token but we want to drop it. That means we *don't* add
        // it to the list of tokens, but we do still set the buffer to the next
        // grapheme, progress the row/col and continue.
        Drop -> {
          let buffer = #(state.row, state.col, lookahead)
          do_run(rest, State(state.tokens, buffer, row, col))
        }

        // We haven't matched a token yet so add the current grapheme to the 
        // buffer and continue.
        NoMatch -> {
          let buffer = #(start_row, start_col, lexeme <> lookahead)
          do_run(rest, State(state.tokens, buffer, row, col))
        }
      }
    }
  }
}

fn do_match(lexeme: String, lookahead: String) -> Match(TokenT) {
  use _, matcher <- list.fold_until(matchers, NoMatch)

  case matcher(lexeme, lookahead) {
    Keep(a) -> Stop(Keep(a))
    Drop -> Stop(Drop)
    NoMatch -> Continue(NoMatch)
  }
}

fn next(row: Int, col: Int, lexeme: String) -> #(Int, Int) {
  case lexeme {
    "\n" -> #(row + 1, 1)
    _ -> #(row, col + 1)
  }
}

const matchers: List(Matcher(TokenT)) = [
  symbols_and_operators,
  keywords_and_identifiers,
  numbers,
  strings,
  whitespace,
]

// MATCHERS --------------------------------------------------------------------

fn symbols_and_operators(lexeme: String, lookahead: String) -> Match(TokenT) {
  // 🚨 Don't forget to order matchers from longest to shortest so that everything
  // gets tested properly. If you don't, you'll get weird bugs like `|>` lexing
  // as `|` and `>` instead of `|>`.
  case lexeme, lookahead {
    "->", _ | "→", _ -> Keep(token.arrow)
    "-", ">" -> NoMatch
    "-", "0" -> NoMatch
    "-", "1" -> NoMatch
    "-", "2" -> NoMatch
    "-", "3" -> NoMatch
    "-", "4" -> NoMatch
    "-", "5" -> NoMatch
    "-", "6" -> NoMatch
    "-", "7" -> NoMatch
    "-", "8" -> NoMatch
    "-", "9" -> NoMatch
    "-", _ -> Keep(token.sub)
    "_", _ -> Keep(token.underscore)
    ",", _ -> Keep(token.comma)
    ";", _ -> Keep(token.seq)
    ":", _ -> Keep(token.colon)
    "?", _ -> Keep(token.question)
    ".", "." -> NoMatch
    ".", _ -> Keep(token.dot)
    "..", _ -> Keep(token.double_dot)
    "(", _ -> Keep(token.lparen)
    ")", _ -> Keep(token.rparen)
    "[", _ -> Keep(token.lbracket)
    "]", _ -> Keep(token.rbracket)
    "{", _ -> Keep(token.lbrace)
    "}", _ -> Keep(token.rbrace)
    "@", _ -> Keep(token.at)
    "*", _ -> Keep(token.mul)
    "/", "/" -> NoMatch
    "/", _ -> Keep(token.div)
    "//", _ -> Keep(token.double_slash)
    "&", _ -> Keep(token.and)
    "#", _ -> Keep(token.hash)
    "%", _ -> Keep(token.mod)
    "^", _ -> Keep(token.pow)
    "+", "+" -> NoMatch
    "+", _ -> Keep(token.add)
    "++", _ -> Keep(token.concat)
    "<", "=" -> NoMatch
    "<", _ -> Keep(token.lt)
    "<=", _ -> Keep(token.lte)
    "=", "=" -> NoMatch
    "=", _ -> Keep(token.equals)
    "==", _ -> Keep(token.eq)
    ">", "=" -> NoMatch
    ">", _ -> Keep(token.gt)
    ">=", _ -> Keep(token.gte)
    "|", ">" -> NoMatch
    "|", _ -> Keep(token.or)
    "|>", _ -> Keep(token.pipe)
    "!=", _ -> Keep(token.neq)

    _, _ -> NoMatch
  }
}

fn keywords_and_identifiers(lexeme: String, lookahead: String) -> Match(TokenT) {
  let assert Ok(is_lower) = regex.from_string("^[a-z][a-zA-Z0-9_]*$")
  let assert Ok(is_upper) = regex.from_string("^[A-Z][a-zA-Z0-9_]*$")
  let assert Ok(is_inner) = regex.from_string("[a-zA-Z0-9_]")

  case lexeme, regex.check(is_inner, lookahead) {
    "as", False -> Keep(token.as_)
    "assert", False -> Keep(token.assert_)
    "case", False -> Keep(token.case_)
    "else", False -> Keep(token.else)
    "expect", False -> Keep(token.expect)
    "ext", False -> Keep(token.ext)
    "forall", False | "∀", _ -> Keep(token.forall)
    "fun", False | "λ", _ -> Keep(token.fun)
    "if", False -> Keep(token.if_)
    "import", False -> Keep(token.import_)
    "let", False -> Keep(token.let_)
    "on", False -> Keep(token.on)
    "pkg", False -> Keep(token.pkg)
    "pub", False -> Keep(token.pub_)
    "switch", False -> Keep(token.switch)
    "then", False -> Keep(token.then)
    "type", False -> Keep(token.type_)

    _, lookead_is_valid -> {
      case regex.check(is_lower, lexeme) {
        True if lookead_is_valid -> NoMatch
        True -> Keep(token.lower(lexeme))
        False ->
          case regex.check(is_upper, lexeme) {
            True if lookead_is_valid -> NoMatch
            True -> Keep(token.upper(lexeme))
            False -> NoMatch
          }
      }
    }
  }
}

fn numbers(lexeme: String, lookahead: String) -> Match(TokenT) {
  let assert Ok(is_digit) = regex.from_string("[0-9]")
  let assert Ok(is_int) = regex.from_string("^-*[0-9]+$")
  let assert Ok(is_num) = regex.from_string("^-*[0-9]+\\.[0-9]+$")

  let lookead_is_valid = regex.check(is_digit, lookahead)

  case regex.check(is_int, lexeme) {
    True if lookahead == "." -> NoMatch
    True if lookead_is_valid -> NoMatch

    True -> {
      let assert Ok(val) = float.parse(lexeme <> ".0")
      Keep(token.num(val))
    }

    False ->
      case regex.check(is_num, lexeme) {
        True if lookead_is_valid -> NoMatch
        True -> {
          let assert Ok(val) = float.parse(lexeme)
          Keep(token.num(val))
        }
        False -> NoMatch
      }
  }
}

fn strings(lexeme: String, _lookahead: String) -> Match(TokenT) {
  let assert Ok(is_string) =
    regex.from_string("^\"([^\"\\\\]|\\\\[\\s\\S])*\"$")

  case regex.check(is_string, lexeme) {
    True ->
      lexeme
      |> string.drop_left(1)
      |> string.drop_right(1)
      |> token.str
      |> Keep
    False -> NoMatch
  }
}

fn whitespace(lexeme: String, lookahead: String) -> Match(TokenT) {
  let assert Ok(is_whitespace) = regex.from_string("^[\\s\\n]+$")

  case regex.check(is_whitespace, lexeme), lookahead {
    True, " " -> NoMatch
    True, "\t" -> NoMatch
    True, "\n" -> NoMatch
    True, _ -> Drop
    False, _ -> NoMatch
  }
}
