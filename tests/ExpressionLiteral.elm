module ExpressionLiteral exposing (..)

import Cherry.Stage.Parse.Expression.Literal as Literal
import Cherry.Data.AST as AST
import Parser

import Expect
import Test exposing (..)
import Fuzz exposing (int)


primitiveParserTest : Test
primitiveParserTest =
  describe "Literal.primitiveParser" [
      test "'true' parses as a literal boolean" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "true"
            expectedResult = Ok (AST.Boolean True)
          in Expect.equal parsedResult expectedResult,

      test "'false' parses as a literal boolean" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "false"
            expectedResult = Ok (AST.Boolean False)
          in Expect.equal parsedResult expectedResult,

      test "A number with a decimal point parses as a number" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "100.300"
            expectedResult = Ok (AST.Number 100.3)
          in Expect.equal parsedResult expectedResult,

      test "A number without a decimal point parses as a number" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "12"
            expectedResult = Ok (AST.Number 12.0)
          in Expect.equal parsedResult expectedResult,

      -- Can't test this yet because negative signs don't work
      skip <| fuzz int "It parses any number" <|
        \num ->
          let
            parsedResult = Parser.run Literal.primitiveParser (String.fromInt num)
            expectedResult = Ok (AST.Number (toFloat num))
          in Expect.equal parsedResult expectedResult,

      test "text surrounded by double quotes parses as a string literal" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "\"double quotes\""
            expectedResult = Ok (AST.String "double quotes")
          in Expect.equal parsedResult expectedResult,

      test "text surrounded by single quotes parses as a string literal" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "'horrible single quotes'"
            expectedResult = Ok (AST.String "horrible single quotes")
          in Expect.equal parsedResult expectedResult,

      -- This tests that you can nest a string inside another by using
      -- different quotation marks. It should not be possible to create a
      -- string literal like "string literal'.
      describe "quote marks nested in a string literal are ignored" [
        test "using double quotes" <|
          \_ -> 
            let
              parsedResult = Parser.run Literal.primitiveParser "\"it's a 'nested' string\""
              expectedResult = Ok (AST.String "it's a 'nested' string")
            in Expect.equal parsedResult expectedResult,

        test "using single quotes" <|
          \_ -> 
            let
              parsedResult = Parser.run Literal.primitiveParser "'a \"nested\" string'"
              expectedResult = Ok (AST.String "a \"nested\" string")
            in Expect.equal parsedResult expectedResult
        ],

      test "Errors when no literal is found" <|
        \_ -> 
          let
            parsedResult = Parser.run Literal.primitiveParser "truish"
            expectedResult = Err ([
              Parser.DeadEnd 1 1 (Parser.ExpectingKeyword "true"),
              Parser.DeadEnd 1 1 (Parser.ExpectingKeyword "false"),
              Parser.DeadEnd 1 1 (Parser.ExpectingFloat),
              Parser.DeadEnd 1 1 (Parser.ExpectingInt),
              Parser.DeadEnd 1 1 (Parser.ExpectingSymbol "\""),
              Parser.DeadEnd 1 1 (Parser.ExpectingSymbol "'")
              ])
          in Expect.equal parsedResult expectedResult
    ]
