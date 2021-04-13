module ExpressionLiteral exposing (..)

import Cherry.Stage.Parse.Expression.Literal as Literal
import Cherry.Data.AST as AST
import Parser

import Expect
import Test exposing (..)
import Fuzz exposing (int)


primitiveParserTest : Test
primitiveParserTest =
    describe "Literal.primitiveParser" 
        [ test "'true' parses as a literal boolean" <| \_ -> 
            Parser.run Literal.primitiveParser "true"
                |> Expect.equal (Ok <| AST.Boolean True)

        , test "'false' parses as a literal boolean" <| \_ -> 
            Parser.run Literal.primitiveParser "false"
                |> Expect.equal (Ok <|AST.Boolean False)

        , test "A number with a decimal point parses as a number" <| \_ -> 
            Parser.run Literal.primitiveParser "100.300"
                |> Expect.equal (Ok <| AST.Number 100.3)

        , test "A number without a decimal point parses as a number" <| \_ -> 
            Parser.run Literal.primitiveParser "12"
                |> Expect.equal (Ok <|AST.Number 12.0)

        -- Can't test this yet because negative signs don't work
        , skip <| fuzz int "It parses any number" <| \num ->
            Parser.run Literal.primitiveParser (String.fromInt num)
                |> Expect.equal (Ok <| AST.Number (toFloat num))

        , test "text surrounded by double quotes parses as a string literal" <| \_ -> 
            Parser.run Literal.primitiveParser "\"double quotes\""
                |> Expect.equal (Ok <| AST.String "double quotes")

        , test "text surrounded by single quotes parses as a string literal" <| \_ -> 
            Parser.run Literal.primitiveParser "'horrible single quotes'"
                |> Expect.equal (Ok <| AST.String "horrible single quotes")

        -- This tests that you can nest a string inside another by using
        -- different quotation marks. It should not be possible to create a
        -- string literal like "string literal'.
        , describe "quote marks nested in a string literal are ignored"
            [ test "using double quotes" <| \_ -> 
                Parser.run Literal.primitiveParser "\"it's a 'nested' string\""
                    |> Expect.equal (Ok <| AST.String "it's a 'nested' string")

            , test "using single quotes" <| \_ -> 
                Parser.run Literal.primitiveParser "'a \"nested\" string'"
                    |> Expect.equal (Ok <| AST.String "a \"nested\" string")

            , test "opening and closing quotes must match" <| \_ ->
                Parser.run Literal.primitiveParser "\"an unbalanced string'"
                    |> Expect.err
            ]

        , test "Errors when no literal is found" <| \_ -> 
            Parser.run Literal.primitiveParser "truish"
                |> Expect.err 
        ]
