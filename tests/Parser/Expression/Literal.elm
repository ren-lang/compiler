module Parser.Expression.Literal exposing 
    ( suite
    )

import Cherry.Stage.Parse.Expression.Literal as Literal
import Cherry.Data.AST as AST
import Parser

import Expect
import Test exposing (..)
import Fuzz exposing (..)
import Cherry.Data.AST exposing (Expression(..))


-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "AST.Literal"
        [ primitiveParserSuite

        , test "Errors when no literals can be parsed" <| \_ ->
            Parser.run Literal.primitiveParser "truish"
                |> Expect.err 
        ]

primitiveParserSuite : Test
primitiveParserSuite =
    describe "Literal.primitiveParser"
        [ booleanTests
        , numberTests
        , stringTests
        ]


-- PRIMITIVE PARSER TESTS ------------------------------------------------------


booleanTests : Test
booleanTests =
    describe "Literal.Boolean"
        [ test "'true' parses as a Literal.Boolean" <| \_ -> 
            Parser.run Literal.primitiveParser "true"
                |> Expect.equal (Ok <| AST.Boolean True)
        , test "'True' does not parse as a Literal.Boolean" <| \_ ->
            Parser.run Literal.primitiveParser "True"
                |> Expect.err
    
        , test "'false' parses as a Literal.Bollean" <| \_ ->
            Parser.run Literal.primitiveParser "false"
                |> Expect.equal (Ok <|AST.Boolean False)
        , test "'False' does not parse as a Literal.Boolean" <| \_ ->
            Parser.run Literal.primitiveParser "False"
                |> Expect.err
        ]

numberTests : Test
numberTests =
    describe "Literal.Number"
        [ test "A number with a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "100.300"
                |> Expect.equal (Ok <| AST.Number 100.3)
        , test "A negative number with a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "-100.300"
                |> Expect.equal (Ok <| AST.Number -100.3)

        , test "A number without a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "12"
                |> Expect.equal (Ok <|AST.Number 12.0)
        , test "A negative number without a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "-12"
                |> Expect.equal (Ok <|AST.Number -12.0)

        , fuzz int "It parses any integer" <| \num ->
            Parser.run Literal.primitiveParser (String.fromInt num)
                |> Expect.equal (Ok <| AST.Number (Basics.toFloat num))
        , fuzz float "It parses any float" <| \num ->
            Parser.run Literal.primitiveParser (String.fromFloat num)
                |> Expect.equal (Ok <| AST.Number num)
        ]

stringTests : Test
stringTests =
    describe "Literal.String"
        [ test "Text surrounded by double quotes parses as a Literal.String" <| \_ -> 
            Parser.run Literal.primitiveParser "\"double quotes\""
                |> Expect.equal (Ok <| AST.String "double quotes")
        , test "Text surrounded by double quotes ignores nested single quotes" <| \_ ->
            Parser.run Literal.primitiveParser "\"double and 'single' quotes\""
                |> Expect.equal (Ok <| AST.String "double and 'single' quotes")
        , test "Text surrounded by double quotes ignores escaped double quotes" <| \_ ->
            Parser.run Literal.primitiveParser "\"double and \\\"double\\\" quotes\""
                |> Expect.equal (Ok <| AST.String "double and \"double\" quotes")

        , test "Text surrounded by single quotes parses as a Literal.String" <| \_ -> 
            Parser.run Literal.primitiveParser "'horrible single quotes'"
                |> Expect.equal (Ok <| AST.String "horrible single quotes")
        , test "Text surrounded by single quotes ignores nested double quotes" <| \_ ->
            Parser.run Literal.primitiveParser "'single and horrible \"double\" quotes'"
                |> Expect.equal (Ok <| AST.String "single and horrible \"double\" quotes")
        , test "Text surrounded by single quotes ignores escaped single quotes" <| \_ ->
            Parser.run Literal.primitiveParser "'single and \\'single\\' quotes'"
                |> Expect.equal (Ok <| AST.String "single and 'single' quotes")
        ]
