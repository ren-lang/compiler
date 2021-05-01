module Parser.Expression.Literal exposing 
    ( suite
    )


-- IMPORTS ---------------------------------------------------------------------


import Dict
import Ren.Data.Expression as Expression
import Ren.Data.Expression.Literal as Literal exposing (Literal(..))
import Parser

import Expect
import Fuzz exposing (..)
import Test exposing (..)


-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Literal.Literal"
        [ primitiveParserSuite
        , containerParserSuite

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

        , test "Errors when trying to parse a container" <| \_ ->
            Parser.run Literal.primitiveParser "[ 1, 2, 3 ]"
                |> Expect.err
        ]

containerParserSuite : Test
containerParserSuite =
    describe "Literal.containerParser"
        [ arrayTests
        , objectTests
        ]


-- PRIMITIVE PARSER TESTS ------------------------------------------------------


booleanTests : Test
booleanTests =
    describe "Literal.Boolean"
        [ test "'true' parses as a Literal.Boolean" <| \_ -> 
            Parser.run Literal.primitiveParser "true"
                |> Expect.equal (Ok <| Literal.Boolean True)
        , test "'True' does not parse as a Literal.Boolean" <| \_ ->
            Parser.run Literal.primitiveParser "True"
                |> Expect.err
    
        , test "'false' parses as a Literal.Bollean" <| \_ ->
            Parser.run Literal.primitiveParser "false"
                |> Expect.equal (Ok <| Literal.Boolean False)
        , test "'False' does not parse as a Literal.Boolean" <| \_ ->
            Parser.run Literal.primitiveParser "False"
                |> Expect.err
        ]

numberTests : Test
numberTests =
    describe "Literal.Number"
        [ test "A number with a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "100.300"
                |> Expect.equal (Ok <| Literal.Number 100.3)
        , test "A negative number with a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "-100.300"
                |> Expect.equal (Ok <| Literal.Number -100.3)

        , test "A number without a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "12"
                |> Expect.equal (Ok <| Literal.Number 12.0)
        , test "A negative number without a decimal point parses as a Literal.Number" <| \_ ->
            Parser.run Literal.primitiveParser "-12"
                |> Expect.equal (Ok <| Literal.Number -12.0)

        , fuzz int "It parses any integer" <| \num ->
            Parser.run Literal.primitiveParser (String.fromInt num)
                |> Expect.equal (Ok <| Literal.Number (Basics.toFloat num))
        , fuzz float "It parses any float" <| \num ->
            Parser.run Literal.primitiveParser (String.fromFloat num)
                |> Expect.equal (Ok <| Literal.Number num)
        ]

stringTests : Test
stringTests =
    describe "Literal.String"
        [ test "Text surrounded by double quotes parses as a Literal.String" <| \_ -> 
            Parser.run Literal.primitiveParser "\"double quotes\""
                |> Expect.equal (Ok <| Literal.String "double quotes")
        , test "Text surrounded by double quotes ignores nested single quotes" <| \_ ->
            Parser.run Literal.primitiveParser "\"double and 'single' quotes\""
                |> Expect.equal (Ok <| Literal.String "double and 'single' quotes")
        , test "Text surrounded by double quotes ignores escaped double quotes" <| \_ ->
            Parser.run Literal.primitiveParser "\"double and \\\"double\\\" quotes\""
                |> Expect.equal (Ok <| Literal.String "double and \"double\" quotes")
        , test "Text with unbalanced double quotes does not hang the parser" <| \_ ->
            Parser.run Literal.primitiveParser "\""
                |> Expect.err

        , test "Text surrounded by single quotes parses as a Literal.String" <| \_ -> 
            Parser.run Literal.primitiveParser "'horrible single quotes'"
                |> Expect.equal (Ok <| Literal.String "horrible single quotes")
        , test "Text surrounded by single quotes ignores nested double quotes" <| \_ ->
            Parser.run Literal.primitiveParser "'single and horrible \"double\" quotes'"
                |> Expect.equal (Ok <| Literal.String "single and horrible \"double\" quotes")
        , test "Text surrounded by single quotes ignores escaped single quotes" <| \_ ->
            Parser.run Literal.primitiveParser "'single and \\'single\\' quotes'"
                |> Expect.equal (Ok <| Literal.String "single and 'single' quotes")
        , test "Text with unbalanced single quotes does not hang the parser" <| \_ ->
            Parser.run Literal.primitiveParser "\'iwufehiuah"
                |> Expect.equal (Ok <| Literal.String "uwrofhdaouhf")
        ]


-- CONTAINER PARSER TESTS ------------------------------------------------------


arrayTests : Test
arrayTests =
    describe "Literal.Array"
        [ test "It parses an array with no elements" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "[]"
                |> Expect.equal (Ok <| Literal.Array [])
        , test "It parses an array with one element" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "[1]"
                |> Expect.equal (Ok <| Literal.Array [ Expression.number 1 ])
        , test "It parses an array with many elements" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "[1,2,3,4]"
                |> Expect.equal (Ok <| Literal.Array <| [ Expression.number 1, Expression.number 2, Expression.number 3, Expression.number 4 ])
        , test "Whitespace is insignificant" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "[   1, 2,\n   3    ,4]"
                |> Expect.equal (Ok <| Literal.Array <| [ Expression.number 1, Expression.number 2, Expression.number 3, Expression.number 4 ])
        , test "Arrays can be nested" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "[[1]]]"
                |> Expect.equal (Ok <| Literal.Array [ Expression.array [ Expression.number 1 ] ])
        ]

objectTests : Test
objectTests =
    describe "Literal.Object"
        [ test "It parses an object with no fields" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "{}"
                |> Expect.equal (Ok <| Literal.Object <| Dict.fromList [])
        , test "It parses an object with one field" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "{ a: 1 }"
                |> Expect.equal (Ok <| Literal.Object <| Dict.fromList [( "a", Expression.number 1 )])
        , test "It parses an object with two fields" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "{ a: 1, b: 2  }"
                |> Expect.equal (Ok <| Literal.Object <| Dict.fromList [( "a", Expression.number 1 ), ( "b", Expression.number 2 )])
        , test "Whitespace is insignificant" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "{\n a     : 1   , b: \n2  }"
                |> Expect.equal (Ok <| Literal.Object <| Dict.fromList [( "a", Expression.number 1 ), ( "b", Expression.number 2 )])
        , test "Objects can be nested" <| \_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) "{ a: { b: 1 } }"
                |> Expect.equal (Ok <| Literal.Object <| Dict.fromList [( "a", Expression.object [( "b", Expression.number 1 )] )])
        ]