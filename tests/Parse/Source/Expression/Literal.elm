module Parse.Source.Expression.Literal exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Dict
import Expect
import Fuzz exposing (..)
import Parser
import Ren.Data.Expression as Expression
import Ren.Data.Expression.Literal as Literal
import Test exposing (..)



-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Literal.Literal"
        [ arrayLiteralSuite
        , booleanLiteralSuite
        , numberLiteralSuite
        , objectLiteralSuite
        , stringLiteralSuite
        , undefinedLiteralSuite
        ]


arrayLiteralSuite : Test
arrayLiteralSuite =
    describe "Literal.Array"
        [ emptyArrayLiteralTest
        , singleElementArrayLiteralTest
        , manyElementsArrayLiteralTest
        , nestedArrayLiteralTest
        ]


booleanLiteralSuite : Test
booleanLiteralSuite =
    describe "Literal.Boolean"
        [ trueBooleanLiteralTest
        , falseBooleanLiteralTest
        ]


numberLiteralSuite : Test
numberLiteralSuite =
    describe "Literal.Number"
        [ floatNumberLiteralTest
        , negativeFloatNumberLiteralTest
        , intNumberLiteralTest
        , negativeIntNumberLiteralTest
        ]


objectLiteralSuite : Test
objectLiteralSuite =
    describe "Literal.Object"
        [ emptyObjectLiteralTest
        , singleFieldObjectLiteralTest
        , manyFieldsObjectLiteralTest
        , nestedObjectLiteralTest
        ]


stringLiteralSuite : Test
stringLiteralSuite =
    describe "Literal.String"
        [ doubleQuoteStringLiteralTest
        , doubleQuoteNestedSingleQuotesStringLiteralTest
        , doubleQuoteEscapedDoubleQuotesStringLiteralTest
        , doubleQuoteUnbalancedStringLiteralTest
        , singleQuoteStringLiteralTest
        , singleQuoteNestedDoubleQuotesStringLiteralTest
        , singleQuoteEscapedSingleQuotesStringLiteralTest
        , singleQuoteUnbalancedStringLiteralTest
        ]


undefinedLiteralSuite : Test
undefinedLiteralSuite =
    describe "Literal.Undefined"
        [ undefinedLiteralTest
        ]



-- ARRAY LITERALS --------------------------------------------------------------


emptyArrayLiteralTest : Test
emptyArrayLiteralTest =
    let
        title =
            "Array literals may be empty."

        input =
            "[]"

        expected =
            Ok <| Literal.Array []
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


singleElementArrayLiteralTest : Test
singleElementArrayLiteralTest =
    let
        title =
            "Array literals may have exactly one element."

        input =
            "[ 1 ]"

        expected =
            Ok <|
                Literal.Array [ Expression.number 1 ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


manyElementsArrayLiteralTest : Test
manyElementsArrayLiteralTest =
    let
        title =
            "Array literals may have more than one element."

        input =
            "[ 1, 2, 3 ]"

        expected =
            Ok <|
                Literal.Array <|
                    [ Expression.number 1
                    , Expression.number 2
                    , Expression.number 3
                    ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


nestedArrayLiteralTest : Test
nestedArrayLiteralTest =
    let
        title =
            "Array literals can be nested."

        input =
            "[ [ 1 ] ]"

        expected =
            Ok <| Literal.Array [ Expression.array [ Expression.number 1 ] ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )



-- BOOLEAN LITERALS ------------------------------------------------------------


trueBooleanLiteralTest : Test
trueBooleanLiteralTest =
    let
        title =
            "`true` is a boolean literal."

        input =
            "true"

        expected =
            Ok <| Literal.Boolean True
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


falseBooleanLiteralTest : Test
falseBooleanLiteralTest =
    let
        title =
            "`false` is a boolean literal."

        input =
            "false"

        expected =
            Ok <| Literal.Boolean False
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )



-- NUMBER LITERALS -------------------------------------------------------------


floatNumberLiteralTest : Test
floatNumberLiteralTest =
    let
        title =
            "Number literals may have a decimal point."

        input =
            "9001.1009"

        expected =
            Ok <| Literal.Number 9001.1009
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


negativeFloatNumberLiteralTest : Test
negativeFloatNumberLiteralTest =
    let
        title =
            "Number literals wit a decimal point may be negative."

        input =
            "-9001.1009"

        expected =
            Ok <| Literal.Number -9001.1009
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


intNumberLiteralTest : Test
intNumberLiteralTest =
    let
        title =
            "Number literals do not need a decimal point."

        input =
            "9001"

        expected =
            Ok <| Literal.Number 9001
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


negativeIntNumberLiteralTest : Test
negativeIntNumberLiteralTest =
    let
        title =
            "Number literals without a decimal point may be negative."

        input =
            "-9001"

        expected =
            Ok <| Literal.Number -9001
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )



-- OBJECT LITERALS -------------------------------------------------------------


emptyObjectLiteralTest : Test
emptyObjectLiteralTest =
    let
        title =
            "Object literals may be empty."

        input =
            "{}"

        expected =
            Ok <| Literal.Object Dict.empty
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


singleFieldObjectLiteralTest : Test
singleFieldObjectLiteralTest =
    let
        title =
            "Object literals may have exactly one field."

        input =
            "{ foo: 1 }"

        expected =
            Ok <|
                Literal.Object <|
                    Dict.fromList
                        [ ( "foo", Expression.number 1 ) ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


manyFieldsObjectLiteralTest : Test
manyFieldsObjectLiteralTest =
    let
        title =
            "Object literals may have more than one field."

        input =
            "{ foo: 1, bar: 2, baz: 3 }"

        expected =
            Ok <|
                Literal.Object <|
                    Dict.fromList
                        [ ( "foo", Expression.number 1 )
                        , ( "bar", Expression.number 2 )
                        , ( "baz", Expression.number 3 )
                        ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )


nestedObjectLiteralTest : Test
nestedObjectLiteralTest =
    let
        title =
            "Object literals can be nested."

        input =
            "{ foo: { bar: 1 } }"

        expected =
            Ok <|
                Literal.Object <|
                    Dict.fromList
                        [ ( "foo"
                          , Expression.object
                                [ ( "bar", Expression.number 1 )
                                ]
                          )
                        ]
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )



-- STRING LITERALS -------------------------------------------------------------


doubleQuoteStringLiteralTest : Test
doubleQuoteStringLiteralTest =
    let
        title =
            "String literals may be written with double quotes."

        input =
            "\"foo\""

        expected =
            Ok <| Literal.String "foo"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


doubleQuoteNestedSingleQuotesStringLiteralTest : Test
doubleQuoteNestedSingleQuotesStringLiteralTest =
    let
        title =
            "Single quotes may be nested in double quote strings without escaping."

        input =
            "\"hello 'foo'\""

        expected =
            Ok <| Literal.String "hello 'foo'"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


doubleQuoteEscapedDoubleQuotesStringLiteralTest : Test
doubleQuoteEscapedDoubleQuotesStringLiteralTest =
    let
        title =
            "Double quotes must be escaped inside double quote strings."

        input =
            "\"hello \\\"foo\\\"\""

        expected =
            Ok <| Literal.String "hello \"foo\""
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


doubleQuoteUnbalancedStringLiteralTest : Test
doubleQuoteUnbalancedStringLiteralTest =
    let
        title =
            "Double quote strings must be balanced."

        input =
            "\"foo"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.err
        )


singleQuoteStringLiteralTest : Test
singleQuoteStringLiteralTest =
    let
        title =
            "String literals may be written with single quotes."

        input =
            "'foo'"

        expected =
            Ok <| Literal.String "foo"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


singleQuoteNestedDoubleQuotesStringLiteralTest : Test
singleQuoteNestedDoubleQuotesStringLiteralTest =
    let
        title =
            "Double quotes may be nested in single quote strings without escaping."

        input =
            "'hello \"foo\"'"

        expected =
            Ok <| Literal.String "hello \"foo\""
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


singleQuoteEscapedSingleQuotesStringLiteralTest : Test
singleQuoteEscapedSingleQuotesStringLiteralTest =
    let
        title =
            "Single quotes must be escaped inside single quote strings."

        input =
            "'hello \\'foo\\''"

        expected =
            Ok <| Literal.String "hello 'foo'"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.equal expected
        )


singleQuoteUnbalancedStringLiteralTest : Test
singleQuoteUnbalancedStringLiteralTest =
    let
        title =
            "Single quote strings must be balanced."

        input =
            "'foo"
    in
    test title
        (\_ ->
            Parser.run Literal.primitiveParser input
                |> Expect.err
        )



-- UNDEFINED LITERALS ----------------------------------------------------------


undefinedLiteralTest : Test
undefinedLiteralTest =
    let
        title =
            "Empty parentheses mean `undefined`."

        input =
            "()"

        expected =
            Ok <| Literal.Undefined
    in
    test title
        (\_ ->
            Parser.run (Literal.parser Expression.local Expression.parser) input
                |> Expect.equal expected
        )
