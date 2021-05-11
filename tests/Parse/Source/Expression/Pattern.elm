module Parse.Source.Expression.Pattern exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Expect
import Fuzz exposing (..)
import Parser
import Ren.Data.Expression.Literal as Literal
import Ren.Data.Expression.Pattern as Pattern
import Test exposing (..)



-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Pattern.Pattern"
        [ arrayDestructurePatternSuite
        , namePatternSuite
        , objectDestructurePatternSuite
        , valuePatternSuite
        , wildcardPatternSuite
        ]


arrayDestructurePatternSuite : Test
arrayDestructurePatternSuite =
    describe "Pattern.ArrayDestructure"
        [ singleElementArrayDestructurePatternTest
        , manyElementsArrayDestructurePatternTest
        , capitalCaseElementArrayDestructurePatternTest
        , nestedArrayDestructurePatternTest
        ]


namePatternSuite : Test
namePatternSuite =
    describe "Pattern.Name"
        [ lowerCaseNamePatternTest
        , lowerCaseWithDigitsNamePatternTest
        , camelCaseNamePatternTest
        , numericNamePatternTest
        ]


objectDestructurePatternSuite : Test
objectDestructurePatternSuite =
    describe "Pattern.ObjectDestructure"
        [ singleFieldObjectDestructurePatternTest
        , manyFieldsObjectDestructurePatternTest
        , capitalCaseFieldObjectDestructurePatternTest
        , nestedObjectDestructurePatternTest
        ]


valuePatternSuite : Test
valuePatternSuite =
    describe "Pattern.Value"
        [ booleanValuePatternTest
        , numberValuePatternTest
        , stringValuePatternTest
        ]


wildcardPatternSuite : Test
wildcardPatternSuite =
    describe "Pattern.Wildcard"
        [ underscoreWildcardPatternTest
        , underscoreWithNameWildcardPatternTest
        ]



-- ARRAY DESTRUCTURE PATTERNS --------------------------------------------------


singleElementArrayDestructurePatternTest : Test
singleElementArrayDestructurePatternTest =
    let
        title =
            "A single element can be matched in an array destructure pattern."

        input =
            "[ foo ] "

        expected =
            Ok <| Pattern.ArrayDestructure [ Pattern.Name "foo" ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


manyElementsArrayDestructurePatternTest : Test
manyElementsArrayDestructurePatternTest =
    let
        title =
            "Multiple elements can be matched in an array destructure pattern"

        input =
            "[ foo, bar, baz ]"

        expected =
            Ok <|
                Pattern.ArrayDestructure
                    [ Pattern.Name "foo"
                    , Pattern.Name "bar"
                    , Pattern.Name "baz"
                    ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


capitalCaseElementArrayDestructurePatternTest : Test
capitalCaseElementArrayDestructurePatternTest =
    let
        title =
            "elements in an array destructure pattern cannot begin with a capital letter."

        input =
            "[ Foo ]"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.err
        )


nestedArrayDestructurePatternTest : Test
nestedArrayDestructurePatternTest =
    let
        title =
            "Other patterns can be nested in an array destructure pattern."

        input =
            "[ foo, [ _, bar, 2 ] ]"

        expected =
            Ok <|
                Pattern.ArrayDestructure
                    [ Pattern.Name "foo"
                    , Pattern.ArrayDestructure
                        [ Pattern.Wildcard Nothing
                        , Pattern.Name "bar"
                        , Pattern.Value <| Literal.Number 2
                        ]
                    ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )



-- NAME PATTERNS ---------------------------------------------------------------


lowerCaseNamePatternTest : Test
lowerCaseNamePatternTest =
    let
        title =
            "Name patterns must begin with a lowercase letter"

        input =
            "foo"

        expected =
            Ok <| Pattern.Name "foo"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


lowerCaseWithDigitsNamePatternTest : Test
lowerCaseWithDigitsNamePatternTest =
    let
        title =
            "Name patterns may contain digits."

        input =
            "foo9001"

        expected =
            Ok <| Pattern.Name "foo9001"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


camelCaseNamePatternTest : Test
camelCaseNamePatternTest =
    let
        title =
            "Name patterns may not begin with a capital letter."

        input =
            "Foo"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.err
        )


numericNamePatternTest : Test
numericNamePatternTest =
    let
        title =
            "Name patterns may not begin with a digit."

        input =
            "9001foo"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.err
        )



-- OBJECT DESTRUCTURE PATTERNS -------------------------------------------------


singleFieldObjectDestructurePatternTest : Test
singleFieldObjectDestructurePatternTest =
    let
        title =
            "A single field can be matched in an object destructure pattern."

        input =
            "{ foo }"

        expected =
            Ok <| Pattern.ObjectDestructure [ ( "foo", Nothing ) ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


manyFieldsObjectDestructurePatternTest : Test
manyFieldsObjectDestructurePatternTest =
    let
        title =
            "Multiple fields can be matched in an object destructure pattern"

        input =
            "{ foo, bar, baz }"

        expected =
            Ok <|
                Pattern.ObjectDestructure
                    [ ( "foo", Nothing )
                    , ( "bar", Nothing )
                    , ( "baz", Nothing )
                    ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


capitalCaseFieldObjectDestructurePatternTest : Test
capitalCaseFieldObjectDestructurePatternTest =
    let
        title =
            "Fields in an object destructure pattern cannot begin with a capital letter."

        input =
            "{ Foo }"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.err
        )


nestedObjectDestructurePatternTest : Test
nestedObjectDestructurePatternTest =
    let
        title =
            "Other patterns can be nested in an object destructure pattern."

        input =
            "{ foo, bar: a, baz: [ _, b ] }"

        expected =
            Ok <|
                Pattern.ObjectDestructure
                    [ ( "foo", Nothing )
                    , ( "bar", Just <| Pattern.Name "a" )
                    , ( "baz"
                      , Just <|
                            Pattern.ArrayDestructure
                                [ Pattern.Wildcard Nothing
                                , Pattern.Name "b"
                                ]
                      )
                    ]
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )



-- VALUE PATTERNS --------------------------------------------------------------


booleanValuePatternTest : Test
booleanValuePatternTest =
    let
        title =
            "Boolean literals can be used as value pattern matches."

        input =
            "true"

        expected =
            Ok <| Pattern.Value <| Literal.Boolean True
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


numberValuePatternTest : Test
numberValuePatternTest =
    let
        title =
            "Number literals can be used as value pattern matches."

        input =
            "9001"

        expected =
            Ok <| Pattern.Value <| Literal.Number 9001
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


stringValuePatternTest : Test
stringValuePatternTest =
    let
        title =
            "String literals can be used as value pattern matches."

        input =
            "'foo'"

        expected =
            Ok <| Pattern.Value <| Literal.String "foo"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )



-- WILDCARD PATTERNS -----------------------------------------------------------


underscoreWildcardPatternTest : Test
underscoreWildcardPatternTest =
    let
        title =
            "Wildcard patterns are matched with an underscore."

        input =
            "_"

        expected =
            Ok <| Pattern.Wildcard Nothing
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )


underscoreWithNameWildcardPatternTest : Test
underscoreWithNameWildcardPatternTest =
    let
        title =
            "Wildcard patterns may have an identifier following the underscore."

        input =
            "_foo"

        expected =
            Ok <| Pattern.Wildcard <| Just "foo"
    in
    test title
        (\_ ->
            Parser.run Pattern.parser input
                |> Expect.equal expected
        )
