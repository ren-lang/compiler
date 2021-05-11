module Parse.Source.Expression.Identifier exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Expect
import Fuzz exposing (..)
import Parser
import Ren.Data.Expression.Identifier as Identifier
import Ren.Data.Expression.Operator as Operator
import Test exposing (..)



-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Identifier.Identifier"
        [ localIdentifierSuite
        , scopedIdentifierSuite
        , operatorIdentifierSuite
        , fieldIdentifierSuite
        ]


localIdentifierSuite : Test
localIdentifierSuite =
    describe "Identifier.Local"
        [ lowerCaseLocalIdentifierTest
        , lowerCaseWithDigitsLocalIdentifierTest
        , camelCaseLocalIdentifierTest
        , capitalCaseLocalIdentifierTest
        , numericLocalIdentifierTest
        ]


scopedIdentifierSuite : Test
scopedIdentifierSuite =
    describe "Identifier.Scoped"
        [ capitalCaseScopedIdentifierTest
        , capitalCaseWithDigitsScopedIdentifierTest
        , multipleScopedIdentifierTest
        , missingLocalScopedIdentifierTest
        , trailingScopedIdentifierTest
        ]


operatorIdentifierSuite : Test
operatorIdentifierSuite =
    describe "Identifier.Operator"
        [ wrappedOperatorIdentifierTest
        , unwrappedOperatorIdentifierTest
        , unknownOperatorIdentifierTest
        ]


fieldIdentifierSuite : Test
fieldIdentifierSuite =
    describe "Identifier.Field"
        [ lowerCaseFieldIdentifierTest
        , lowerCaseWithDigitsFieldIdentifierTest
        , camelCaseFieldIdentifierTest
        , camelCaseWithDigitsFieldIdentifierTest
        , capitalCaseFieldIdentifierTest
        , numericFieldIdentifierTest
        ]



-- LOCAL IDENTIFIERS -----------------------------------------------------------


lowerCaseLocalIdentifierTest : Test
lowerCaseLocalIdentifierTest =
    let
        title =
            "Local identifiers must begin with a lowercase letter."

        input =
            "foo"

        expecting =
            Ok <| Identifier.Local "foo"
    in
        test title
            (\_ ->
                Parser.run Identifier.parser input
                    |> Expect.equal expecting
            )


lowerCaseWithDigitsLocalIdentifierTest : Test
lowerCaseWithDigitsLocalIdentifierTest =
    let
        title =
            "Local identifiers may contain digits."

        input =
            "foo9001"

        expecting =
            Ok <| Identifier.Local "foo9001"
    in
        test title
            (\_ ->
                Parser.run Identifier.parser input
                    |> Expect.equal expecting
            )

camelCaseLocalIdentifierTest : Test
camelCaseLocalIdentifierTest =
    let
        title =
            "Local identifiers can contain capital letters."

        input =
            "fooBar"

        expecting =
            Ok <| Identifier.Local "fooBar"
    in
        test title
            (\_ ->
                Parser.run Identifier.parser input
                    |> Expect.equal expecting
            )

capitalCaseLocalIdentifierTest : Test
capitalCaseLocalIdentifierTest =
    let
        title =
            "Local identifiers cannot start with an uppercase letter."

        input =
            "Foo"
    in
        test title
            (\_ ->
                Parser.run Identifier.parser input
                    |> Expect.err
            )


numericLocalIdentifierTest : Test
numericLocalIdentifierTest =
    let
        title =
            "Local identifiers cannot start with a digit."

        input =
            "9001foo"
    in
        test title
            (\_ ->
                Parser.run Identifier.parser input
                    |> Expect.err
            )

-- SCOPED IDENTIFIERS ----------------------------------------------------------


capitalCaseScopedIdentifierTest : Test
capitalCaseScopedIdentifierTest =
    let
        title =
            "Scoped identifiers must be CapitalCase."

        input =
            "Foo.bar"

        expecting =
            Ok <| Identifier.Scoped [ "Foo" ] "bar"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


capitalCaseWithDigitsScopedIdentifierTest : Test
capitalCaseWithDigitsScopedIdentifierTest =
    let
        title =
            "Scoped identifiers may contain digits."

        input =
            "Foo9001.bar"

        expecting =
            Ok <| Identifier.Scoped [ "Foo9001" ] "bar"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


multipleScopedIdentifierTest : Test
multipleScopedIdentifierTest =
    let
        title =
            "Scoped identifiers can include >1 namespaces."

        input =
            "Foo.Bar.baz"

        expecting =
            Ok <| Identifier.Scoped [ "Foo", "Bar" ] "baz"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


missingLocalScopedIdentifierTest : Test
missingLocalScopedIdentifierTest =
    let
        title =
            "Scoped identifiers must end with a local identifier."

        input =
            "Foo.Bar"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )


trailingScopedIdentifierTest : Test
trailingScopedIdentifierTest =
    let
        title =
            "Trailing period is not allowed with scoped identifiers."

        input =
            "Foo.Bar."
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )



-- OPERATOR IDENTIFIERS --------------------------------------------------------


wrappedOperatorIdentifierTest : Test
wrappedOperatorIdentifierTest =
    let
        title =
            "Operators can be used as identifiers when wrapped in parentheses."

        input =
            "(+)"

        expecting =
            Ok <| Identifier.Operator Operator.Add
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


unwrappedOperatorIdentifierTest : Test
unwrappedOperatorIdentifierTest =
    let
        title =
            "Operators not wrapped in parentheses cannot be used as identifiers."

        input =
            "+"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )


unknownOperatorIdentifierTest : Test
unknownOperatorIdentifierTest =
    let
        title =
            "Unknown operators cannot be used as identifiers."

        input =
            "($)"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )



-- FIELD IDENTIFIERS -----------------------------------------------------------


lowerCaseFieldIdentifierTest : Test
lowerCaseFieldIdentifierTest =
    let
        title =
            "Field identifiers may be in all lowercase."

        input =
            ".foo"

        expecting =
            Ok <| Identifier.Field "foo"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


lowerCaseWithDigitsFieldIdentifierTest : Test
lowerCaseWithDigitsFieldIdentifierTest =
    let
        title =
            "LowerCase field identifiers may contain digits."

        input =
            ".foo9001"

        expecting =
            Ok <| Identifier.Field "foo9001"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


camelCaseFieldIdentifierTest : Test
camelCaseFieldIdentifierTest =
    let
        title =
            "Field identifiers may be in camelCase."

        input =
            ".fooBar"

        expecting =
            Ok <| Identifier.Field "fooBar"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


camelCaseWithDigitsFieldIdentifierTest : Test
camelCaseWithDigitsFieldIdentifierTest =
    let
        title =
            "CamelCasel field identifiers may contain digits."

        input =
            ".fooBar9001"

        expecting =
            Ok <| Identifier.Field "fooBar9001"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.equal expecting
        )


capitalCaseFieldIdentifierTest : Test
capitalCaseFieldIdentifierTest =
    let
        title =
            "Field identifiers cannot begin with a capital letter."

        input =
            ".Foo"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )


numericFieldIdentifierTest : Test
numericFieldIdentifierTest =
    let
        title =
            "Field identifiers cannot begin with a digit."

        input =
            ".9001"
    in
    test title
        (\_ ->
            Parser.run Identifier.parser input
                |> Expect.err
        )
