module Parse.Source.Expression.Accessor exposing (suite)

-- IMPORTS ---------------------------------------------------------------------

import Expect
import Fuzz exposing (..)
import Parser
import Ren.Data.Expression as Expression
import Ren.Data.Expression.Accessor as Accessor
import Test exposing (..)



-- SUITES ----------------------------------------------------------------------


suite : Test
suite =
    describe "Accessor.Accessor"
        [ fixedAccessorSuite
        , computedAccessorSuite
        ]


fixedAccessorSuite : Test
fixedAccessorSuite =
    describe "Accessor.Fixed"
        [ lowercaseFixedAccessorTest
        , lowercaseWithDigitsFixedAccessorTest
        , camelCaseFixedAccessorTest
        , camelCaseWithDigitsFixedAccessorTest
        , capitalCaseFixedAccessorTest
        , numericFixedAccessorTest
        ]


computedAccessorSuite : Test
computedAccessorSuite =
    describe "Accessor.Computed"
        [ literalComputedAccessorTest
        , applicationComputedAccessorTest
        , invalidExpressionComputedAccessorTest
        ]



-- FIXED ACCESSORS -------------------------------------------------------------


lowercaseFixedAccessorTest : Test
lowercaseFixedAccessorTest =
    let
        title =
            "Lowercase fixed accessors correctly parse."

        input =
            ".foo"

        expecting =
            Ok (Accessor.Fixed "foo")
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


lowercaseWithDigitsFixedAccessorTest : Test
lowercaseWithDigitsFixedAccessorTest =
    let
        title =
            "Lowercase fixed accessors with digits correctly parse."

        input =
            ".foo9001"

        expecting =
            Ok (Accessor.Fixed "foo9001")
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


camelCaseFixedAccessorTest : Test
camelCaseFixedAccessorTest =
    let
        title =
            "CamelCase fixed accessors correctly parse."

        input =
            ".fooBar"

        expecting =
            Ok (Accessor.Fixed "fooBar")
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


camelCaseWithDigitsFixedAccessorTest : Test
camelCaseWithDigitsFixedAccessorTest =
    let
        title =
            "CamelCase fixed accessors with digits correctly parse."

        input =
            ".fooBar9001"

        expecting =
            Ok (Accessor.Fixed "fooBar9001")
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


capitalCaseFixedAccessorTest : Test
capitalCaseFixedAccessorTest =
    let
        title =
            "CapitalCase fixed accessors fail to parse."

        input =
            ".Foo"
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.err
        )


numericFixedAccessorTest : Test
numericFixedAccessorTest =
    let
        title =
            "Numeric fixed accessors fail to parse."

        input =
            ".9001"
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.err
        )



-- COMPUTED ACCESSORS ----------------------------------------------------------


literalComputedAccessorTest : Test
literalComputedAccessorTest =
    let
        title =
            "Literals can be used as computed accessors."

        input =
            "[9001]"

        expecting =
            Ok (Accessor.Computed <| Expression.number 9001)
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


applicationComputedAccessorTest : Test
applicationComputedAccessorTest =
    let
        title =
            "Function application can be used as a computed accessor."

        input =
            "[ foo bar ]"

        expecting =
            Ok
                (Accessor.Computed <|
                    Expression.Application (Expression.local "foo") [ Expression.local "bar" ]
                )
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.equal expecting
        )


invalidExpressionComputedAccessorTest : Test
invalidExpressionComputedAccessorTest =
    let
        title =
            "Invalid expressions cannot be used as computed accessors."

        input =
            "[ 1 + ]"
    in
    test title
        (\_ ->
            Parser.run (Accessor.parser Expression.parser) input
                |> Expect.err
        )
